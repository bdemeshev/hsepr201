#' Get students data from google sheets
#'
#' Get students data from google sheets
#'
#' Get students data from google sheets
#' @param path filename of csv with student data
#' @return tibble with useful id data
#' @export
get_students_data = function(path) {
  student = rio::import(path)
  student = dplyr::select(student, id_for_online, last_name, first_name, middle_name,
                          group, comment)
  return(student)
}


#' Get list of git repositories and commits
#'
#' Get list of git repositories and commits
#'
#' Get list of git repositories and commits
#' @param path folder name
#' @return tibble with useful id data
#' @export
get_gh_repos = function(path = ".") {
  commits = tibble::tibble(folder = list.dirs(path, recursive = FALSE))
  commits = dplyr::mutate(commits, user = stringr::str_match(folder, "/([A-Za-z0-9_-]*)$")[, 2])
  commits = dplyr::mutate(commits, gh_commits = purrr::map(folder, ~get_gh_commits(.)))
  commits = tidyr::unnest(commits, gh_commits)
  return(commits)
}


#' Get list of git repository commits
#'
#' Get list of git repository commits
#'
#' Get list of git repository commits
#' @param path folder name
#' @return tibble with useful id data
#' @export
get_gh_commits = function(path = ".") {
  log_format = "%cd\t%cn\t%ce\t%s\t%an\t%h\t%p"
  # https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History
  log_options = glue:: glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S %z"')

  log_cmd = glue::glue('git -C {path} log {log_options}')

  history_logs = system(log_cmd, intern = TRUE) %>%
    stringr::str_split_fixed("\t", 7) %>%
    dplyr::as_tibble(.name_repair = "minimal") %>%
    stats::setNames(c("date", "committer", "mail", "message", "username", "hash", "parent_hash"))
  return(history_logs)
}

#' Parse lms filenames
#'
#' Parse lms filenames
#'
#' Parse lms filenames
#' @param path folder name
#' @return tibble with useful id data
#' @export
parse_lms_filenames = function(path = ".") {
  filenames = list.files(path)
  uploads = tibble::as_tibble(
    stringr::str_match(filenames, "^([A-Za-z-]*)_([A-Za-z-]*)_([0-9A-Za-z-]*)_([0-9-]*)"),
      .name_repair = "unique")
  colnames(uploads) = c("filename", "last_name", "initials", "title", "timedate")
  uploads = dplyr::mutate(uploads, extension = tools::file_ext(filenames))
  uploads = dplyr::mutate(uploads, filename = filenames)
  uploads = dplyr::mutate(uploads, timedate = lubridate::ymd_hms(timedate))
  return(uploads)
}

#' Separate grain from chaffs
#'
#' The good, the bad and the ugly :)
#'
#' Just for information: check whether the submission was unique and with pdf file.
#' @param uploads tibble with uploads data
#' @return tibble with good and bad guys
#' @export
separate_lms_grain_chaff = function(uploads) {
  grain_chaff = dplyr::group_by(uploads, last_name, initials) %>%
    dplyr::summarize(n_uploads = dplyr::n(),
              last_time = max(timedate), first_time = min(timedate),
              extension = paste0(extension, collapse = ",")) %>%
    dplyr::mutate(good = (n_uploads == 1) &
             (stringr::str_to_lower(extension) == "pdf"))
  return(grain_chaff)
}

#' Force unique date
#'
#' Force unique date for a vector of timedates
#'
#' Force unique date for a vector of timedates.
#' @param timedate vector with probably different dates
#' @return timedate vector with unique date forced
#' @export
force_unique_date = function(timedate) {
  dates = unique(lubridate::date(timedate))
  unique_date = dates[1]
  if (length(dates) > 1) {
    warning("Какая-то ересь! Загруженные файлы относятся к разным дням\n",
            paste0(dates, collapse = " "), "\n",
            "Была принудительно форсирована дата ", unique_date)
    lubridate::date(timedate) = unique_date
  }
  return(timedate)
}

#' Get hse lms unique submission
#'
#' Get hse lms unique submission
#'
#' Get hse lms unique submission
#' @param uploads tibble with uploads data
#' @param which either first or last submission is counted
#' @param start submissions before start are ignored and non counted as first submission
#' @return tibble with unique submissions
#' @export
get_lms_unique_submission = function(uploads,
                          which = c("first", "last"),
                          start = "13:00:00") {
  which = match.arg(which)

  uploads = dplyr::mutate(uploads, timedate = force_unique_date(timedate))
  unique_date = lubridate::date(uploads$timedate[1])

  start = lubridate::ymd_hms(paste0(unique_date, " ", start))

  early_birds = dplyr::filter(uploads, timedate < start)
  if (nrow(early_birds) > 0) {
    warning("Какой-то ахтунг! Есть загруженные файлы раньше времени начала! Игнорируем их.")
    print(early_birds)
  }

  uploads = dplyr::filter(uploads, timedate >= start)

  if (which == "first") {
    unique_submission = dplyr::group_by(uploads, last_name, initials) %>% dplyr::filter(timedate == min(timedate))
  } else {
    unique_submission = dplyr::group_by(uploads, last_name, initials) %>% dplyr::filter(timedate == max(timedate))
  }

  return(unique_submission)
}

#' Calculate penalties
#'
#' Calculate penalties
#'
#' Calculate penalties
#' @param timedate vector of timedates
#' @param deadline vector of deadlines
#' @param penalty vector of penalties (same length as deadline)
#' @return penalties vector
#' @export
calculate_penalty = function(timedate,
                  deadline = c("13:40:00", "13:45:00", "13:50:00"),
                  penalty = c(0.3, 0.6, 1)) {
  submitted = force_unique_date(timedate)


  dead_pen = tibble::tibble(deadline = c(deadline, "00:00:00"),
                            penalty = c(penalty, 0))
  cross_times = tidyr::crossing(submitted, dead_pen)

  unique_date = lubridate::date(submitted[1])
  cross_times = dplyr::mutate(cross_times,
                deadline = lubridate::ymd_hms(paste0(unique_date, " ", deadline)))
  cross_times = dplyr::filter(cross_times, submitted > deadline)
  cross_times = cross_times %>% dplyr::group_by(submitted) %>%
    dplyr::filter(deadline == max(deadline))
  return(cross_times$penalty)
}


#' Transliterate cyrillic text
#'
#' Transliterate cyrillic text
#'
#' Transliterate cyrillic text
#' @param x cyrillic text
#' @return transliterated text
#' @export
translit = function(x) {
  return(stringi::stri_trans_general(x, id = "russian-latin/bgn"))
}

#' Replace yo by ye
#'
#' Replace yo by ye
#'
#' Replace yo by ye
#' @param x cyrillic text
#' @return cyrillic text
#' @export
yo2e = function(x) {
  x = stringr::str_replace_all(x, "Ё", "E")
  x = stringr::str_replace_all(x, "ё", "е")
  return(x)
}

#' Get lms prefix
#'
#' Get lms prefix
#'
#' Get lms prefix
#' @param last_name last name in cyrillic
#' @param first_name first name in cyrillic
#' @return filename prefix in english
#' @export
get_lms_prefix = function(last_name, first_name) {
  last_name = last_name %>% stringr::str_to_lower() %>% yo2e() %>% translit()
  first_name = stringr::str_sub(first_name, end = 1) %>%
    stringr::str_to_lower() %>% yo2e() %>% translit()

  prefix = paste0(last_name, "_", first_name)
  # simplification in lms
  prefix = stringr::str_replace_all(prefix, "ʹ", "")
  prefix = stringr::str_replace_all(prefix, "kh", "h")
  prefix = stringr::str_replace_all(prefix, "ts", "c")
  prefix = stringr::str_replace_all(prefix, "·", "")
  prefix = stringr::str_replace_all(prefix, "ye", "e")
  prefix = stringr::str_replace_all(prefix, "zh", "j")
  return(prefix)
}


#' List files in a folder
#'
#' List files in a folder
#'
#' List files in a folder
#' @param path folder name
#' @param remove file names to exclude
#' @return character vector with a list of files separated by comma
#' @export
#' @example
#' list_files()
list_files = function(path = ".", remove = c("LICENSE", "README.md")) {
  files = list.files(path)
  files = files[!(files %in% remove)]
  files = paste0(files, collapse = ", ")
  return(files)
}


#' Recognise student id using folder content
#'
#' Recognise student id using folder content
#'
#' Recognise student id using folder content.
#' The function searches for the files like krx_xxx.pdf or something close to this.
#' @param path folder name
#' @return student integer id or NA
#' @export
get_stud_id_from_repository = function(path = ".") {
  files = tibble::tibble(filename = list.files(path))
  files = dplyr::filter(files, !(filename %in% c("LICENSE", "README.md")))
  files = dplyr::mutate(files, filename = stringr::str_to_lower(filename))

  files = dplyr::mutate(files, id_str = stringr::str_match(filename, "kr[-_ ]*[0-9][- _]([0-9]*).pdf")[, 2])
  files = dplyr::mutate(files, id_candidate = as.integer(id_str))

  candidates = stats::na.omit(unique(files$id_candidate))

  if (length(candidates) == 1) {
    return(candidates)
  } else {
    warning("Multiple or no candidates!!!!\n Folder: ", path, "\n Files: ", files$filename)
    return(NA_integer_)
  }
}



#' Create target file/folder name from kr, id, time
#'
#' Create target file/folder name from kr, id, time
#'
#' Create target file/folder name from kr, id, time
#' @param kr integer kr number
#' @param time datetime of the submission
#' @param id student id
#' @return target file/folder name without extension
#' @export
create_target_fname = function(kr, time, id) {
  hms = hms::as_hms(time)
  hms = stringr::str_replace_all(hms, ":", "-")
  fname = glue::glue("gh_kr{kr}_id{id}_{hms}")
  return(fname)
}


#' Copy one github entry: file or folder
#'
#' Copy one github entry: file or folder
#'
#' Copy one github entry: file or folder
#' @param from repository folder
#' @param to student target folder
#' @param name name for target file/folder
#' @param remove file names to exclude
#' @return nothing, just copy
#' @export
copy_one_gh_entry = function(from, to, name,
                             remove = c("LICENSE", "README.md")) {
  files = list.files(from)
  files = files[!(files %in% remove)]
  if (length(files) == 0) {
    warning("Nothing to copy!\n Folder: ", from, "\n")
  }
  if (length(files) == 1) {
    # copy file
    from = paste0(from, "/", files)
    to = paste0(to, "/", name, ".", tools::file_ext(from))
    file.copy(from, to)
  }
  if (length(files) > 1) {
    # copy many files
    to = paste0(to, "/", name, "/")
    dir.create(to)
    for (file in files) {
      file_from = paste0(from, "/", file)
      file_to = paste0(to, "/", file)
      file.copy(file_from, file_to)
    }
  }
}

#' Create folder
#'
#' Create folder
#'
#' Create folder. No warning if the folder already exists.
#' @param path folder
#' @return nothing, just create folder
#' @export
dir_create = function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

