
# hsepr201

<!-- badges: start -->
<!-- badges: end -->

The goal of hsepr201 is to...

> — Всем — два!
>
> — И мне?!
>
> — И тебе тоже!
>
>      Серёжа Ламзин

## Installation

You can install the dev version of hsepr201 with:

``` r
devtools::install_github("bdemeshev/hsepr201")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

devtools::install_github("bdemeshev/hsepr201")

library(tidyverse)
library(rio)
library(hsepr201)
library(lubridate)

# kr3
# set path to github folder
path = "~/Downloads/stat_minikr_03-04-23-2020-01-57-53/"
gh_commits = get_gh_repos(path)
glimpse(gh_commits)
gh_commits = filter(gh_commits, username != "Boris Demeshev")

# check manually commits before 13:00
start = lubridate::parse_date_time("2020-04-23 13:00:00 +0300",
                  "Ymd HMS z", tz = "Europe/Moscow")
early_bird = filter(gh_commits, date <= start)
early_bird
### check manually early_bird!

gh_commits = filter(gh_commits, date > start)


# filter first commit:
glimpse(gh_commits)
gh_first_commit = group_by(gh_commits, user) %>%
  filter(date == min(date)) %>% ungroup()

gh_first_commit$user

# set repositories to first commit!
for (row_no in 1:nrow(gh_first_commit)) {
  setwd(gh_first_commit$folder[row_no])
  hash = gh_first_commit$hash[row_no]
  checkout_cmd = glue::glue('git checkout {hash}')
  system(checkout_cmd, intern = TRUE) 
}

gh_first_commit = mutate(gh_first_commit, 
                         stud_id = map_int(folder, ~get_stud_id_from_repository(.)))
gh_first_commit = mutate(gh_first_commit, 
                         files = map_chr(folder, ~list_files(.)))

gh_first_commit$files

# errors:
# same id for many users or no id
double_id = group_by(gh_first_commit, stud_id) %>% summarise(n = n()) %>%
  filter(n > 1) %>% pull(stud_id) %>% na.omit()
double_id
unrecognized = filter(gh_first_commit, stud_id %in% c(double_id, NA))
unrecognized

out_folder = "~/Downloads/minikr_03_output_gh/"
dir_create(out_folder)

unrecognized_folder = paste0(out_folder, "gh_copy_of_ambigous_identity/")
unrecognized_folder
dir_create(unrecognized_folder)

# save unrecognized people
for (folder in unrecognized$folder) {
  file.copy(from = folder, to = unrecognized_folder, recursive = TRUE)
}

unrecognized$user

gh_2_merge = rename(gh_first_commit, id = stud_id)

# save two lost souls! manually
stop("!!!!!!!!!!!! здесь мы руками прописываем id")

gh_2_merge = mutate(gh_2_merge, id = case_when(user == "artemkarapetyanbec186" ~ as.integer(300),
                                               user == "shyngyskhanturlubay" ~ as.integer(191),
                                               TRUE ~ id))
glimpse(gh_2_merge)




stud_gsh = rio::import("~/Downloads/economics_probability_2019_20 - all-2020-04-26.csv")
glimpse(stud_gsh)
stud = select(stud_gsh, stud_id:nolms)
stud = rename(stud, id = id_for_online)
nrow(stud)
stud = mutate(stud,
              group_folder = paste0(out_folder, group),
              ind_folder = paste0(group_folder, "/", last_name, " ", first_name)
)

same_folder_studs = group_by(stud, ind_folder) %>% summarise(n = n()) %>% filter(n > 1)
if (nrow(same_folder_studs) > 0) {
  stop("Students with the same target folder name detected!!!")
}




gh_first_commit_joined = left_join(gh_2_merge, stud, by = "id")


gh_joined = mutate(gh_first_commit_joined, hour = hour(date),
                   min = minute(date),
                   sec = second(date),
                   target = create_target_fname(kr = 3, date, id))

gh_joined = mutate(gh_joined,
              group_folder = paste0(out_folder, group),
              ind_folder = paste0(group_folder, "/", last_name, " ", first_name))



for (row_no in 1:nrow(gh_joined)) {
  name = create_target_fname(kr = 3, id = gh_joined$id[row_no],
                             time = gh_joined$date[row_no])
  from = gh_joined$folder[row_no]
  to = gh_joined$ind_folder[row_no]
  # cat("From: ", from, "\n To: ", to, "\n Name: ", name, "\n")
  dir_create(gh_joined$group_folder[row_no])
  dir_create(gh_joined$ind_folder[row_no])

  copy_one_gh_entry(from, to, name)
}

rio::export(gh_joined, paste0(out_folder, "/", "gh_report.xlsx"))


#################################################

out_folder = "~/Downloads/minikr_03_output_lms/"

dir_create(out_folder)
stud = mutate(stud,
              group_folder = paste0(out_folder, group),
              ind_folder = paste0(group_folder, "/", last_name, " ", first_name))




stud = mutate(stud, prelim_prefix = get_lms_prefix(last_name, first_name))

# студенты с риском общего lms префикса:
doubles = group_by(stud, prelim_prefix) %>% summarise(n = n()) %>%
  filter(n > 1) %>% pull(prelim_prefix)
doubles

risky_students = filter(stud, prelim_prefix %in% doubles)
risky_students



stud = mutate(stud, prefix = 
              ifelse(prelim_prefix %in% doubles, NA, prelim_prefix))
non_risky_students = filter(stud, !is.na(prefix))



###############################


path = "~/Downloads/mini-kontrolnaya-3_20200423-142136/"
lms_files = parse_lms_filenames(path)
glimpse(lms_files)
lms_files = mutate(lms_files, prefix = paste0(last_name, "_", initials))


risky_path = paste0(out_folder, "/copy_of_ambigous_identity/")
risky_path
dir_create(risky_path)

# спасаем тех, у кого есть человек на ту же фамилию и ту же букву имени
lms_risky_files = filter(lms_files, prefix %in% doubles)
lms_risky_files

for (filename in lms_risky_files$filename) {
  from = paste0(path, filename)
  to = paste0(risky_path, filename)
  file.copy(from = from, to = to)
}

# спасаем того, у кого не нашлось соответствия
lms_unmatched_files = filter(lms_files, !(prefix %in% stud$prelim_prefix))
lms_unmatched_files

for (filename in lms_unmatched_files$filename) {
  from = paste0(path, filename)
  to = paste0(risky_path, filename)
  file.copy(from = from, to = to)
}

lms_unjoined_files = bind_rows(lms_risky_files, lms_unmatched_files)
lms_unjoined_files = lms_unjoined_files %>% select(-last_name, -initials, -title)
lms_unjoined_files
risky_students

stop("!!!!!!!!!!!! здесь мы руками прописываем id тем у кого неоднозначно трактуется префикс файла")

lms_unjoined_files = mutate(lms_unjoined_files,
                            id = case_when(prefix == "afanasev_a" ~ 348,
                                           prefix == "andreev_i" ~ 167,
                                           prefix == "kuzmin_a" ~ 382))
lms_risky_joined = left_join(risky_students, lms_unjoined_files, by = "id")

# остальных mergim и растаскиваем по папкам
glimpse(lms_files)
lms_to_join = lms_files %>% select(-last_name, -initials, -title)

lms_non_risky_joined = left_join(non_risky_students, lms_to_join, by = "prefix")

lms_all_joined = bind_rows(lms_non_risky_joined, lms_risky_joined)

check = all.equal(sort(unique(lms_all_joined$id)), sort(unique(stud$id)))
if (!check) {
  stop("!!!!!!!! not all id present in lms_all_joined !!!!!!")
}

for (i in 1:nrow(lms_all_joined)) {
  row = lms_all_joined[i, ]
  if (!is.na(row$filename)) {
    dir_create(row$group_folder)
    dir_create(row$ind_folder)
    from = paste0(path, "/", row$filename)
    to = paste0(row$ind_folder, "/", row$filename)
    cat("From: ", from, "\n To: ", to, "\n\n")
    file.copy(from = from, to = to)
  }
}

lms_all_joined = mutate(lms_all_joined, hour = hour(timedate), 
                        min = minute(timedate),
                        sec = second(timedate))

export(lms_all_joined, paste0(out_folder, "lms_full_report.xlsx"))

# filter first attempt
lms_first_attempt = lms_all_joined %>% group_by(id) %>%
  filter(is.na(filename) | timedate == min(timedate)) 
  
export(lms_first_attempt, paste0(out_folder, "lms_first_attempt.xlsx"))




```

