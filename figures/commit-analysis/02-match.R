# load packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(glue)

# load data --------------------------------------------------------------------

gradebook    <- read_csv(here::here("figures/commit-analysis/", "data/sta199-gradebook.csv"))
commits      <- read_csv(here::here("figures/commit-analysis/", "data/all-commits.csv"))
match_name   <- read_csv(here::here("figures/commit-analysis/", "data/match-name.csv"))

# filter out course admin commits ----------------------------------------------

admin_emails <- c("mine@stat.duke.edu",
                  "rundel.mine@gmail.com",
                  "noreply@github.com",
                  "peter.hase@duke.edu",
                  "rundel@gmail.com",
                  "ss656@duke.edu",
                  "sarah.sibley@duke.edu",
                  "walkerharrison1000@gmail.com",
                  "larson.gary@gmail.com",
                  "walker.harrison@duke.edu"
                  )

commits <- commits %>% filter(!(email %in% admin_emails))

# bring in student names -------------------------------------------------------

commits <- commits %>%
  mutate(
    name = str_remove(name, '\\“'),
    name = str_remove(name, '\\”'),
    ) %>% # to make it possible to match
  left_join(match_name, by = c("name" = "commit_name"))

expect_equal(which(is.na(commits$student_name)), integer(0))

# filter out those not in gradebook (unknown or dropped) -----------------------

commits <- commits %>%
  filter(student_name != "UNKNOWN") %>%
  filter(student_name %in% gradebook$student_name)

expect_equal(which(!(commits$student_name %in% gradebook$student_name)), integer(0))

# separate into dates and times, cast as date type -----------------------------

commits <- commits %>%
  separate(date, into = c("commit_date", "commit_time"), sep = " ", remove = FALSE)

# set MT dates -----------------------------------------------------------------
# reference: https://www2.stat.duke.edu/courses/Spring18/Sta199/syllabus/

mt1_date <- as.Date("2018-02-16")
mt2_date <- as.Date("2018-04-06")

# mark commits pahses in relation to MT dates ----------------------------------

commits <- commits %>%
  mutate(phase = case_when(
    commit_date < mt1_date                           ~ "before_mt1",
    commit_date >= mt1_date & commit_date < mt2_date ~ "between_mt1_mt2",
    commit_date >= mt2_date                          ~ "after_mt2"
  ))

# count commits by phase -------------------------------------------------------

commit_counts <- commits %>%
  group_by(student_name, phase) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  complete(student_name, phase, fill = list(n = 0)) %>%
  pivot_wider(names_from = phase, values_from = n, names_prefix = "commits_")

expect_equal(nrow(commit_counts), nrow(gradebook))

# match to gradebook -----------------------------------------------------------

matched_data <- left_join(gradebook, commit_counts) %>%
  mutate(
    commits_before_mt2 = commits_before_mt1 + commits_between_mt1_mt2,
    commits_all = commits_before_mt1 + commits_between_mt1_mt2 + commits_after_mt2,
    id = 1:n()
  ) %>%
  select(-student_id, -student_name, -team)

# save data --------------------------------------------------------------------

# anonymize and prune
write_csv(matched_data %>% select(id,  overall,
                                       commits_before_mt1, commits_between_mt1_mt2, commits_after_mt2, commits_before_mt2, commits_all),
          path = here::here("figures/commit-analysis/", "data/roster.csv"))

# non-anonymize
write_csv(matched_data,
          path = here::here("figures/commit-analysis/", "data/roster-complete.csv"))

