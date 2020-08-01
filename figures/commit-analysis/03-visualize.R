# load packages ----------------------------------------------------------------

library(tidyverse)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(here)

# load data --------------------------------------------------------------------

roster <- read_csv(here::here("figures/commit-analysis", "data/roster.csv"))

# plot relationships with scatterplots -----------------------------------------

all_scat <- ggplot(roster, aes(x = commits_all, y = overall)) +
  geom_point(alpha = 0.7, position = "jitter") +
  geom_smooth(span = 0.95, color = "darkgray") +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "All commits"
  ) +
  theme_minimal()

mt1_scat <- ggplot(roster, aes(x = commits_before_mt1, y = overall)) +
  geom_point(alpha = 0.7, position = "jitter") +
  geom_smooth(span = 0.95, color = "darkgray") +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "Pre-Midterm 1 commits"
  ) +
  theme_minimal()

mt2_scat <- ggplot(roster, aes(x = commits_before_mt2, y = overall)) +
  geom_point(alpha = 0.7, position = "jitter") +
  geom_smooth(span = 0.95, color = "darkgray") +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "Pre-Midterm 2 commits"
  ) +
  theme_minimal()

all_scat + mt1_scat + mt2_scat + plot_annotation("Course grade vs. number of commits")
ggsave(filename = here::here("figures/commit-analysis/", "commit_scatter.jpeg"), height = 3, width = 8, units = "in")

# bin commits ------------------------------------------------------------------

bin <- function(x, probs){
  cut(x,
      breaks = c(0, as.numeric(quantile(x, probs = probs))),
      labels = c("Low", "Medium", "High")
  )
}

roster <- roster %>%
  mutate_at(
    vars(starts_with("commits_")), list(binned = ~bin(., probs = c(0.25, 0.7, 1)))
  )

# plot relationships with boxplots ---------------------------------------------

all_box <- ggplot(roster, aes(x = commits_all_binned, y = overall)) +
  geom_boxplot() +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "All commits"
  ) +
  theme_minimal()

mt1_box <- ggplot(roster, aes(x = commits_before_mt1_binned, y = overall)) +
  geom_boxplot() +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "Pre-Midterm 1 commits"
  ) +
  theme_minimal()

mt2_box <- ggplot(roster, aes(x = commits_before_mt2_binned, y = overall)) +
  geom_boxplot() +
  labs(
    x = "Number of commits",
    y = "Course grade",
    title = "Pre-Midterm 2 commits"
  ) +
  theme_minimal()

all_box + mt1_box + mt2_box + plot_annotation("Course grade vs. number of commits")
ggsave(filename = here::here("figures/commit-analysis/", "commit_box.jpeg"), height = 5, width = 14, units = "in")
