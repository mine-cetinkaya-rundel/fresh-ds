# load pakcages ----------------------------------------------------------------

#install_github("rundel/ghclass")
library(ghclass)
library(tidyverse)
library(here)

# get repos --------------------------------------------------------------------

repos <- org_repos("Sta199-S18",
                   filter = "website|community|keys|tutorials|demo|web-scraping", exclude = TRUE)

# get commits ------------------------------------------------------------------

all_commits <- repo_commits(repos)

# save commits -----------------------------------------------------------------

write_csv(all_commits,
          path = here::here("figures/commit-analysis/", "data/all-commits.csv"))
