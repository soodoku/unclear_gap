# Load libs
library(tidyverse)
library(readr)

# Load dat
uncert <- read_csv("data/lucid/Uncertainty+Effect+replication_January+28,+2023_20.38.zip")

# Filter attn_check
uncert$attn_checkc <- uncert$attn_check == "Extremely interested,Very interested"

# Filter those who consent and not preview and pass the attn check
fin_dat <- uncert[uncert$consent == 'Yes' & uncert$DistributionChannel != "preview" & uncert$attn_checkc, ]

# PID 
fin_dat$dem <- fin_dat$political_party %in% c("1", "2", "3", "6")
fin_dat$rep <- fin_dat$political_party %in% c("5", "8", "9", "10")
fin_dat$dem[fin_dat$rep == FALSE & fin_dat$dem == FALSE] <- NA
fin_dat$rep[fin_dat$rep == FALSE & fin_dat$dem == FALSE] <- NA

fin_dat <- fin_dat %>%
  mutate(pid3 = case_when(
   political_party %in% c("1", "2", "3", "6") ~ "dem",
   political_party %in% c("5", "8", "9", "10") ~ "rep",
   political_party %in% c("4", "7") ~ "ind"
  ))

# Unemployment interpretation

fin_dat <- fin_dat %>%
  mutate(unemp_got_better = case_when(
    econ_cond == "reps" ~ unemp_reps == "Got Better",
    econ_cond == "dems" ~ unemploy_dems == "Got better" # notice the case
  ))

fin_dat <- fin_dat %>%
  mutate(infl_got_better = case_when(
    econ_cond == "reps" ~ inflation_reps == "Got Better",
    econ_cond == "dems" ~ inflation_dems == "Got better" # notice the case
  ))

### Congeniality
fin_dat <- fin_dat %>%
  mutate(econ_cong = case_when(
    pid3 == "dem" & econ_cond == "dems" ~ 1,
    pid3 == "rep" & econ_cond == "rep" ~ 1,
    TRUE ~ 0
  ))

### Analysis

fin_dat %>% 
  filter(pid3 != "ind") %>%
  group_by(econ_cong) %>%
  summarize(unemp_better = mean(unemp_got_better), infl_better = mean(infl_got_better))


summary(lm(unemp_got_better ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ]))
summary(lm(infl_got_better ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ]))


