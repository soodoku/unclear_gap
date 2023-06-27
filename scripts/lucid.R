# Load libs
library(tidyverse)
library(readr)
library(knitr)
library(stargazer)
library(lme4)

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

# Let's convert to numbers

items <- c("unemp_reps", "unemploy_dems", "inflation_reps", "inflation_dems")
fin_dat[, items] <- lapply(fin_dat[, items], function(x) tolower(x))

recode_to_numbers <- function(x) {
  recode(x, "got better" = 1, "stayed about the same" = .5, "got worse" = 0)
}

fin_dat <- fin_dat %>%
  mutate(
    unemp_reps_n = recode_to_numbers(unemp_reps),
    unemploy_dems_n = recode_to_numbers(unemploy_dems),
    inflation_reps_n = recode_to_numbers(inflation_reps),
    inflation_dems_n = recode_to_numbers(inflation_dems)
  )

# Unemployment interpretation

fin_dat <- fin_dat %>%
  mutate(unemp_got_better = case_when(
    econ_cond == "reps" ~ unemp_reps == "got getter",
    econ_cond == "dems" ~ unemploy_dems == "got better"
  ),
  
  infl_got_better = case_when(
    econ_cond == "reps" ~ inflation_reps == "got better",
    econ_cond == "dems" ~ inflation_dems == "got better" 
  ))

fin_dat <- fin_dat %>%
  mutate(unemp_n = case_when(
    econ_cond == "reps" ~ unemp_reps_n,
    econ_cond == "dems" ~ unemploy_dems_n
  ),
  
  infl_n = case_when(
    econ_cond == "reps" ~ inflation_reps_n,
    econ_cond == "dems" ~ inflation_dems_n
  ))


### Congeniality
fin_dat <- fin_dat %>%
  mutate(econ_uncong = case_when(
    pid3 == "dem" & econ_cond == "reps" ~ 1,
    pid3 == "rep" & econ_cond == "dems" ~ 1,
    TRUE ~ 0
  ))

### Analysis

lucid_out <- fin_dat %>% 
  filter(pid3 != "ind") %>%
  group_by(econ_cong) %>%
  summarize(unemp_better = mean(unemp_got_better), 
            se_uemp = sqrt(unemp_better*(1- unemp_better)/n()),
            infl_better = mean(infl_got_better),
            se_infl = sqrt(infl_better*(1- infl_better)/n()))

kable(lucid_out, digits = 3)

summary(lm(unemp_got_better ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ]))
summary(lm(infl_got_better ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ]))

# Commensurate to Turk
unemp_mod <- lm(unemp_n ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ])
infl_mod  <- lm(infl_n ~ econ_cong, data = fin_dat[fin_dat$pid3 != "ind", ])

# Let's do a joint
long_data <- fin_dat %>%
  filter(fin_dat$pid3 != "ind") %>%
  select(unemp_n, infl_n, econ_cong, rid) %>%
  pivot_longer(cols = - c(rid, econ_cong),
               names_to = "variable",
               values_to = "measure")

agg_mod <- with(long_data, lmer(measure ~ econ_cong + (1|rid)))

library(stats)

stargazer(unemp_mod, infl_mod, #agg_mod, 
          dep.var.labels = c("Unemployment", "Inflation"), # "Aggregate"),
          covariate.labels = "Out-party cue",
          align = TRUE, 
          type = "latex",
          omit.stat=c("LL","ser","f", "adj.rsq", "aic", "bic"),
          label = "lucid",
          out = "tabs/table_2_lucid.tex",
          title = "Impact of Treatment on Economic Evaluations (Lucid)",
          notes = c("Standard errors in parentheses.",
                    "All variables have been rescaled 0-1 for ease of interpretation."),
          notes.align = "l",
          header = FALSE)

