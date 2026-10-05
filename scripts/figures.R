source("R/style.R")
library(ggplot2)
dir.create("figs", showWarnings = FALSE)
d <- read.csv("tabs/effects.csv")
d <- subset(d, party == "Pooled" & sample %in% c(
  "Full",
  "Screen passed"
) & outcome %in% c("unemployment", "inflation"))
d$outcome <- factor(d$outcome,
  levels = c("inflation", "unemployment"),
  labels = c("Inflation", "Unemployment")
)
d$study <- factor(d$study, levels = c("MTurk", "Lucid"))
d$sample <- factor(d$sample, levels = c("Full", "Screen passed"))
p <- ggplot(d, aes(estimate, outcome, color = sample, shape = sample)) +
  geom_vline(xintercept = 0, color = "grey65", linetype = 2) +
  geom_errorbar(aes(xmin = low, xmax = high),
    orientation = "y", width = .12,
    position = position_dodge(width = .45)
  ) +
  geom_point(size = 2.5, position = position_dodge(width = .45)) +
  facet_wrap(~study, nrow = 1) +
  scale_color_manual(values = c("#174A65", "#B05A26")) +
  labs(
    x = "Opposing-party minus own-party cue (0-100 evaluation points)", color = NULL,
    shape = NULL
  ) +
  paper_theme()
save_figure(p, "effects", height = 3.3)
