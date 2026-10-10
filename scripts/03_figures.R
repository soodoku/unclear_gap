library(ggplot2)
d <- read.csv(file.path(table_dir, "effects.csv"))
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
  geom_vline(xintercept = 0, color = colors[["reference"]], linetype = 2) +
  geom_errorbar(aes(xmin = low, xmax = high),
    orientation = "y", width = .12,
    position = position_dodge(width = .45)
  ) +
  geom_point(size = 2.5, position = position_dodge(width = .45)) +
  facet_wrap(~study, nrow = 1) +
  scale_color_manual(values = colors[c("Full", "Screen passed")]) +
  labs(
    x = "Opposing-party minus own-party cue (0-100 evaluation points)", color = NULL,
    shape = NULL
  ) +
  theme_paper()
save_figure(p, "effects")
