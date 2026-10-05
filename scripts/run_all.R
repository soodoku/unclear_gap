source("R/data.R")
source("R/analysis.R")
dir.create("tabs", showWarnings = FALSE)
save_table <- function(x, name) {
  write.csv(x, file.path("tabs", paste0(name, ".csv")),
    row.names = FALSE, na = ""
  )
}
results <- distributions <- flow <- cells <- list()
diagnostics <- influence <- clusters <- balance <- retention <- list()
for (study in c("MTurk", "Lucid")) {
  raw <- read_data(study)
  d <- raw[raw$eligible & raw$partisan, ]
  flow[[study]] <- data.frame(
    study = study,
    stage = c(
      "Export records", "Consenting nonpreview assigned", "Nonpartisans", "Partisans",
      "Screen passed partisans", "Screen flagged partisans", "Strict identifiers",
      "Missing outcomes"
    ),
    n = c(
      nrow(raw), sum(raw$eligible), sum(raw$eligible & !raw$partisan), nrow(d),
      sum(d$screen_pass), sum(!d$screen_pass), sum(d$strict),
      sum(!complete.cases(d[c("unemployment", "inflation")]))
    )
  )
  for (sample in c("Full", "Screen passed", "Screen flagged", "Strict identifiers")) {
    keep <- switch(sample,
      Full = rep(TRUE, nrow(d)),
      `Screen passed` = d$screen_pass,
      `Screen flagged` = !d$screen_pass,
      `Strict identifiers` = d$strict
    )
    z <- d[keep, ]
    for (party in c("Pooled", "Democrat", "Republican")) {
      x <- if (party == "Pooled") z else z[z$party == party, ]
      for (outcome in c(
        "unemployment", "inflation", "average", "unemployment_better",
        "inflation_better", "unemployment_same", "inflation_same",
        "unemployment_worse", "inflation_worse"
      )) {
        results[[length(results) + 1]] <- cbind(
          study = study, sample = sample,
          party = party, outcome = outcome, contrast(x, outcome)
        )
      }
    }
    for (party in c("Democrat", "Republican")) {
      for (a in 0:1) {
        for (outcome in c("unemployment", "inflation")) {
          x <- z[z$party == party & z$out == a, ]
          cells[[length(cells) + 1]] <- cbind(
            study = study, sample = sample, party = party,
            out = a, outcome = outcome, cell_summary(x[[outcome]])
          )
          for (k in c("better", "same", "worse")) {
            distributions[[length(distributions) + 1]] <- data.frame(
              study = study, sample = sample,
              party = party, out = a, outcome = outcome, response = k,
              n = sum(x[[paste0(outcome, "_", k)]] == 100), total = nrow(x),
              percent = mean(x[[paste0(outcome, "_", k)]])
            )
          }
        }
      }
    }
  }
  for (outcome in c("unemployment", "inflation", "average")) {
    diagnostics[[length(diagnostics) + 1]] <- cbind(
      study = study, outcome = outcome,
      permutation_check(d, outcome)
    )
    influence[[length(influence) + 1]] <- cbind(
      study = study, outcome = outcome,
      influence_range(d, outcome)
    )
    clusters[[length(clusters) + 1]] <- cbind(
      study = study, outcome = outcome,
      cluster_contrast(d, outcome)
    )
  }
  for (v in c("age", "gender", "education", "screen_pass")) {
    z <- d
    z$balance_value <- as.numeric(z[[v]])
    target <- if (v == "screen_pass") "retention" else "balance"
    current <- get(target)
    current[[length(current) + 1]] <- cbind(study = study, variable = v, contrast(
      z,
      "balance_value"
    ))
    assign(target, current)
  }
}
results <- do.call(rbind, results)
primary <- subset(results, sample == "Full")
primary$p_holm <- NA_real_
family <- primary$party == "Pooled" & primary$outcome %in% c("unemployment", "inflation")
primary$p_holm[family] <- p.adjust(primary$p[family], method = "holm")
save_table(results, "effects")
save_table(primary, "primary")
for (name in c(
  "flow", "cells", "distributions", "diagnostics", "influence", "clusters",
  "balance", "retention"
)) {
  save_table(do.call(rbind, get(name)), name)
}
print(subset(primary, party == "Pooled" & outcome %in% c(
  "unemployment", "inflation",
  "unemployment_better", "inflation_better"
)), row.names = FALSE)
writeLines(trimws(capture.output(sessionInfo()), which = "right"), "tabs/session_info.txt")
