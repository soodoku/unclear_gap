valid_codes <- function(x, values) {
  if (any(!is.na(x) & !x %in% values)) stop("Unexpected response code")
  x
}
party_mturk <- function(d) {
  out <- rep("Nonpartisan", nrow(d))
  out[d$pid_dem %in% 1:2 | d$pid_ind %in% 2] <- "Democrat"
  out[d$pid_rep %in% 1:2 | d$pid_ind %in% 1] <- "Republican"
  out[is.na(d$pid3_raw)] <- NA_character_
  out
}
party_lucid <- function(x) {
  valid_codes(x, 1:10)
  ifelse(is.na(x), NA_character_, ifelse(x %in% c(1, 2, 3, 6), "Democrat",
    ifelse(x %in% c(5, 8, 9, 10), "Republican", "Nonpartisan")
  ))
}
response_score <- function(x) 50 * (3 - valid_codes(x, 1:3))
read_data <- function(study, root = raw_dir) {
  stopifnot(study %in% c("MTurk", "Lucid"))
  d <- read.csv(file.path(root, paste0(tolower(study), ".csv")), na.strings = "")
  stopifnot(!anyDuplicated(d$row_id), all(vapply(d, is.numeric, logical(1))))
  if (study == "MTurk") {
    stopifnot(nrow(d) == 1505L)
    d$party <- party_mturk(d)
    d$strict <- d$pid3_raw %in% 1:2
    flags <- cbind(
      d$sleep %in% c(1, 5), d$prosthetic == 1, d$vision == 1,
      d$hearing == 1, d$gang == 1, d$family_gang == 1
    )
    d$screen_count <- rowSums(flags, na.rm = TRUE)
    d$flagged <- d$funny_ip == 1 | d$screen_count > 1
    d$screen_pass <- !d$flagged
    d$eligible <- d$consent == 1 & !is.na(d$cue_d)
  } else {
    stopifnot(nrow(d) == 821L)
    d$party <- party_lucid(d$political_party)
    d$strict <- d$political_party %in% c(1, 2, 9, 10)
    att <- d[paste0("attention_", 1:5)]
    d$screen_pass <- complete.cases(att) & att[[1]] == 1 & att[[2]] == 1 &
      rowSums(att[3:5], na.rm = TRUE) == 0
    d$flagged <- !d$screen_pass
    d$eligible <- d$consent == 1 & d$preview == 0 & !is.na(d$cue_d)
  }
  valid_codes(d$cue_d, 0:1)
  for (item in c("unemployment", "inflation")) {
    a <- valid_codes(d[[paste0(item, "_d")]], 1:3)
    b <- valid_codes(d[[paste0(item, "_r")]], 1:3)
    stopifnot(!any(!is.na(a) & !is.na(b)))
    stopifnot(all(is.na(a) | d$cue_d %in% 1), all(is.na(b) | d$cue_d %in% 0))
    raw <- ifelse(d$cue_d == 1, a, b)
    d[[item]] <- response_score(raw)
    for (k in 1:3) d[[paste0(item, "_", c("better", "same", "worse")[k])]] <- 100 * (raw == k)
  }
  d$partisan <- d$party %in% c("Democrat", "Republican")
  d$out <- as.integer((d$party == "Democrat") != (d$cue_d == 1))
  d$out[!d$partisan] <- NA_integer_
  d$average <- (d$unemployment + d$inflation) / 2
  d$study <- study
  stopifnot(all(complete.cases(d[d$eligible, c("unemployment", "inflation")])))
  d
}

cell_summary <- function(y) {
  y <- y[!is.na(y)]
  if (length(y) < 2) stop("Each comparison cell needs at least two observations")
  data.frame(mean = mean(y), variance = var(y) / length(y), n = length(y))
}
combine_cells <- function(cells, coefficients) {
  stopifnot(nrow(cells) == length(coefficients))
  estimate <- sum(coefficients * cells$mean)
  v <- coefficients^2 * cells$variance
  se <- sqrt(sum(v))
  df <- if (se == 0) sum(cells$n) - nrow(cells) else sum(v)^2 / sum(v^2 / (cells$n - 1))
  critical <- qt(.975, df)
  p <- if (se == 0) as.numeric(estimate == 0) else 2 * pt(-abs(estimate / se), df)
  data.frame(
    estimate = estimate, se = se, df = df, low = estimate - critical * se,
    high = estimate + critical * se, p = p, n = sum(cells$n)
  )
}
contrast <- function(d, outcome, group = "party") {
  stopifnot(nrow(d) > 0, !anyNA(d[[group]]), all(d$out %in% 0:1))
  groups <- sort(unique(d[[group]]))
  shares <- as.numeric(table(factor(d[[group]], levels = groups))) / nrow(d)
  cells <- do.call(rbind, lapply(groups, function(g) {
    do.call(rbind, lapply(0:1, function(a) {
      cell_summary(d[
        d[[group]] == g & d$out == a,
        outcome
      ])
    }))
  }))
  result <- combine_cells(cells, rep(shares, each = 2) * rep(c(-1, 1), length(groups)))
  result$inparty <- sum(shares * cells$mean[seq(1, nrow(cells), 2)])
  result$outparty <- sum(shares * cells$mean[seq(2, nrow(cells), 2)])
  result
}
cluster_contrast <- function(d, outcome) {
  groups <- sort(unique(d$party))
  d$cell <- factor(paste(d$party, d$out), levels = as.vector(t(outer(groups, 0:1, paste))))
  fit <- lm(reformulate("cell", outcome, intercept = FALSE), data = d)
  shares <- as.numeric(table(factor(d$party, levels = groups))) / nrow(d)
  coef <- rep(shares, each = 2) * rep(c(-1, 1), length(groups))
  v <- sandwich::vcovCL(fit, cluster = d$ip_group, type = "HC1")
  estimate <- sum(coef * stats::coef(fit))
  se <- sqrt(drop(t(coef) %*% v %*% coef))
  clusters <- length(unique(d$ip_group))
  data.frame(
    estimate = estimate, se = se, low = estimate - qt(.975, clusters - 1) * se,
    high = estimate + qt(.975, clusters - 1) * se, clusters = clusters, n = nrow(d)
  )
}
permutation_check <- function(d, outcome, draws = 9999L, seed = 20261004L) {
  set.seed(seed)
  groups <- split(seq_len(nrow(d)), d$party)
  statistic <- function(out) {
    sum(vapply(groups, function(i) {
      length(i) / nrow(d) *
        (mean(d[[outcome]][i[out[i] == 1]]) - mean(d[[outcome]][i[out[i] == 0]]))
    }, numeric(1)))
  }
  observed <- statistic(d$out)
  sims <- replicate(draws, {
    a <- d$out
    for (i in groups) a[i] <- sample(a[i])
    statistic(a)
  })
  data.frame(
    estimate = observed, draws = draws, seed = seed,
    p = (1 + sum(abs(sims) >= abs(observed) - 1e-12)) / (draws + 1)
  )
}
influence_range <- function(d, outcome) {
  estimate <- contrast(d, outcome)$estimate
  shares <- table(d$party) / nrow(d)
  after <- numeric(nrow(d))
  for (p in unique(d$party)) {
    for (a in 0:1) {
      i <- which(d$party == p & d$out == a)
      y <- d[[outcome]][i]
      after[i] <- estimate + shares[p] * (2 * a - 1) * (mean(y) - y) / (length(i) - 1)
    }
  }
  data.frame(estimate = estimate, minimum = min(after), maximum = max(after))
}

save_table <- function(x, name) {
  write.csv(x, file.path(table_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
}

save_figure <- function(plot, name,
                        width = figure_size[["width"]], height = figure_size[["height"]]) {
  ggplot2::ggsave(file.path(figure_dir, paste0(name, ".pdf")), plot,
    width = width,
    height = height
  )
  ggplot2::ggsave(file.path(figure_dir, paste0(name, ".png")), plot,
    width = width,
    height = height, dpi = figure_dpi, bg = "white"
  )
}
