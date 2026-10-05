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
