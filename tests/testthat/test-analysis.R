root <- normalizePath("../..")
source(file.path(root, "R/data.R"))
source(file.path(root, "R/analysis.R"))
mt <- read_data("MTurk", file.path(root, "data/raw"))
lu <- read_data("Lucid", file.path(root, "data/raw"))
m <- mt[mt$eligible & mt$partisan, ]
l <- lu[lu$eligible & lu$partisan, ]
testthat::test_that("eligibility, screens, and party codes match instruments", {
  testthat::expect_equal(c(nrow(m), nrow(l)), c(1425L, 624L))
  testthat::expect_equal(c(sum(m$screen_pass), sum(l$screen_pass)), c(861L, 529L))
  testthat::expect_equal(c(sum(m$strict), sum(l$strict)), c(1258L, 467L))
  testthat::expect_equal(party_lucid(1:10), c(
    rep("Democrat", 3), "Nonpartisan",
    "Republican", "Democrat", "Nonpartisan", rep("Republican", 3)
  ))
  testthat::expect_equal(response_score(c(1, 2, 3, NA)), c(100, 50, 0, NA))
  testthat::expect_error(response_score(4), "Unexpected")
  testthat::expect_equal(c(length(unique(m$ip_group)), length(unique(l$ip_group))), c(
    1381L,
    621L
  ))
})
testthat::test_that("both Lucid cue arms preserve better responses", {
  for (a in 0:1) {
    z <- l[l$cue_d == a, ]
    raw <- z[[if (a == 1) "unemployment_d" else "unemployment_r"]]
    testthat::expect_true(sum(raw == 1) > 0)
    testthat::expect_equal(z$unemployment_better, 100 * (raw == 1))
  }
  z <- l[l$screen_pass, ]
  testthat::expect_equal(unname(coef(lm(unemployment_better ~ out, z))[2]),
    -9.175050301810865,
    tolerance = 1e-8
  )
})
testthat::test_that("standardized effects and variances match saturated HC2 regression", {
  for (z in list(m, l)) {
    for (item in c("unemployment", "inflation", "average")) {
      z$cell <- factor(paste(z$party, z$out))
      fit <- lm(reformulate("cell", item, intercept = FALSE), z)
      w <- rep(as.numeric(table(z$party)) / nrow(z), each = 2) * c(-1, 1)
      a <- contrast(z, item)
      testthat::expect_equal(a$estimate, sum(w * coef(fit)), tolerance = 1e-10)
      regression_variance <- drop(t(w) %*% sandwich::vcovHC(fit, type = "HC2") %*% w)
      testthat::expect_equal(a$se^2, regression_variance, tolerance = 1e-10)
      testthat::expect_equal(cluster_contrast(z, item)$estimate, a$estimate, tolerance = 1e-10)
    }
  }
})
testthat::test_that("original screened score contrasts are independently reproduced", {
  testthat::expect_equal(unname(coef(lm(unemployment ~ out, m[m$screen_pass, ]))[2]),
    -12.49391727494,
    tolerance = 1e-8
  )
  testthat::expect_equal(unname(coef(lm(inflation ~ out, l[l$screen_pass, ]))[2]),
    -10.173900546134,
    tolerance = 1e-8
  )
})
testthat::test_that("paired outcomes retain within-person covariance", {
  testthat::expect_equal(
    contrast(m, "average")$estimate,
    (contrast(m, "unemployment")$estimate + contrast(m, "inflation")$estimate) / 2
  )
  independent_variance <-
    (contrast(m, "unemployment")$se^2 + contrast(m, "inflation")$se^2) / 4
  testthat::expect_true(contrast(m, "average")$se^2 > independent_variance)
  testthat::expect_equal(
    m$unemployment_better + m$unemployment_same + m$unemployment_worse,
    rep(100, nrow(m))
  )
  testthat::expect_equal(l$inflation_better + l$inflation_same + l$inflation_worse, rep(
    100,
    nrow(l)
  ))
})
testthat::test_that("single-deletion diagnostic agrees with direct fixed-share calculation", {
  z <- m
  shares <- prop.table(table(z$party))
  i <- which.min(z$unemployment)
  a <- z[-i, ]
  direct <- sum(vapply(names(shares), function(g) {
    opposing <- mean(a$unemployment[a$party == g & a$out == 1])
    own <- mean(a$unemployment[a$party == g & a$out == 0])
    shares[g] * (opposing - own)
  }, numeric(1)))
  r <- influence_range(z, "unemployment")
  testthat::expect_true(direct >= r$minimum - 1e-10 && direct <= r$maximum + 1e-10)
  testthat::expect_equal(
    permutation_check(l, "unemployment", draws = 99),
    permutation_check(l, "unemployment", draws = 99)
  )
})
