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
read_data <- function(study, root = "data/raw") {
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
