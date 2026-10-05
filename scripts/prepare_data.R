args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Supply the directory containing the original data folders")
root <- args[1]
paths <- file.path(root, c(
  "turk/2020_06_29.csv", "turk/merged_survey_ip_06_29_2020_final.csv",
  "lucid/Uncertainty+Effect+replication_January+28,+2023_20.38.zip"
))
read_source <- function(p) {
  read.csv(p,
    colClasses = "character", check.names = FALSE,
    na.strings = c("", "NA"), fileEncoding = "latin1"
  )
}
a <- read_source(paths[1])
b <- read_source(paths[2])
stopifnot(nrow(a) == 1505L, nrow(b) == 1505L)
stopifnot(!anyDuplicated(a[["Response ID"]]), !anyDuplicated(b$Response.ID))
j <- match(a[["Response ID"]], b$Response.ID)
stopifnot(!anyNA(j))
b <- b[j, ]
core <- c(
  "pid_dem", "pid_rep", "pid_ind", "randomization_1", "gop_unemployment",
  "gop_inflation", "obama_unemployment", "obama_inflation"
)
for (v in core) stopifnot(identical(a[[v]], b[[v]]))
number <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  stopifnot(all(is.na(x) | !is.na(out)))
  out
}
boolean <- function(x) {
  stopifnot(all(is.na(x) | tolower(x) %in% c("true", "false")))
  as.integer(tolower(x) == "true")
}
map <- c(
  consent = "consent", finished = "Finished", age = "age", gender = "gender",
  pid3_raw = "pid_3 - Selected Choice", pid_dem = "pid_dem", pid_rep = "pid_rep",
  pid_ind = "pid_ind", pol_interest = "pol_interest", ft_dems = "ft_dems", ft_reps = "ft_reps",
  unemployment_r = "gop_unemployment", unemployment_d = "obama_unemployment",
  inflation_r = "gop_inflation", inflation_d = "obama_inflation", sleep = "sleep",
  prosthetic = "prosthetic", vision = "blind", hearing = "deaf", gang = "gang",
  family_gang = "family_gang", hits = "hits", sincerity = "sincerity",
  education = "education"
)
t <- data.frame(row_id = seq_len(nrow(a)), ip_group = match(
  a[["IP Address"]],
  unique(a[["IP Address"]])
))
for (v in names(map)) t[[v]] <- number(a[[map[[v]]]])
for (i in 1:6) {
  t[[paste0("race_", i)]] <- vapply(a[["race - Selected Choice"]], function(x) {
    if (is.na(x)) {
      return(NA_integer_)
    }
    as.integer(as.character(i) %in% strsplit(x, ",", fixed = TRUE)[[1]])
  }, integer(1))
}
pre <- grep("^We'd like to understand", names(a), value = TRUE)
stopifnot(length(pre) == 3L)
for (i in seq_along(pre)) {
  t[[paste0("pre_", c(
    "economy", "unemployment",
    "inflation"
  )[i])]] <- number(a[[pre[i]]])
}
stopifnot(all(is.na(a$randomization_1) | a$randomization_1 %in% c("obama", "congress")))
t$cue_d <- as.integer(a$randomization_1 == "obama")
for (v in c("funny_ip", "duplicated", "foreign_ip")) t[[v]] <- boolean(b[[v]])
t$blacklisted <- number(b$blacklisted)
stopifnot(all(is.na(t$sleep) | as.integer(t$sleep %in% c(1, 5)) == number(b$sleep)))
for (pair in list(
  c("prosthetic", "prosthetic"), c("vision", "blind"),
  c("hearing", "deaf"), c("gang", "gang_resp"), c("family_gang", "gang_fam")
)) {
  stopifnot(identical(t[[pair[1]]] == 1, as.logical(boolean(b[[pair[2]]]))))
}
member <- unzip(paths[3], list = TRUE)$Name
stopifnot(length(member) == 1L, grepl("\\.csv$", member))
con <- unz(paths[3], member)
l <- read.csv(con, colClasses = "character", check.names = FALSE, na.strings = "")
stopifnot(grepl("ImportId", l$StartDate[2]), nrow(l) == 823L)
l <- l[-c(1, 2), ]
stopifnot(!anyDuplicated(l$ResponseId))
u <- data.frame(
  row_id = seq_len(nrow(l)), ip_group = match(l$IPAddress, unique(l$IPAddress)),
  consent = as.integer(l$consent == "Yes"),
  preview = as.integer(l$DistributionChannel == "preview"),
  finished = boolean(l$Finished)
)
for (v in c("age", "gender", "hhi", "ethnicity", "hispanic", "education", "political_party")) {
  u[[v]] <- number(l[[v]])
}
attention_labels <- c(
  "Extremely interested", "Very interested", "Moderately interested",
  "Slightly interested", "Not at all interested"
)
for (i in seq_along(attention_labels)) {
  u[[paste0("attention_", i)]] <- vapply(l$attn_check, function(x) {
    if (is.na(x)) {
      return(NA_integer_)
    }
    as.integer(attention_labels[i] %in% strsplit(x, ",", fixed = TRUE)[[1]])
  }, integer(1))
}
u$cue_d <- as.integer(l$econ_cond == "dems")
stopifnot(all(l$econ_cond %in% c("dems", "reps")))
items <- c(
  unemployment_r = "unemp_reps", unemployment_d = "unemploy_dems",
  inflation_r = "inflation_reps", inflation_d = "inflation_dems"
)
response_labels <- c("got better", "stayed about the same", "got worse")
for (v in names(items)) {
  value <- tolower(l[[items[[v]]]])
  stopifnot(all(is.na(value) | value %in% response_labels))
  u[[v]] <- match(value, response_labels)
}
for (entry in list(list(d = t, name = "mturk"), list(d = u, name = "lucid"))) {
  stopifnot(all(vapply(entry$d, is.numeric, logical(1))))
  write.csv(entry$d, file.path("data/raw", paste0(entry$name, ".csv")),
    row.names = FALSE,
    na = ""
  )
}
manifest <- data.frame(
  file = sub(paste0(root, "/"), "", paths, fixed = TRUE),
  rows = c(nrow(a), nrow(b), nrow(l)),
  sha256 = vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE)
)
write.csv(manifest, "docs/source_manifest.csv", row.names = FALSE)
