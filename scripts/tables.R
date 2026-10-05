d <- read.csv("tabs/effects.csv")
f <- read.csv("tabs/flow.csv")
fmt <- function(x) formatC(x, digits = 1, format = "f")
ci <- function(x) paste0("[", fmt(x$low), ", ", fmt(x$high), "]")
macros <- character()
for (study_name in c("MTurk", "Lucid")) {
  for (item in c(
    "unemployment", "inflation", "unemployment_better", "inflation_better",
    "average"
  )) {
    x <- subset(d, d$study == study_name & sample == "Full" & party == "Pooled" & outcome == item)
    stopifnot(nrow(x) == 1L)
    prefix <- paste0(study_name, gsub("_", "", item))
    for (v in c("estimate", "inparty", "outparty")) {
      macros <- c(
        macros,
        paste0("\\newcommand{\\", prefix, v, "}{", fmt(x[[v]]), "}")
      )
    }
    macros <- c(macros, paste0("\\newcommand{\\", prefix, "CI}{", ci(x), "}"))
  }
  stages <- c(
    N = "Partisans", Export = "Export records", Eligible = "Consenting nonpreview assigned",
    Nonpartisan = "Nonpartisans", Screen = "Screen passed partisans",
    Flagged = "Screen flagged partisans", Strict = "Strict identifiers"
  )
  for (n in names(stages)) {
    macros <- c(macros, paste0(
      "\\newcommand{\\", study_name, n, "}{",
      format(f$n[f$study == study_name & f$stage == stages[[n]]],
        big.mark = ",",
        trim = TRUE
      ), "}"
    ))
  }
}
writeLines(macros, "tabs/macros.tex")
rows <- function(x, fields, path) {
  writeLines(apply(x[fields], 1, function(z) paste0(paste(z, collapse = " & "), " \\\\")), path)
}
x <- subset(d, sample == "Full" & party == "Pooled" & outcome %in% c(
  "unemployment",
  "inflation"
))
x$item <- tools::toTitleCase(x$outcome)
x$interval <- ci(x)
for (v in c("estimate", "inparty", "outparty")) x[[v]] <- fmt(x[[v]])
rows(x, c("study", "item", "n", "inparty", "outparty", "estimate", "interval"), "tabs/main.tex")
x <- subset(d, party == "Pooled" & outcome %in% c("unemployment", "inflation"))
x$item <- tools::toTitleCase(x$outcome)
x$effect <- paste0(fmt(x$estimate), " ", ci(x))
rows(x, c("study", "sample", "item", "n", "effect"), "tabs/sensitivity.tex")
x <- subset(d, sample == "Full" & party != "Pooled" & outcome %in% c(
  "unemployment",
  "inflation"
))
x$item <- tools::toTitleCase(x$outcome)
x$effect <- paste0(fmt(x$estimate), " ", ci(x))
rows(x, c("study", "party", "item", "n", "effect"), "tabs/party.tex")
x <- subset(d, sample == "Full" & party == "Pooled" & grepl("_better", outcome))
x$item <- tools::toTitleCase(sub("_better", "", x$outcome))
x$effect <- paste0(fmt(x$estimate), " ", ci(x))
x$inparty <- fmt(x$inparty)
x$outparty <- fmt(x$outparty)
rows(x, c("study", "item", "inparty", "outparty", "effect"), "tabs/binary.tex")
lines <- readLines("docs/README.in.md")
for (study_name in c("MTurk", "Lucid")) {
  for (item in c("unemployment", "inflation")) {
    x <- subset(d, d$study == study_name & sample == "Full" & party == "Pooled" & outcome == item)
    stopifnot(nrow(x) == 1L)
    lines <- gsub(paste0("{{", study_name, "_", item, "}}"), paste0(
      fmt(x$estimate), " ",
      ci(x)
    ), lines, fixed = TRUE)
  }
}
for (study_name in c("MTurk", "Lucid")) {
  lines <- gsub(paste0("{{", study_name, "_N}}"),
    format(f$n[f$study == study_name & f$stage == "Partisans"], big.mark = ",", trim = TRUE),
    lines,
    fixed = TRUE
  )
}
stopifnot(!any(grepl("{{", lines, fixed = TRUE)))
writeLines(lines, "README.md")
