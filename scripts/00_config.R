project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(project_root, ...)
raw_dir <- project_file("data", "raw")
derived_dir <- project_file("data", "derived")
table_dir <- project_file("tabs")
figure_dir <- project_file("figs")
prepared_data_file <- file.path(derived_dir, "prepared_data.rds")
studies <- c("MTurk", "Lucid")
colors <- c(Full = "#174A65", `Screen passed` = "#B05A26", reference = "grey65")
figure_size <- c(width = 7, height = 3.3)
figure_dpi <- 220
table_style <- list(font_size = "small", column_padding = "6pt", row_stretch = 1, digits = 1)

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(), strip.text = ggplot2::element_text(
      face = "bold",
      color = "#222222"
    ), axis.text = ggplot2::element_text(color = "#222222"),
    axis.title.y = ggplot2::element_blank(), plot.title.position = "plot",
    legend.position = "bottom"
  )
}
