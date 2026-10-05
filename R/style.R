paper_theme <- function() {
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

save_figure <- function(plot, name, width = 7, height = 4) {
  ggplot2::ggsave(file.path("figs", paste0(name, ".pdf")), plot,
    width = width,
    height = height
  )
  ggplot2::ggsave(file.path("figs", paste0(name, ".png")), plot,
    width = width,
    height = height, dpi = 220, bg = "white"
  )
}
