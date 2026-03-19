
#-------------------------------------------------------------------
# Function to save r plots and theme for r plots
#--------------------------------------------------------------------



save_plot <- function(plot, filename, width = 8, height = 6) {
  ggsave(
    file.path(output_plots_Dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
}


theme_plot <- function() {
  theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}
