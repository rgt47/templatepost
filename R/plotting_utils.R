# Plotting utilities for blog analysis
#
# Reusable plotting functions and theme settings for consistent
# visualization across analysis/scripts/03_generate_figures.R and any
# post that sources this file directly (source("R/plotting_utils.R")).

# Configure a minimal, clean ggplot2 theme suitable for publication.
setup_plot_theme <- function(base_size = 12) {
  ggplot2::theme_set(ggplot2::theme_minimal(base_size = base_size))
  invisible()
}

# Named vector of hex colors for consistent use across figures.
get_analysis_colors <- function() {
  c(
    primary = "#FF6B6B",
    secondary = "#4ECDC4",
    tertiary = "#45B7D1",
    quaternary = "#96CEB4"
  )
}

# Wrapper around ggplot2::ggsave() with consistent DPI/format settings.
# Creates the parent directory if needed.
save_plot <- function(filename, plot = ggplot2::last_plot(),
                       width = 8, height = 5, dpi = 300) {
  if (is.null(plot)) {
    stop("No plot to save. Either provide 'plot' argument or create a plot first.")
  }

  dir <- dirname(filename)
  if (dir != "." && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  ggplot2::ggsave(
    filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = "png"
  )

  cat("Saved:", filename, "\n")
  invisible(filename)
}

# Combine multiple plots into a grid via patchwork.
combine_plots <- function(..., ncol = 2, heights = NULL) {
  plots <- list(...)

  if (length(plots) == 0) {
    stop("No plots provided")
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork package required. Install with: install.packages('patchwork')")
  }

  combined <- Reduce(function(p1, p2) p1 + p2, plots)

  if (is.null(heights)) {
    combined + patchwork::plot_layout(ncol = ncol)
  } else {
    combined + patchwork::plot_layout(ncol = ncol, heights = heights)
  }
}
