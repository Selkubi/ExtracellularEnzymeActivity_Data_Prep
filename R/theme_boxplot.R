#' Custom Theme for Boxplots
#'
#' This function applies a custom `ggplot2` theme designed specifically for boxplot visualizations.
#' It removes panel backgrounds and grid lines, adjusts text and axis line styles, and positions the legend at the bottom.
#'
#' @return A `ggplot2` theme object that can be applied to ggplot visualizations.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   theme_boxplot()
theme_boxplot <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      # General text settings
      text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 11),
      # X-axis text settings
      axis.text.x = ggplot2::element_text(color = "black", size = 10),
      # Y-axis text settings
      axis.text.y = ggplot2::element_text(color = "black", size = 10),

      # Legend settings
      legend.position = "bottom",

      # Strip settings
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11, color = "black"),

      # Panel and axis line settings
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      panel.border = ggplot2::element_rect(color = "black", size = 0.5),  # Add panel border
      axis.line.y.right = ggplot2::element_line(color = "black", linewidth = 0.5),

      # Grid settings
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Plot background
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),

      # Boxplot specific settings
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)  # Adjust margins as needed
    )
}
