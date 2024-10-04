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
      text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 11),
      axis.text.x = ggplot2::element_text(color = "black"),
      axis.text.y = element_blank(),
      legend.position = "bottom",
      strip.placement ="outside",
      strip.background = ggplot2::element_blank(),
      strip.text = element_text(size = 11, color = "black"),
      axis.line =  ggplot2::element_line(color = "black"),
      panel.border = ggplot2::element_blank(),
      axis.line.y.right = ggplot2::element_line(color = "black"),
      panel.grid = ggplot2::element_blank())
}


panel.background = element_rect(fill = NA),
