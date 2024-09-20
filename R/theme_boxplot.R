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
theme_boxplot <-  function() {
  ggplot2::theme(panel.background = element_rect(fill = NA),
                 panel.grid = element_blank(),
                 strip.background = element_blank(),
                 strip.text = element_text(size = 11, color = "black"),
                 strip.placement = "outside") +
    ggplot2::theme(text = element_text(size = 11),
                   axis.line.x = element_line(color = "black"),
                   axis.line.y = element_line(),
                   axis.text = element_text(size = 11, color = "black"),
                   axis.title = element_blank(),
                   legend.position = "bottom")
}
