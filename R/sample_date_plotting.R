#' Custom Color and Fill Scales for Sampling Date
#'
#' These functions define manual color and fill scales for representing sampling dates in `ggplot2` visualizations.
#' Different colors are assigned to each sampling date, allowing for distinct visualization of date-related data.
#'
#' @return Both functions return a scale object for use in `ggplot2` plots.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar(position = "dodge") +
#'   fill_sample_date()
#'
#' ggplot(mtcars, aes(factor(cyl), color = factor(gear))) +
#'   geom_line() +
#'   color_sample_date()
fill_sample_date <- function() {
  ggplot2::scale_fill_manual(name =  "Sampling Date",
                    values = c("#b7cdb8", "#e7dff1", "#c9bbd8", "#ac98c1"),
                    guide = "legend")
}

color_sample_date <- function() {
  ggplot2::scale_color_manual(name =  "Sampling Date",
                     values = c("#b7cdb8", "#e7dff1", "#c9bbd8", "#ac98c1"),
                     guide = "legend")
}

fill_sample_date_no_zero <- function() {
  ggplot2::scale_fill_manual(name =  "Sampling Date",
                             values = c("#e7dff1", "#c9bbd8", "#ac98c1"),
                             guide = "legend")
}

color_sample_date_no_zero <- function() {
  ggplot2::scale_color_manual(name =  "Sampling Date",
                              values = c("#e7dff1", "#c9bbd8", "#ac98c1"),
                              guide = "legend")
}

color_column <- function() {
  ggplot2::scale_color_manual(name =  "Column Position",
                              values = c("#164C6B", "#F3A712", "#E4572E"),
                              guide = "legend")
}
