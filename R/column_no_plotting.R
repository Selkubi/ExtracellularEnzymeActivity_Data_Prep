#' Custom Fill Colors for Column Location
#'
#' This function defines a manual color scale for filling based on the column location, with specific colors assigned to each column.
#'
#' @return A scale object for use in ggplot2.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar(position = "dodge") +
#'   fill_col_no()
fill_col_no <- function() {
  ggplot2::scale_fill_manual(name =  "Column Location",
                    values = c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                    guide = "legend")
}

#' Custom Line Colors for Column Location
#'
#' This function defines a manual color scale for line colors based on the column location, with specific colors assigned to each column.
#'
#' @return A scale object for use in ggplot2.
#' @export
#'
#' @examples
#' # Example usage in a ggplot
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), color = factor(gear))) +
#'   geom_line() +
#'   color_col_no()
color_col_no <- function() {
  ggplot2::scale_color_manual(name =  "Column Location",
                     values = c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                     guide = "legend")
}
