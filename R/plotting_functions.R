set_coloring_column <- function(data) {
  data$highlight = factor(paste0(ifelse(data$sample_date == "0", "before", "after"), " C", substr(data$col_no, 4, 4)))
  return(data)
}


col_names <- c(
  `Reservoir` = "Res.",
  `Col1` = "Column 1",
  `Col2` = "Column 2",
  `Col3` = "Column 3"
)

#' Generate Observation Count for Each Group
#'
#' This function calculates the number of observations for a given vector `x`.
#' It returns a data frame with the maximum value of `x` (used for positioning the label)
#' and a label that shows the number of observations.
#'
#' @param x A numeric vector for which the observation count will be computed.
#' @return A data frame with two columns: `y` containing the maximum value of `x`,
#' and `label` containing the number of observations in `x`.
#'
#' @examples
#' # Example usage of n_fun
#' data <- c(1, 2, 3, 4, NA)
#' n_fun(data)
#'
n_fun <- function(x) {
  return(data.frame(y = max(x, na.rm = TRUE), label = paste0(length(x))))
}


#' Add Observation Numbers to a ggplot2 Plot
#'
#' This function adds the number of observations for each group as text labels on a ggplot.
#' It uses the `n_fun` function to calculate the observation numbers.
#'
#' @param mapping Aesthetic mappings created by `aes()` in ggplot2.
#' @param data The data to be displayed in the plot. If `NULL`, the default, the data is inherited from the plot.
#' @param position Position adjustment as in `geom_text()`.
#' @param na.rm A logical value indicating whether missing values (NA) should be removed. Defaults to `TRUE`.
#' @param ... Other arguments passed to `stat_summary()`.
#'
#' @return A layer that adds observation numbers to the plot.
#'
#' @examples
#' # Example usage in ggplot2:
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_boxplot() +
#'   observation_numbers(aes(x = factor(cyl), y = mpg))
#'
observation_numbers <- function(mapping = NULL, data = NULL, position = "identity", na.rm = TRUE, ...) {
  stat_summary(fun.data = n_fun, geom = "text", na.rm = na.rm, mapping = mapping, position = position, ...)
}
