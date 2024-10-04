#' Column Names and Labeller
#'
#' These functions handle custom column names and labeling for use in `ggplot2` or other visualizations.
#' The `col_names` vector provides a mapping between short column identifiers and their full descriptive names.
#' The `column_labeller` function retrieves the appropriate label for each column based on its value, useful for customizing facet labels or other plot components.
#'
#' @return
#' - `col_names`: A named vector that maps column identifiers to descriptive column names.
#' - `column_labeller`: A function that returns the appropriate column label for a given value.
#' @export
#'
#' @examples
#' # Accessing column names
#' col_names
#'
#' # Using column_labeller with ggplot2
#' column_labeller(variable = NULL, value = "Col1")
#'
#' # Example usage with ggplot2 for custom facet labeling
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   facet_wrap(~gear, labeller = column_labeller)
col_names <- c(
  `Col1` = "Col 1",
  `Col2` = "Col 2",
  `Col3` = "Col 3"
)

column_labeller <- function(variable, value) {
  return(col_names[value])
}
