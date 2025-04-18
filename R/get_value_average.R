#' Calculate Average Contrasts Result
#'
#' This function calculates the average contrasts result from a list of emmGrid objects.
#'
#' @param x A character string representing the name of the argument to extract from the emmGrid objects.
#' @param data A list of emmGrid objects from which to extract the contrasts and p-values.
#'
#' @return A data frame containing the contrasts, positions, and the average values until the 2nd significant figure.
#' @export
#'
#' @examples
#' # Example usage:
#' # data <- list(emmGrid1, emmGrid2, ...)
#' # result <- average_contrasts_result_calculation("p.value", data)
#' # print(result)
#'
get_value_average <- function(x, list){

  # Extract p-values from each emmGrid object
  arg_name <- deparse(substitute(x))
  var_name <- paste(arg_name, "_matrix")

  # Convert the list of p-values to a matrix
  var_name <- do.call(rbind, list)

  # Compute the average p-value for each pairwise comparison
  return(apply(var_name, 2, mean)|> signif(digits = 2))
}
