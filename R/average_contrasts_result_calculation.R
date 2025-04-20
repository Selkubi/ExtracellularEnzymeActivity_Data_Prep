
#' Calculate Average Contrasts Result
#'
#' This function calculates the average contrasts result from a list of emmGrid objects.
#'
#' @param x A character string representing the name of the argument to extract from the emmGrid objects.
#' @param data A list of emmGrid objects from which to extract the contrasts and p-values.
#'
#' @return A data frame containing the contrasts, positions, and the average values.
#' @export
#'
#' @examples
#' # Example usage:
#' # data <- list(emmGrid1, emmGrid2, ...)
#' # result <- average_contrasts_result_calculation("p.value", data)
#' # print(result)
#'
average_contrasts_result_calculation <- function(x, data){
  arg_name <- x
  value_name <- paste0("average_", arg_name)

  dynamic_list <-list()
  # Extract p-values from each emmGrid object
  for (i in seq_along(data)) {
    dynamic_list[[i]] <- summary(data[[i]])[[arg_name]]
  }

  # Create a data frame to store the results
  results <- data[[i]]@grid

  results[[eval(value_name)]] <- get_value_average(arg_name, dynamic_list)
  return(results)

}
