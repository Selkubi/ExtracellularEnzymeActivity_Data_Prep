#' Perform All P-Value Calculations
#'
#' This function performs all necessary p-value calculations and accompanying calculations
#' for estimate, SE, df, and t.ratio from a list of emmGrid objects.
#'
#' @param data A list of emmGrid objects from which to extract the contrasts and values.
#'
#' @return A data frame containing the contrasts, positions, and the calculated values.
#' @export
#'
#' @examples
#' # Example usage:
#' # data <- list(emmGrid1, emmGrid2, ...)
#' # result <- all_p_value_calculations(data)
#' # print(result)
all_p_value_calculations <- function(object) {
  # Calculate the average p-value
  results <- average_contrasts_result_calculation("p.value", data = object)

  # Define the variables for additional calculations
  variables <- c("estimate", "SE", "df", "t.ratio")

  # Loop through each variable and merge the results
  for (i in seq_along(variables)) {
    # Calculate the average for the current variable
    temp_result <- average_contrasts_result_calculation(x = variables[[i]], data = object)

    # Merge the current result with the main data frame
    results <- merge(results, temp_result, by = colnames(temp_result[, 1:2]), all = TRUE)
  }

  return(results)
}
