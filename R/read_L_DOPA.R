
#' Filter Positive L-DOPA Measurements Based on Abiotic Correction
#' This function takes a data frame of L-DOPA measurements and returns only the positive values, applying a threshold of zero. Negative values are replaced with `NA`. It assumes that the first five columns contain the relevant measurements.
#'
#' @param data A data frame containing L-DOPA measurements. The first five columns are expected to contain the replicate data.
#'
#' @return  A data frame with the same structure as the input, where only positive values are retained in the first five columns. Negative values are replaced with `NA`.
#' @export
#'
#' @examples
#' # Example data frame with some negative values
#' example_data <- data.frame(rep1 = c(-0.1, 0.5, 0.3),
#'                            rep2 = c(0.2, -0.4, 0.1),
#'                            rep3 = c(0.6, 0.7, -0.2),
#'                            rep4 = c(0.1, 0.3, -0.1),
#'                            rep5 = c(-0.5, 0.8, 0.2))
#'
#' # Apply the function
#' return_positive_ldopa(example_data)
#'
return_positive_ldopa <- function(data) {
  data[, 1:5] <- ifelse(data[, 1:5] > 0, data[, 1:5], NA)
  return(data)
}
return_positive_ldopa <- function(data) {
  return(ifelse(data[, 1:5] > 0, data[, 1:5], NA))
}

#' Read L-DOPA Measurement Data
#'
#' @param df Path to the Excel file.
#' @param cells The cell range in the Excel sheet to read from (e.g., "Z114:AD114").
#' @param col_names Names of the columns for the output data frame. Defaults to "rep1", "rep2", ..., "rep5".
#'
#' @return A data frame with positive L-DOPA measurements.
#' @export
#'
#' @examples read_L_DOPA("data.xlsx", cells = "Z114:AD114")
read_L_DOPA <- function(df, cells = "Z114:AD114") {
  measurement = read_excel(df,  range = cells,  col_names = paste0("rep", seq_along(1:5)))
  final = return_positive_ldopa(measurement)
  return(final)
}

