#' Read positive cells in the plate
#' It first checks if the abiotic correction for a cell is positive (saved in the above cell).
#'
#'  Returns the five replicates that has positive abiotic correction.If an abitoic correction is detected as negative, then it returns 0.
#' @param data
#'
#' @return a vector of 5 data points correcponding to the 5 concentrations calculated from the plate replicates.
#' @export
#'
#' @examples
read_positive_cells <- function(data) {
  return(ifelse(data[1, 1:5] > 0, data[2, 1:5], NA))
}
