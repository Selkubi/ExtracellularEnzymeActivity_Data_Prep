#' Convert Selected Columns to Numeric
#'
#' This function converts the first five columns of a data table to numeric type.
#'
#' @import data.table
#' @param x A data table containing at least five columns.
#'
#' @return The modified data table with the first five columns converted to numeric.
#'
#' @export
#'
#' @examples
#' # Example data table
#' # dt <- data.table(V1 = c("1", "2"), V2 = c("3", "4"), V3 = c("5", "6"), V4 = c("7", "8"), V5 = c("9", "10"))
#' # result <- convert_to_numeric(dt)
#' # print(result)
convert_to_numeric <- function(x) {
  x[, c("V1", "V2", "V3", "V4", "V5") := lapply(.SD, as.numeric), .SDcols = c("V1", "V2", "V3", "V4", "V5")]
  return(x)
}
