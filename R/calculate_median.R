#' Calculate Median for Each Sample
#'
#' This function calculates the median of replicate measurements for each sample and adds the result as a new column named "median" to the input data frame.
#'
#' @param x A data frame containing enzyme activity measurements. The first column should be `sample` identifiers, and the remaining columns should contain replicate measurements of enzyme activity for each sample.
#'
#' @return A data frame with the original data and an additional column `median`, which contains the median of the replicate measurements for each sample.
#'
#' @export
#'
#' @examples
#' # Example data frame with samples and replicate measurements
#' # df <- data.frame(
#' #   sample = c("S1", "S2", "S3"),
#' #   rep1 = c(1.2, 0.8, 1.5),
#' #   rep2 = c(1.1, 0.9, 1.6),
#' #   rep3 = c(1.3, 0.7, 1.4)
#' # )
#' # result <- calculate_median(df)
#' # print(result)
calculate_median <- function(x) {
  x$median = apply(x[, -1], 1, median, na.rm = TRUE)
  return(x)
}

