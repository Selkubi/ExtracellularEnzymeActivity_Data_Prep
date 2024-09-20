#' Calculate Trimmed Mean for Each Sample
#'
#' This function calculates the trimmed mean (with 25% trimming) for each sample across multiple replicates, and adds the resulting mean as a new column named "median" to the input data frame.
#'
#' @param x A data frame containing enzyme activity measurements. The first column should be `sample` identifiers, and the remaining columns should contain replicate measurements of enzyme activity for each sample.
#'
#' @return A data frame with the original data and an additional column `median`, which contains the trimmed mean (25% trimming) of the replicate measurements for each sample.
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
#' # result <- calculate_mean(df)
#' # print(result)
calculate_mean <- function(x) {
  x$median = apply(x[, -1], 1, mean, na.rm = TRUE, trim = 0.25)
  return(x)
}
