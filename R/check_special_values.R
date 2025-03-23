#' Check for NaN Values in a Data Frame
#'
#' This function checks each element of a data frame to determine if it is a NaN (Not a Number).
#' It returns a logical data frame of the same dimensions, where `TRUE` indicates the presence of NaN and `FALSE` indicates the absence of NaN.
#'
#' @param x A data frame to be checked for NaN values.
#' @return A logical data frame where each element is `TRUE` if the corresponding element in `x` is NaN, and `FALSE` otherwise.
#'
#' @examples
#' df <- data.frame(a = c(1, NaN, 3), b = c(NaN, 2, 3))
#' is.nan.data.frame(df)
#'

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

#' Check for Infinite Values in a Data Frame
#'
#' This function checks each element of a data frame to determine if it is an infinite value (Inf or -Inf).
#' It returns a logical data frame of the same dimensions, where `TRUE` indicates the presence of infinite values and `FALSE` indicates the absence of them.
#'
#' @param x A data frame to be checked for infinite values.
#' @return A logical data frame where each element is `TRUE` if the corresponding element in `x` is infinite, and `FALSE` otherwise.
#'
#' @examples
#' df <- data.frame(a = c(1, Inf, -Inf), b = c(3, 2, Inf))
#' is.inf.data.frame(df)
#'

is.inf.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.infinite))
}
