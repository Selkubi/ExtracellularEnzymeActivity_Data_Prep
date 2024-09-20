#' Read Glucosidase (Glu) Enzyme Activity Data
#'
#' This function reads Glucosidase (Glu) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the Glu enzyme activity data.
#' @param range anchored(anchor = "Z51", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)

#'
#' @return A data frame containing positive Glu enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_glu("enzyme_data.xlsx")
read_glu <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z51", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

#' Read Xylosidas (Xyl) Enzyme Activity Data
#'
#' This function reads Xylosidas (Xyl) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the Xyl enzyme activity data.
#' @param range anchored(anchor = "Z57", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)
#'
#' @return A data frame containing positive Xyl enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_xyl("enzyme_data.xlsx")
read_xyl <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z57", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

#' Read N-Acetylglucosaminidase (NAG) Enzyme Activity Data
#'
#' This function reads N-Acetylglucosaminidase (NAG) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the NAG enzyme activity data.
#' @param range anchored(anchor = "Z63", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)
#'
#'
#' @return A data frame containing positive NAG enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_NAG("enzyme_data.xlsx")
read_NAG <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z63", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

#' Read Phosphatase (Pho) Enzyme Activity Data
#'
#' This function reads Phosphatase (Pho) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the Pho enzyme activity data.
#' @param range anchored(anchor = "Z69", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)
#'
#' @return A data frame containing positive Pho enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_Pho("enzyme_data.xlsx")
read_Pho <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z69", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

#' Read Cellobiohydrolase (Cbh) Enzyme Activity Data
#'
#' This function reads Cellobiohydrolase (Cbh) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the Cbh enzyme activity data.
#' @param range anchored(anchor = "Z75", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)
#'
#' @return A data frame containing positive Cbh enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_Cbh("enzyme_data.xlsx")
read_Cbh <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z75", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

#' Read Peptidase (Pep) Enzyme Activity Data
#'
#' This function reads Peptidase (Pep) enzyme activity measurements from an Excel file.
#'
#' @param df A string representing the path to the Excel file containing the Pep enzyme activity data.
#' @param range anchored(anchor = "Z75", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)
#'
#' @return A data frame containing positive Pep enzyme activity measurements.
#' @export
#'
#' @examples
#' # Example usage:
#' # read_Pep("enzyme_data.xlsx")
read_Pep <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z83", dim = c(2L, 5L), input = NULL,
                                                byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

