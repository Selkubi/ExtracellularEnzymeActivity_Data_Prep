
#' Pastes the sample name from the file path
#'
#' @param x
#'
#' @return
#' @export
#'
name_change <- function(x) {
  substr(x$sample, 10, nchar(x$sample) - 5)
}

#' Extracts the enzyme concentration from all the plates' xlsx files for a certain enzyme
#'
#' @param x
#' @param func enzyme name to be extracted, formtted as read_'enzyme'. Ex; read_glu, read_xyl, read_NAG, read_Pho, read_Cbh, read_L_DOPA and read_Pep.
#' Check out the function documentations for their explanation and position in the plate.
#' @param ...
#'
#' @return a df of the selected enzyme extracted from all the plate xlsx files.
#' @export
#'
#' @examples  Gly <- enzyme_as_data_table(paths, func = read_glu)

enzyme_as_data_table <- function(x, func, ...) {
  df = data.table::as.data.table(t(mapply(x, FUN = func, USE.NAMES = TRUE)), keep.rownames = "sample")
  df$sample = name_change(df)
  return(df)
}
