
read_positive_cells <- function(data) {
  return(ifelse(data[1, 1:5] > 0, data[2, 1:5], 0))
}

read_glu <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z51", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

read_xyl <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z57", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

read_NAG <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z63", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

read_Pho <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z69", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

read_Cbh <- function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z75", dim = c(2L, 5L), input = NULL, col_names = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

read_Pep = function(df) {
  measurement = read_xlsx(df,  range = anchored(anchor = "Z83", dim = c(2L, 5L), input = NULL,
                                              byrow = FALSE),  col_names = paste0("rep", seq_along(1:5)))
  final = read_positive_cells(measurement)
  return(final)
}

return_positive_ldopa = function(data) {
  return(ifelse(data[, 1:5] > 0, data[, 1:5], NA))
}

read_L_DOPA = function(df) {
  measurement = read_excel(df,  range = "Z114:AD114",  col_names = paste0("rep", seq_along(1:5)))
  final = return_positive_ldopa(measurement)
  return(final)
}

name_change = function(x) {
   substr(x$sample, 10, nchar(x$sample) - 5)
}

enzyme_as_data_table = function(x, func) {
  df = as.data.table(t(mapply(x, FUN = func, USE.NAMES = TRUE)), keep.rownames = "sample")
  df$sample = name_change(df)
  return(df)
}

convert_to_numeric = function(x) {
  x[, `:=`(V1 = as.numeric(V1),
            V2 = as.numeric(V2),
            V3 = as.numeric(V3),
            V4 = as.numeric(V4),
            V5 = as.numeric(V5))]
}

calculate_median = function(x) {
  x$median = apply(x[, -1], 1, median, na.rm = TRUE)
  return(x)
}

calculate_mean = function(x) {
  x$median = apply(x[, -1], 1, mean, na.rm = TRUE, trim = 0.25)
  return(x)
}

correct_name = function(x) {
  x$sample = apply(x[, 1], 1, substr, start = 4, stop = 11)
  return(x)
}

calculate_xyl_gly = function(x) {
  data.table(sample = x$Gly$sample, xyl_gly = map2_dfr(x$Gly[, "median"], x$Xyl[, "median"], "sample",
                                       .f = ~.y / .x))
}

calculate_glu.xyl_cbh = function(x) {
  data.table(sample = x$Gly$sample, glu.xyl_cbh = map2_dfr(x$Gly[, "median"], x$Xyl[, "median"], x$Cbh[, "median"], "sample",
                                       .f = ~((..1 + ..2) / ..3)))
}

calculate_glu_pep = function(x) {
  data.table(sample = x$Gly$sample, glu_pep = map2_dfr(x$Gly[, "median"], x$Pep[, "median"], "sample",
                               .f = ~.x / .y))
}

calculate_pep_pho = function(x) {
  data.table(sample = x$Pep$sample, pep_pho = map2_dfr(x$Pep[, "median"], x$Pho[, "median"], "sample",
                               .f = ~.x / .y))
}

calculate_glu_nag = function(x) {
  data.table(sample = x$Gly$sample, glu_nag = map2_dfr(x$Gly[, "median"], x$NAG[,"median"], "sample",
                               .f = ~.x / .y))
}

calculate_glu_ldopa = function(x) {
  data.table(sample = x$Gly$sample, glu_ldopa = map2_dfr(x$Gly[,"median"], x$Ldopa[,"median"], "sample",
                               .f = ~.x / .y))
}

calculate_cbh_ldopa = function(x) {
  data.table(sample = x$Cbh$sample, cbh_ldopa = map2_dfr(x$Cbh[,"median"], x$Ldopa[,"median"], "sample",
                               .f = ~.x / .y))
}

calculate_nag_ldopa = function(x) {
  data.table(sample = x$NAG$sample, nag_ldopa = map2_dfr(x$NAG[,"median"], x$Ldopa[,"median"], "sample",
                               .f = ~.x / .y))
}

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

is.inf.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.infinite))
}