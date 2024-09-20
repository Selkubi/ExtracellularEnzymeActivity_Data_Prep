#' Calculate Enzyme Activity Ratios
#'
#' This function computes a variety of enzyme activity ratios from a given data set containing median values of different enzyme activities. It returns a data table with the calculated ratios for each sample, including:
#' - Xylanase/Glucuronidase (Xyl/Gly)
#' - (Glucuronidase + Xylanase) / Cellobiohydrolase (Gly + Xyl / Cbh)
#' - Glucuronidase/Peptidase (Gly/Pep)
#' - Peptidase/Phosphatase (Pep/Pho)
#' - Glucuronidase/N-Acetylglucosaminidase (Gly/NAG)
#' - Glucuronidase/L-DOPA (Gly/L-DOPA)
#' - Cellobiohydrolase/L-DOPA (Cbh/L-DOPA)
#' - N-Acetylglucosaminidase/L-DOPA (NAG/L-DOPA)
#'
#' @param x A list of data frames, where each data frame corresponds to an enzyme's measurements. Each data frame should contain a "sample" column for sample identifiers and a "median" column with median enzyme activity values for each sample. The enzyme data frames should include: Gly, Xyl, Cbh, Pep, Pho, NAG, and L-DOPA.
#'
#' @return A data table with the following calculated enzyme activity ratios for each sample:
#' - `xyl_gly`: Ratio of xylosidase  to glucosidase
#' - `glu.xyl_cbh`: Sum of glucosidase  and xylosidase , divided by cellobiohydrolase.
#' - `glu_pep`: Ratio of glucosidase  to leucine aminopeptidas.
#' - `pep_pho`: Ratio of leucine aminopeptidas to Phosphatase.
#' - `glu_nag`: Ratio of glucosidase  to N-Acetylglucosaminidase.
#' - `glu_ldopa`: Ratio of glucosidase to L-DOPA.
#' - `cbh_ldopa`: Ratio of Cellobiohydrolase to L-DOPA.
#' - `nag_ldopa`: Ratio of N-Acetylglucosaminidase to L-DOPA.
#'
#' @export
#'
#' @examples
#' # Assuming 'enzyme_data' is a list of data frames with median enzyme activity values
#' # enzyme_data <- list(
#' #   Gly = data.frame(sample = c("S1", "S2"), median = c(1.2, 0.8)),
#' #   Xyl = data.frame(sample = c("S1", "S2"), median = c(0.5, 0.6)),
#' #   Cbh = data.frame(sample = c("S1", "S2"), median = c(1.0, 0.9)),
#' #   Pep = data.frame(sample = c("S1", "S2"), median = c(0.3, 0.4)),
#' #   Pho = data.frame(sample = c("S1", "S2"), median = c(0.7, 0.8)),
#' #   NAG = data.frame(sample = c("S1", "S2"), median = c(0.9, 0.5)),
#' #   Ldopa = data.frame(sample = c("S1", "S2"), median = c(0.4, 0.3))
#' # )
#' # result <- calculate_enzyme_ratios(enzyme_data)
#' # print(result)
calculate_enzyme_ratios = function(x) {
  data.table(sample = x$Gly$sample, xyl_gly = map2_dfr(x$Gly[, "median"], x$Xyl[, "median"], "sample",  .f = ~.y / .x),
             glu.xyl_cbh = map2_dfr(x$Gly[, "median"], x$Xyl[, "median"], x$Cbh[, "median"], "sample", .f = ~((..1 + ..2) / ..3)),
             glu_pep = map2_dfr(x$Gly[, "median"], x$Pep[, "median"], "sample", .f = ~.x / .y),
             pep_pho = map2_dfr(x$Pep[, "median"], x$Pho[, "median"], "sample", .f = ~.x / .y),
             glu_nag = map2_dfr(x$Gly[, "median"], x$NAG[,"median"], "sample", .f = ~.x / .y),
             glu_ldopa = map2_dfr(x$Gly[, "median"], x$Ldopa[,"median"], "sample", .f = ~.x / .y),
             cbh_ldopa = map2_dfr(x$Cbh[,"median"], x$Ldopa[,"median"], "sample", .f = ~.x / .y),
             nag_ldopa = map2_dfr(x$NAG[,"median"], x$Ldopa[,"median"], "sample", .f = ~.x / .y))
}
