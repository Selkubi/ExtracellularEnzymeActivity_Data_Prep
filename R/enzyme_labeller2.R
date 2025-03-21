#' Enzyme Ratio Names and Labeller for the log ratios
#'
#' These functions handle enzyme ratio names and label them in a format suitable for `ggplot2` visualizations.
#' The `enzyme_ratio_names` list defines a set of common enzyme ratios and their corresponding display names.
#' The `enzyme_labeller` function retrieves the appropriate label for each enzyme ratio based on its value, which can be used for customizing plots.
#'
#' @return
#' - `enzyme_ratio_names`: A named list of enzyme ratio abbreviations and their corresponding descriptive names.
#' - `enzyme_labeller`: A function that returns the appropriate enzyme ratio label for a given value.
#' @export
#'
#' @examples
#' # Accessing enzyme ratio names
#' enzyme_ratio_names
#'
#' # Using enzyme_labeller with ggplot2
#' enzyme_labeller(variable = NULL, value = "glu_pep.median")
#' # Custom labeling in ggplot2
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   facet_wrap(~gear, labeller = enzyme_labeller)
enzyme_ratio_names2 <- list(
  "ratio_xyl_gly.median"  = "Xyl / Glu",
  "ratio_glu.xyl_cbh.median" = "Glu + Xyl / \n Cbh",
  "ratio_glu_pep.median" =  "Glu / Pep",
  "ratio_pep_pho.median" = "Pep / Pho",
  "ratio_glu_nag.median" = "Glu / NAG",
  "ratio_glu_ldopa.median" = "Glu /\n L-DOPA",
  "ratio_cbh_ldopa.median" = "Cbh /\n L-DOPA",
  "ratio_nag_ldopa.median" = "NAG /\n L-DOPA"
)

enzyme_labeller2 <- function(variable, value) {
  return(enzyme_ratio_names2[value])
}
