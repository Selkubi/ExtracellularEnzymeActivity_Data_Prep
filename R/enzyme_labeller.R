#' Enzyme Ratio Names and Labeller
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
enzyme_ratio_names <- list(
  "xyl_gly.median" = "Xyl / Glu",
  "glu.xyl_cbh.median" = "Glu + Xyl / Cbh",
  "glu_pep.median" =  "Glu / Pep",
  "pep_pho.median" = "Pep / Pho",
  "glu_nag.median" = "Glu / NAG",
  "glu_ldopa.median" = "Glu / L-DOPA",
  "cbh_ldopa.median" = "Cbh / L-DOPA",
  "nag_ldopa.median" = "NAG / L-DOPA"
)

enzyme_labeller <- function(variable, value) {
  return(enzyme_ratio_names[value])
}
