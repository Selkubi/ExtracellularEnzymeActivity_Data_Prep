#' Extract Significant Pairwise Comparisons from an emmeans Object
#'
#' This function filters pairwise comparison results from an `emmeans` object to return only
#' the comparisons with a p-value less than 0.05, indicating statistical significance.
#'
#' @param emmeans_object An object of class `emmGrid` returned by the `emmeans` function
#'                       with pairwise contrasts. Typically, this object is obtained
#'                       by using the `emmeans` function with the `pairwise` option.
#'
#' @return A `data.table` containing only the rows where the p-value is less than 0.05.
#'         The returned table includes the significant comparisons and their associated p-values.
#'
#' @details This function converts the provided `emmeans` object to a `data.table`
#'          and filters for rows where `p.value < 0.05`. This is useful for identifying
#'          statistically significant differences in pairwise comparisons.
#'
#' @examples
#' # Assuming `model` is a fitted model (e.g., from `lmer()`),
#' # and `emmeans_object` contains pairwise comparisons:
#' library(emmeans)
#' model <- lmer(response ~ fixed_effects + (1 | random_effect), data = your_data)
#' emmeans_object <- emmeans(model, pairwise ~ your_factor)$contrasts
#'
#' # Extract significant pairs
#' significant_pairs <- significant_p_value_extractor(emmeans_object)
#'
#' @export
significant_p_value_extractor <- function(emmeans_object){
  significant_pairs <- as.data.table(emmeans_object)[p.value < 0.05, ]
  return(significant_pairs)
}
