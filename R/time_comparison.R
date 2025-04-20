#' Perform Pairwise Comparisons of Estimated Marginal Means by Day and Position
#'
#' The `time_comparison` function performs pairwise comparisons of estimated
#' marginal means (EMMs) for different days, grouped by position, using a
#' specified model. This function is useful for post-hoc analysis to determine
#' significant differences between days within each position.
#'
#' @param model An object of class `lmerMod` or any other model object that is
#'   compatible with the `emmeans` function. This model should include `day`
#'   and `position` as factors.
#'
#' @return A data frame containing the results of the pairwise comparisons,
#'   including the estimated differences, standard errors, test statistics,
#'   p-values, and confidence intervals. The comparisons are adjusted using
#'   Tukey's method to control for multiple comparisons.
#'
#' @details
#' The function performs the following steps:
#' 1. Computes the estimated marginal means (EMMs) for the `day` factor,
#'    grouped by the `position` factor, using the `emmeans` function.
#' 2. Performs pairwise comparisons of the EMMs using the `contrast` function
#'    with Tukey's adjustment for multiple comparisons.
#' 3. Returns the results of the pairwise comparisons.
#'
#' @examples
#' # Assuming 'my_model' is a fitted model object
#' results <- time_comparison(my_model)
#' print(results)
#'
#' @import emmeans
#' @export
#'
#' @seealso
#' The `emmeans` function for computing estimated marginal means and the
#' `contrast` function for performing pairwise comparisons.
#'
time_comparison <- function(model) {
  posthoc_spesific_position <- emmeans(model, specs = ~day | position)
  pairwise_comparisons <- contrast(posthoc_spesific_position, method = "pairwise", adjust = "tukey")
  return(pairwise_comparisons)
}
