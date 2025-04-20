#' Perform Pairwise Comparisons of Estimated Marginal Means by Position and Day
#'
#' The `position_comparison` function performs pairwise comparisons of estimated
#' marginal means (EMMs) for different positions, grouped by day, using a
#' specified model. This function is useful for post-hoc analysis to determine
#' significant differences between positions within each day.
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
#' 1. Computes the estimated marginal means (EMMs) for the `position` factor,
#'    grouped by the `day` factor, using the `emmeans` function.
#' 2. Performs pairwise comparisons of the EMMs using the `contrast` function
#'    with Tukey's adjustment for multiple comparisons.
#' 3. Returns the results of the pairwise comparisons.
#'
#' @examples
#' # Assuming 'my_model' is a fitted model object
#' results <- position_comparison(my_model)
#' print(results)
#'
#' @import emmeans
#' @export
#'
#' @seealso
#' The `emmeans` function for computing estimated marginal means and the
#' `contrast` function for performing pairwise comparisons.
#'
position_comparison <- function(model) {
  posthoc_specific_day <- emmeans(model, specs = ~position | day)
  pairwise_comparisons <- contrast(posthoc_specific_day, method = "pairwise", adjust = "tukey")
  return(pairwise_comparisons)
}
