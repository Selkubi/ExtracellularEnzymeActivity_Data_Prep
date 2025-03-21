#' Fit a Linear Mixed-Effects Model with Flexible Input
#'
#' This function fits a linear mixed-effects model using the `lmer` function from the `lme4` package. It allows for flexible specification of the response variable, fixed effect, random effect, and interaction terms. The formula is dynamically generated based on the provided inputs.
#'
#' @param x A numeric vector or column from the data representing the response variable (e.g., `xyl_gly.median`).
#' @param y A factor or numeric column from the data representing the main fixed effect (e.g., `day`).
#' @param fixed_factor A factor or numeric column representing an additional fixed effect for interaction (e.g., `position`).
#' @param random_factor A factor or grouping variable representing the random effect (e.g., `chainID`).
#' @param data A data frame or data.table containing the variables in the model.
#'
#' @return A fitted `lmerMod` object, which contains the results of the linear mixed-effects model.
#' @details
#' The function constructs a formula dynamically in the form of `response ~ y * fixed_factor + (1 | random_factor)`, where the interaction between `y` and `fixed_factor` is included in the fixed effects, and the random effect is specified with `random_factor`.
#'
#' @examples
#' # Assuming you have a data.table called ER_data with columns 'xyl_gly.median', 'day', 'position', and 'chainID':
#' fit <- experiment_lmer(xyl_gly.median, day, fixed_factor = position, random_factor = chainID, data = ER_data)
#' summary(fit)
#'
#' @import lme4
#' @export
experiment_lmer <- function(response_col, y, fixed_factor, random_factor, data) {
  # Construct the formula dynamically using the column name
  formula <- as.formula(paste(response_col, "~", y, "*", fixed_factor, "+ (1 |", random_factor, ")"))

  # Fit the model using lmer
  lmer(formula, data = data)
}
