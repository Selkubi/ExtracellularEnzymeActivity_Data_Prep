#' Tidy Pooled Model from MICE Imputation
#'
#' This function takes a list of linear mixed models generated from mice imputation and returns a tidied and pooled model.
#'
#' @param lmm A list of linear mixed models (lmm) generated from mice imputation to be tidied and pooled.
#'
#' @return A tibble containing the pooled estimates of the fixed effects from the models.
#'   The tibble includes the following columns:
#'   \itemize{
#'     \item \code{term}: The term or predictor in the model.
#'     \item \code{estimate}: The mean estimate of the term across all models.
#'     \item \code{std.error}: The mean standard error of the term across all models.
#'     \item \code{statistic}: The mean statistic (e.g., t-value) of the term across all models.
#'     \item \code{p.value}: The mean p-value of the term across all models.
#'   }
#'
#' @examples
#' # Example usage:
#' # Assuming `imputed_models` is a list of linear mixed models from mice imputation
#' pooled_results <- tidy_pooled_model(imputed_models)
#'
#' @export
#'
#' @importFrom broom.mixed tidy
#' @importFrom dplyr bind_rows group_by summarise
tidy_pooled_model <- function(lmm){
  tidied_models <- lapply(lmm, function(model){
    broom.mixed::tidy(model, effects = "fixed")[, c("term", "estimate", "std.error", "statistic", "p.value")]
  })

  pooled_model <- dplyr::bind_rows(tidied_models, .id = "model_id") |>
    dplyr::group_by(term) |>
    dplyr::summarise(estimate = mean(estimate),
                     std.error = mean(std.error),
                     statistic = mean(statistic),
                     p.value = mean(p.value))

  return(pooled_model)
}

