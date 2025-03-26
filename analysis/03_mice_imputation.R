# this scripts follows the same strategy in 02_mixed model but does it within the mice impitation


# MICE imputation
imputed_data <- mice::mice(ER_data, m = 5, method = 'pmm', seed = 123)
summary(imputed_data)
completed_data_1 <- mice::complete(imputed_data, 1)
completed_datasets <- lapply(1:5, function(i) mice::complete(imputed_data, i))

models <- lapply(completed_datasets, function(x) {
  experiment_lmer("xyl_gly.median", "day", fixed_factor = "position", random_factor = "chainID", data = ER_data)
})


lp <- lapply(models, time_comparison)


pool_models <- mice::pool(lp)
