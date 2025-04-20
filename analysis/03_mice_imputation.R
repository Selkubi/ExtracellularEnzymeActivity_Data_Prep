# this scripts follows the same strategy in 02_mixed model but does it within the mice imputation
# MICE imputation
imputed_data <- mice::mice(ER_data, m = 5, method = 'pmm', seed = 123)
summary(imputed_data)
completed_datasets <- lapply(1:5, function(i) mice::complete(imputed_data, i))

models <- lapply(X = completed_datasets, FUN = experiment_lmer, response_col = "glu.xyl_cbh.median", y = "day", fixed_factor = "position", random_factor = "chainID")
lp <- lapply(models, time_comparison)

pooled_results <- all_p_value_calculations(object = lp)

# string of enzyme ratios that we will apply the above LMM
enzyme_ratios <- c("xyl_gly.median", "glu.xyl_cbh.median", "glu_pep.median", "pep_pho.median",
                   "glu_nag.median", "glu_ldopa.median", "cbh_ldopa.median", "nag_ldopa.median")


## TIME COMPARISON
# create the list necessary to save all the results
models_time_comparison <- list()
pooled_results_time_comparison <- list()

# run the loop for all the enzyme ratios that we want to model for the paper
for(i in seq_along(enzyme_ratios)){

        # apply the model to all the imputed datasets calcualted above
        # for those that do not miss a data, the vectors in the imputed datasets are the same
        models_time_comparison[[i]] <- lapply(X = completed_datasets, FUN = experiment_lmer, response_col = enzyme_ratios[[i]],
                                              y = "day", fixed_factor = "position", random_factor = "chainID")

        # Apply the time contrasts
        lp <- lapply(models_time_comparison[[i]], time_comparison)

        # pool the results by taking the average.
        # This is ok for our case since the results are too close to each other and we did not impute many missing values
        # For more info, check paper by David Disabato
        pooled_results_time_comparison[[enzyme_ratios[[i]]]] <- all_p_value_calculations(object = lp)
}


## POSITION COMPARISON
# create the list necessary to save all the results
models_position_comparison <- list()
pooled_results_position_comparison <- list()


# run the loop for all the enzyme ratios that we want to model for the paper
for(i in seq_along(enzyme_ratios)){

  # apply the model to all the imputed datasets calcualted above
  # for those that do not miss a data, the vectors in the imputed datasets are the same
  models_position_comparison[[i]] <- lapply(X = completed_datasets, FUN = experiment_lmer, response_col = enzyme_ratios[[i]],
                                        y = "day", fixed_factor = "position", random_factor = "chainID")

  # Apply the time contrasts
  lp <- lapply(models_position_comparison[[i]], position_comparison)

  # pool the results by taking the average.
  # This is ok for our case since the results are too close to each other and we did not impute many missing values
  # For more info, check paper by David Disabato
  pooled_results_position_comparison[[enzyme_ratios[[i]]]] <- all_p_value_calculations(object = lp)
}
