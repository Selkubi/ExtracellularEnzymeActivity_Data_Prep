library(modelsummary)

# Run all the other scripts beforehand before running this.

# Can do it with modelsummary
modelsummary((mice::pool(models_time_comparison$glu.xyl_cbh.median)))
#which can broom the mice models and compare them as well
modelsummary(models_time_comparison$glu.xyl_cbh.median, modelsummary_get = "broom")


# Can also do it with stargazer
stargazer::stargazer(models_time_comparison$glu.xyl_cbh.median[[1]], models_time_comparison$xyl_gly.median[[1]],
          type = "text", single.row = TRUE )


# List of models of enzyme ratios to print the statistics of
models <- list("Cbh / L-DOPA" = mice::pool(models_time_comparison$cbh_ldopa.median),
               "Glu + Xyl / Cbh" = mice::pool(models_time_comparison$glu.xyl_cbh.median),
               "Glu / L-DOPA" = mice::pool(models_time_comparison$glu_ldopa.median),
               "Glu / NAG" = mice::pool(models_time_comparison$glu_nag.median),
               "Glu / Pep" = mice::pool(models_time_comparison$glu_pep.median),
               "Pep / Pho" = mice::pool(models_time_comparison$pep_pho.median),
               "Xyl / Glu" = mice::pool(models_time_comparison$xyl_gly.median),
               "NAG / L-DOPA" = mice::pool(models_time_comparison$nag_ldopa.median))


# Print the statistics of the models
modelsummary(models,
             statistic = "({std.error})",
             stars = TRUE,
             fmt = fmt_decimal(digits = 1),
             gof_map = c("nobs"),
             output = "output/model_summary.csv")
