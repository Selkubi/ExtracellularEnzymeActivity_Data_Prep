# this scripts follows the same strategy in 02_mixed model but does it within the mice imputation
# MICE imputation
imputed_data <- mice::mice(ER_data, m = 5, method = 'pmm', seed = 123)
summary(imputed_data)
completed_datasets <- lapply(1:5, function(i) mice::complete(imputed_data, i))

models <- lapply(X = completed_datasets, FUN = experiment_lmer, response_col = "glu.xyl_cbh.median", y = "day", fixed_factor = "position", random_factor = "chainID")
lp <- lapply(models, time_comparison)

p_values_list <- list()

# Extract p-values from each emmGrid object
for (i in seq_along(lp)) {
  p_values_list[[i]] <- summary(lp[[i]])$p.value

}

# Convert the list of p-values to a matrix
p_values_matrix <- do.call(rbind, p_values_list)

# Compute the average p-value for each pairwise comparison
average_p_values <- apply(p_values_matrix, 2, mean)

# Extract contrasts and positions from the first emmGrid object
contrasts <- lapply(lp, function(x) levels(x)$contrast) |> unlist() |> unique()
positions <- lapply(lp, function(x) levels(x)$position) |> unlist() |> unique()

# Create a data frame to store the results
results <- data.frame(
  contrast = rep(contrasts, length(positions)),
  position = rep(positions, each = length(contrasts)),
  average_p_value = rep(average_p_values, each = length(positions))
)

# Print the results
print(results)
# this only pools, disregarding the contrasts
pool_models <- mice::pool(fit)

# Initialize containers for the results
estimates <- list()
standard_errors <- list()

# Extract estimates and standard errors from each emmGrid object
for (i in seq_along(lp)) {
  estimates[[i]] <- lp[[i]][emmean]
  standard_errors[[i]] <- lp[[i]]$SE
}
