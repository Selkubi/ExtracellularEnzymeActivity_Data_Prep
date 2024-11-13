source("analysis/EEA.R")
library(lme4)

ER_data[, c("day", "chainID", "position") := tstrsplit(ER_data$sample, "_")]
ER_data[, "columnID" := c(1:nrow(ER_data))]
ER_data[,c("day")] <- as.factor(ER_data$day)
ER_data[,c("position")] <- as.factor(ER_data$position)
ER_data[,c("chainID")] <- as.factor(ER_data$chainID)
### Following ch6: Random and mixed effects models

# Check the model assumtions with the following plots
par(mfrow = c(2, 4))
lapply(ER_data[, 2:9], function(x) {
  stripchart(x ~ ER_data$chainID, vertical = TRUE, pch = 1, xlab = "chainID" , main = deparse(colnames(x)))
})

#LEts check the normality of the enzymes
shapiro_test_results <- sapply(log(ER_data[, -c(1,10:13)]), function(x) shapiro.test(x)$p.value)

# plot the histograms to visually check the normality
for (col in names(ER_data[, -c(1,10:13)])) {
  hist(ER_data[[col]], main = col, xlab = "Value")
}
par(mfrow = c(1, 1))

# The distribuitions looks fine with a couple point exceptions (and the Na values)
# To get the p value for the mixed effects etc, use the lmerTest package (which also uses lmer but adds some stats)
fm04 <- lmer(xyl_gly.median ~ day * position + (1 | chainID), data = ER_data)
summary(fm04)

model <- experiment_lmer("glu.xyl_cbh.median", "day", fixed_factor = "position", random_factor = "chainID", data = ER_data)
summary(model)

model <- lmer(glu.xyl_cbh.median ~ day*position + (1|chainID), data = ER_data)
summary(model)

fit_mle <- nlme::lme(fixed = glu.xyl_cbh.median ~ day * position,
                     random = ~ 1 | chainID,
                     data = ER_data,
                     na.action = na.omit)


emmeans::contrast((emmeans::emmeans(fit_mle, ~ day | position, data = ER_data)), method = "pairwise", adjust = "tukey")
emmeans::contrast((emmeans::emmeans(fit_mle, ~ day | position, data = ER_data)), method =  "trt.vs.ctrl", ref = "S09")

summary(fit_mle)
fm <-list()
# Apply experiment_lmer to columns 2 to 9 in ER_data
fm <- lapply(names(ER_data)[2:9], function(colname){
  # Call experiment_lmer with each response variable
  data_subset <- ER_data[!is.na(ER_data[[colname]]), ]

  fit <- experiment_lmer(x = colname,
                  y = "day",
                  fixed_factor = "position",
                  random_factor = "chainID",
                  data = data_subset)
  if (isSingular(fit)) {
    warning(paste("Singular fit detected for variable:", colname))
  }
  summary <- summary(fit)
  return(list(fit, summary))
})
names(fm) <- names(ER_data)[2:9]

# Get the day comparison for each column for each enzyme
posthoc_spesific_position <- list()
pairwise_comparisons <- list()
for (i in seq_along(fm)) {
  posthoc_spesific_position[[names(fm)[i]]] <- emmeans::emmeans(fm[[i]][[1]], ~ day | position)
  pairwise_comparisons[[names(fm)[i]]] <- emmeans::contrast(posthoc_spesific_position[[i]], method = "pairwise", adjust = "tukey")
}

# Get the day spesific comparisons where each day is compared only to S09
control_comparisons <- list()
for(i in seq_along(fm)){
  control_comparisons[[names(fm)[i]]] <- emmeans::contrast(posthoc_spesific_position[[i]], "trt.vs.ctrl", ref = "S09")
}

fit_bayes <- brms::brm(
  brms::bf(glu.xyl_cbh.median | mi() ~ day * position + (1 | chainID)),  # Define the model with missing indicator (mi)
  data = ER_data,
  family = gaussian(),
  chains = 4,              # Number of MCMC chains
  iter = 2000,             # Number of iterations per chain
  control = list(adapt_delta = 0.95)  # Control for convergence
)
imputed_values <- brms::posterior_predict(fit_bayes, newdata = ER_data, allow_new_levels = TRUE)
missing_rows <- is.na(ER_data$glu.xyl_cbh.median)
imputed_values_missing <- imputed_values[, missing_rows]
mean(imputed_values_missing[,2])
mean(imputed_values_missing[,1])

emmeans_bayes <- emmeans::emmeans(fit_bayes, ~ day | position, data = ER_data)
contrast_bayes <- emmeans::contrast(emmeans_bayes, method =  "trt.vs.ctrl", ref = "S09")


