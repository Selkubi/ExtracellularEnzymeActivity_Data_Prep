source("analysis/01_EEA_data_import.R")
#library(lme4)
library(emmeans)
library(lmerTest)

EEA_data[, c("day", "chainID", "position") := tstrsplit(EEA_data$sample, "_")]
EEA_data[, c("day")] <- as.factor(EEA_data$day)
EEA_data[, "columnID" := c(1:nrow(EEA_data))]
EEA_data[, c("position")] <- as.factor(EEA_data$position)
EEA_data[, c("chainID")] <- as.factor(EEA_data$chainID)

# Merge the two together
data_all <- merge(EEA_data, Sample_biomass, all.x = FALSE,
                  by.x = c("chainID", "day", "position"),
                  by.y = c("replicate", "Sample_date", "col_no"))

# Calculate biomass normalised EEA

data_all[, log_cell_pro_ml := (log(Cell_pro_ml))]
data_all[, plotting_cell_pro_ml := (Cell_pro_ml)/10^8]
data_all[, log_normalized_Cbh := (Cbh / log_cell_pro_ml)]

enzyme_cols <- c("Cbh", "Gly", "Ldopa", "NAG", "Pep", "Pho", "Xyl")
data_all[, paste0("log_normalized_", enzyme_cols) := lapply(.SD, function(x) x / log_cell_pro_ml),
         .SDcols = enzyme_cols]
data_all[, paste0("plotting_normalized_", enzyme_cols) := lapply(.SD, function(x) x / plotting_cell_pro_ml),
         .SDcols = enzyme_cols]
data_all <- data_all[-45, ] #quick remove the replicated sample
EEA_data <- data_all[,c("chainID", "day", "position", "sample",
                        "log_normalized_Cbh", "log_normalized_Gly", "log_normalized_Ldopa",
                        "log_normalized_NAG", "log_normalized_Pep", "log_normalized_Pho", "log_normalized_Xyl")]

### Following ch6: Random and mixed effects models
# Check the model assumtions with the following plots
lapply(EEA_data[, 5:11], function(x) {
  stripchart(x ~ EEA_data$chainID, vertical = TRUE, pch = 1, xlab = "chainID" , main = deparse(colnames(x)))
})

stripchart(log_normalized_Gly  ~ chainID, vertical = TRUE, pch = 1, xlab = "chainID", data = EEA_data)

#LEts check the normality of the enzymes
shapiro_test_results <- sapply(log(EEA_data[, -c(1:4)]), function(x) shapiro.test(x)$p.value)

# plot the histograms to visually check the normality
par(mfrow = c(2, 4))
for (col in names(EEA_data[, -c(1:4)])) {
  hist(log(EEA_data[[col]]), main = col, xlab = "Value")
}
par(mfrow = c(1, 1))

# The distribuitions looks fine with a couple point exceptions (and the Na values)
# To get the p value for the mixed effects etc, use the lmerTest package (which also uses lmer but adds some stats)
fm04 <- experiment_lmer("log_normalized_Pep", "day", fixed_factor = "position", random_factor = "chainID",
                        data = EEA_data)
summary(fm04)
pairwise_comparisons <- time_comparison(fm04, EEA_data)

model1 <- nlme::lme(log_normalized_Pep ~ day * position, random = ~  1 | chainID, data = completed_datasets[[5]])
summary(model1)
anova(model1)
confint(model1)

model2 <- lm(glu_nag.median ~ day * position,  data = completed_datasets[[5]])
summary(model2)
AIC(model1, model2)
anova(model1, model2)
RLRsim::exactRLRT(model1)

model3 <- experiment_lmer("glu.xyl_cbh.median", "day", fixed_factor = "position", random_factor = "chainID",
                          data = completed_datasets[[5]])


#contrast for days
posthoc_spesific_position <- emmeans(model1, ~ day | position)
pairwise_comparisons <- contrast(posthoc_spesific_position, method = "pairwise", adjust = "tukey")


# Plotting
fitdata_gmvolume = as.data.frame(effects::Effect(c("day", "position"),model1))

ggplot(data = ER_data, aes(y=xyl_gly.median, x=position, color=day, group=day, fill=day)) +
  geom_point() +
  geom_line(data = fitdata_gmvolume, aes(y=fit)) +
  geom_ribbon(data = fitdata_gmvolume, aes(y=fit, ymin=lower, ymax=upper), alpha=0.4) +
  xlab("Position") +
  ylab(bquote('xyl_gly.median'~(mm^3))) +
  labs(fill = "Day", group="Day", color="Day")
