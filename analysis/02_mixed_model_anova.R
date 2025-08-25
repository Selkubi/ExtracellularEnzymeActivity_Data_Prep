source("analysis/01_EEA_data_import.R")
#library(lme4)
library(emmeans)
library(lmerTest)

ER_data[, c("day", "chainID", "position") := tstrsplit(ER_data$sample, "_")]
ER_data[, c("day")] <- as.factor(ER_data$day)
ER_data[, "columnID" := c(1:nrow(ER_data))]
ER_data[, c("position")] <- as.factor(ER_data$position)
ER_data[, c("chainID")] <- as.factor(ER_data$chainID)
### Following ch6: Random and mixed effects models

# Check the model assumtions with the following plots
lapply(ER_data[, 2:9], function(x) {
  stripchart(x ~ ER_data$chainID, vertical = TRUE, pch = 1, xlab = "chainID" , main = deparse(colnames(x)))
})

stripchart(xyl_gly.median ~ chainID, vertical = TRUE, pch = 1, xlab = "chainID", data = ER_data)

#LEts check the normality of the enzymes
shapiro_test_results <- sapply(log(ER_data[, -c(1,10:13)]), function(x) shapiro.test(x)$p.value)

# plot the histograms to visually check the normality
par(mfrow = c(2, 4))
for (col in names(ER_data[, -c(1,10:13)])) {
  hist(ER_data[[col]], main = col, xlab = "Value")
}
par(mfrow = c(1, 1))

# The distribuitions looks fine with a couple point exceptions (and the Na values)
# To get the p value for the mixed effects etc, use the lmerTest package (which also uses lmer but adds some stats)
fm04 <- experiment_lmer("xyl_gly.median", "day", fixed_factor = "position", random_factor = "chainID",
                        data = ER_data)
summary(fm04)
pairwise_comparisons <- time_comparison(fm04, ER_data)

model1 <- nlme::lme(glu_nag.median ~ day * position, random = ~  1 | chainID, data = completed_datasets[[5]])
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
