source("R/EEA.R")
library(lme4)
library(ggplot2)

ER_data[, c("day", "chainID", "position") := tstrsplit(ER_data$sample, "_")]
ER_data[, "columnID" := c(1:nrow(ER_data))]
ER_data[,c("day")] <- as.factor(ER_data$day)
ER_data[,c("position")] <- as.factor(ER_data$position)
ER_data[,c("chainID")] <- as.factor(ER_data$chainID)


fm <- lmer(xyl_gly.median ~  as.factor(day) + (1 | chainID), data = ER_data)
summary(fm)

ER_data <- within(ER_data, position_time <- factor(position:day))

fm02 <- lmer(xyl_gly.median ~  day + (1 | chainID) + (1 | position)+ (1 | position_time),
             data = ER_data, REML = FALSE) 
summary(fm02)
car::Anova(fm02)

fm03 <- lmer(xyl_gly.median ~  day * position + (1 | chainID), data = ER_data) 

summary(fm03)
anova(fm03)
plot(fm03,  which = c(1), col = 1, add.smooth = FALSE)
plot(ER_data$day, resid(fm03))
plot(ER_data$position, resid(fm03))
plot(ER_data$chainID, resid(fm03))

ER_data$fit_fm03 <- predict(fm03)

F0 <- fitted(fm03, level = 0)
F1 <- fitted(fm03, level = 1)
C0 <- coefficients(fm03)

I <- order(ER_data$position)
position <- sort(ER_data$position)

plot(position, F0[I], lwd = 4, type = "l")
ggplot()+
  geom_point(aes(x=ER_data$position, y=F0))

### Following ch6: Random and mixed effects models

# Check the model assumtions with the following plots 
plot(fm03) # TA plot

# q-q plots
par(mfrow = c(1, 2))
qqnorm(ranef(fm03)$chainID[,"(Intercept)"], 
       main = "Random effects")
qqnorm(resid(fm03), main = "Residuals")
par(mfrow = c(1, 1))

with(ER_data, interaction.plot(x.factor = position, 
                               trace.factor = chainID, 
                               response = xyl_gly.median))
ggplot(ER_data, aes(x = position, y = xyl_gly.median, group = chainID, col = chainID)) + 
  geom_point() + stat_summary(fun = mean, geom = "line") + theme_bw()

# To get the p value for the mixed effects etc, use the lmerTest package (which also uses lmer but adds some stats)
fm04 <- lmerTest::lmer(xyl_gly.median ~  day * position + (1 | chainID), data = ER_data)
summary(fm04)
anova(fm04)
fixef(fm04)
