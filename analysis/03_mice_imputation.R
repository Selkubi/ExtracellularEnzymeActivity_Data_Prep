# this scripts follows the same strategy in 02_mixed model but does it within the mice impitation


# MICE imputation
imputed_data <- mice::mice(ER_data, m = 5, method = 'pmm', seed = 123)
summary(imputed_data)
completed_data_1 <- mice::complete(imputed_data, 1)
completed_datasets <- lapply(1:5, function(i) mice::complete(imputed_data, i))

models <- lapply(completed_datasets, function(x) {
  nlme::lme(fixed = glu.xyl_cbh.median ~ day * position,
            random = ~ 1 | chainID,
            data = x)
})


lapply(models[[1]], emmeans(day | position, data = ER_data))
emmeans::contrast(emmeans(models[[1]], ~ day | position, data = ER_data), method =  "trt.vs.ctrl", ref = "S09")

lp <- lapply(models, function(x){
  # Call experiment_lmer with each response variable
  fit <-  emmeans(x, ~ day | position, data = ER_data)
  contrast_reults <- contrast(fit, method =  "trt.vs.ctrl", ref = "S09")
  return((contrast_reults))
})

pool_models <- mice::pool(lp)


nlme::lme(fixed = glu.xyl_cbh.median ~ day * position,
          random = ~ 1 | chainID,
          data = completed_datasets[[1]])
