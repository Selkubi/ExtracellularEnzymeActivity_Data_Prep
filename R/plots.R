library(data.table)
library(ggplot2)

source("R/functions.R")
source("R/plotting_functions.R")
source("R/EEA.R") # Read the enzyme results, calculate the means/medians and enzyme ratios

# Set the factor names for plotting
ER_data[, c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
ER_data$sample_date <- factor(ER_data$sample_date,
                           levels = c("S09", "S13", "S16", "S19"),
                           labels = c("0", "3", "10", "17")) #Convert the sample date to days as a factor for ease in plotting
ER_data$col_no <-factor(ER_data$col_no,
                                 levels = c("C1", "C2", "C3"),
                                 labels = c("Col1", "Col2", "Col3"))

melted_ER <- melt(ER_data, id.vars = c("sample", "sample_date", "col_no", "replicate"))

# Overview of all the enzyme ratios
ggplot(melted_ER) +
  facet_wrap(~variable, scale = "free", nrow = 2) +
  geom_boxplot(aes(x = as.factor(sample_date), y = (value), fill = as.factor(col_no))) +
  color_selected + fill_selected + theme_boxplot +
  geom_hline(yintercept = c(0), color = "red", linetype = "dashed")

# Individual enzyme ratio boxplots
ER_data <- set_coloring_column(ER_data)

# Xyloside/Glucosidase enzyme ratio
ggplot(ER_data, mapping = aes(x = sample_date, y = xyl_gly.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Xyl/Glu") + xlab ("Days")

#glu.xyl_cbh.median
ggplot(ER_data, mapping = aes(x = sample_date, y = glu.xyl_cbh.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Glu+Xyl/Cbh") + xlab ("Days")

#glu_pep.median
ggplot(ER_data, mapping = aes(x = sample_date, y = glu_pep.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Glu/Pep") + xlab ("Days")

#pep_pho.median
ggplot(ER_data, mapping = aes(x = sample_date, y = pep_pho.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Pep/Phosph") + xlab ("Days")

#glu_nag.median
ggplot(ER_data, mapping = aes(x = sample_date, y = glu_nag.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Glu/Nag") + xlab ("Days")

#glu_ldopa.median
ggplot(ER_data, mapping = aes(x = sample_date, y = glu_ldopa.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Glu/L-DOPA") + xlab ("Days")

#cbh_ldopa.median
ggplot(ER_data, mapping = aes(x = sample_date, y = cbh_ldopa.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Cbh/L-DOPA") + xlab ("Days")

#nag_ldopa.median
ggplot(ER_data, mapping = aes(x = sample_date, y = nag_ldopa.median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Nag/L-DOPA") + xlab ("Days")

#Leucine aminopepdinase median
ER_data <- ER_data[calculate_median(Pep)[, c(1, 7)], on = .(sample = sample)]

ggplot(ER_data, mapping = aes(x = sample_date, y = median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no() + color_col_no() +
  theme_boxplot() + observation_numbers() +
  ylab("Pep") + xlab ("Days")

#L-DOPA median
ER_data <- ER_data[calculate_median(Ldopa)[, c(1, 7)], on = .(sample = sample)]

ggplot(ER_data, mapping = aes(x = sample_date, y = median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers() +
  ylab("L-DOPA") + xlab ("Days")


