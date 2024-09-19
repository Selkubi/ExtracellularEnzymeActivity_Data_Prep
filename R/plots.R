library(data.table)
library(ggplot2)

source("R/functions.R")
source("R/plotting_functions.R")
source("R/EEA.R") # Read the enzyme results, calculate the means/medians and enzyme ratios

if(!dir.exists("output/plots")) {dir.create("output/plots")}

# Set the factor names for plotting
ER_data[, c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
ER_data$sample_date <- factor(ER_data$sample_date,
                           levels = c("S09", "S13", "S16", "S19"),
                           labels = c("0", "03", "10", "17")) #Convert the sample date to days as a factor for ease in plotting
ER_data$col_no <-factor(ER_data$col_no,
                                 levels = c("C1", "C2", "C3"),
                                 labels = c("Col1", "Col2", "Col3"))
melted_ER <- melt(ER_data, id.vars = c("sample", "sample_date", "col_no", "replicate"))

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
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Pep") + xlab ("Days")

#L-DOPA median
ER_data <- ER_data[calculate_median(Ldopa)[, c(1, 7)], on = .(sample = sample)]

ggplot(ER_data, mapping = aes(x = sample_date, y = median)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("L-DOPA") + xlab ("Days")

# Enzyme ratio facet
melted_ER <- set_coloring_column(melted_ER)

# Overview of all the enzyme ratios
ggplot(melted_ER) +
  facet_grid(variable ~ col_no, scale = "free", labeller = labeller(variable = enzyme_labeller, col_no = column_labeller)) +
  geom_boxplot(data = starter_community_C1, aes(starter, value, fill = highlight, color = highlight), width = 0.5) +
  geom_boxplot(data = starter_community_C2, aes(starter, value, fill = highlight, color = highlight), width = 0.5) +
  geom_boxplot(data = starter_community_C3, aes(starter, value, fill = highlight, color = highlight), width = 0.5) +
  geom_boxplot(aes(x = as.factor(sample_date), y = (value), fill = highlight, color = highlight), width = 0.5) +
  fill_col_no + color_col_no +
  theme_boxplot +
  geom_hline(yintercept = c(0), color = "red", linetype = "dashed") + xlab ("Day") + ylab (NULL)

starter_community_C1 <- melted_ER[sample_date =="0" & col_no =="Col3"]
starter_community_C1$starter <- as.factor("S")
starter_community_C1$col_no <- "Col1"

starter_community_C2 <- melted_ER[sample_date =="0" & col_no =="Col2"]
starter_community_C2$starter <-  as.factor("S")
starter_community_C2$col_no <- "Col2"

starter_community_C3 <- melted_ER[sample_date =="0" & col_no =="Col1"]
starter_community_C3$starter <-  as.factor("S")
starter_community_C3$col_no <- "Col3"


# Plot: Replicate chains as lines
data <- melted_ER
ribbon_info <- data[,.(max = max(value), min = min(value), mean = mean(value), median = median(value)), by = .(sample_date, col_no, variable)]

enzme_ratios <- ggplot(data, aes(x = col_no, y = value)) +
  facet_wrap(~variable, scales = "free_y", nrow = 4, labeller = labeller(variable = enzyme_labeller), strip.position = "left") +
  geom_ribbon(data = ribbon_info, aes(x = col_no, y = mean, group = sample_date,
                  ymin = min, 
                  ymax = max, fill = sample_date), alpha = 0.01) +
  geom_point(aes(color = sample_date, group = replicate), size = 0.5) + 
  geom_line(aes(group = replicate, color = sample_date), alpha = 0.5) +
  geom_line(data = ribbon_info, aes(x = col_no, y = median, group = sample_date, color = sample_date), linewidth = 2) + 
  fill_sample_date() + color_sample_date() +
  scale_x_discrete(expand = c(0.05, 0.05), labels = column_labeller) +
  scale_y_continuous(breaks = scales::pretty_breaks(7)) +
  theme_boxplot()

pdf('output/plots/enzyme_ratios.pdf', width = 5, height = 10)
plot(enzme_ratios)
dev.off()
              
              