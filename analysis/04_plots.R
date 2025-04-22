library(data.table)
library(ggplot2)
library(dplyr)

#source("R/functions.R")
#source("R/plotting_functions.R")
source("analysis/01_EEA_Data_import.R") # Read the enzyme results, calculate the means/medians and enzyme ratios

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

# Enzyme Ratio plots with log ratio of day0 to other days

summarized_data  <- ER_data |>
  group_by(sample_date, col_no) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

sample_date_0  <- summarized_data |>
  ungroup() |>
  filter(sample_date == '0') |>
  select(-sample_date)

date_0_ratios <- ER_data |>
  mutate(across(where(is.numeric), ~ . / sample_date_0[[cur_column()]][match(col_no, sample_date_0$col_no)], .names = "ratio_{col}")) |>
  ungroup() |>
  select(sample_date, col_no, replicate, starts_with("ratio_"))

# After checking if the day0 values are all equals to 1, convert to a melted table
data <-  date_0_ratios |>
  tidyr::pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "variable",
    values_to = "value"
  ) |>
  filter(sample_date != 0) |>
  mutate(log_ratio = log(value))

ribbon_info <- data |>
  group_by(variable, col_no, sample_date) |>
  mutate(max = max(log_ratio),
         min = min(log_ratio),
         median = median(log_ratio),
         mean = mean(log_ratio))

log_ratio_boxplots <- ggplot(data = ribbon_info, mapping = aes(x = sample_date, y = log_ratio, group = col_no, color = col_no, shape = col_no)) +
  facet_wrap(~variable, nrow = 4, scale = "free", labeller = labeller(variable = enzyme_labeller2),
                                                                      strip.position = "left", axes = "all", axis.labels = "all_y") +
  geom_line(aes(y = 0), colour = "#999", linewidth = 1, linetype = 1) +
  geom_point(size = 1.5, show.legend = FALSE) +
  geom_line(mapping = aes(y = mean), linewidth = 1.2) +
  theme_boxplot() + xlab("Days") + ylab("log ratio of dayX:day0") +
  color_column() +
  theme(legend.position = "bottom")

pdf('output/plots/log_ratio_enzyme_ratios.pdf', width = 5, height = 8)
plot(log_ratio_boxplots)
dev.off()

# Plot: Replicate chains as lines

enzme_ratios <- ggplot(data, aes(x = col_no, y = log_ratio, color = sample_date, group = sample_date)) +
  facet_wrap(~variable, scales = "free", nrow = 4, labeller = labeller(variable = enzyme_labeller),
             strip.position = "left", axes = "all", axis.labels = "all_y") +
  geom_line(aes(y = 0), colour = "#333", linewidth = 1, linetype = 1) +
  geom_point(size = 1.5) +
  geom_line(data = ribbon_info, aes(y = mean), linewidth = 2) +
  color_sample_date_no_zero() + fill_sample_date_no_zero() +
  scale_x_discrete(expand = c(0.05, 0.05), labels = column_labeller()) +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  theme_boxplot()

pdf('output/plots/enzyme_ratios.pdf', width = 5, height = 8)
plot(enzme_ratios)
dev.off()
