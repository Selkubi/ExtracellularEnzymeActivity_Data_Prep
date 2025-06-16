library(data.table)
library(ggplot2)
library(dplyr)

#source("R/functions.R")
#source("R/plotting_functions.R")
#source("analysis/01_EEA_Data_import.R") # Read the enzyme results, calculate the means/medians and enzyme ratios

if(!dir.exists("output/plots")) {dir.create("output/plots")}

# Set the factor names for plotting
ER_data[, c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
ER_data$sample_date <- factor(ER_data$sample_date,
                           levels = c("S09", "S13", "S16", "S19"),
                           labels = c("0", "03", "10", "17")) #Convert the sample date to days as a factor for ease in plotting
ER_data$col_no <-factor(ER_data$col_no,
                                 levels = c("C1", "C2", "C3"),
                                 labels = c("Col1", "Col2", "Col3"))
melted_ER <- melt(ER_data[,-c("day", "chainID", "position", "columnID")], id.vars = c("sample", "sample_date", "col_no", "replicate"))

# Enzyme Ratio plots with log ratio of day0 to other days

summarized_data  <- ER_data |>
  group_by(sample_date, col_no) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

sample_date_0  <- summarized_data |>
  ungroup() |>
  filter(sample_date == '0') |>
  select(-sample_date)

date_0_ratios <- ER_data[,-c("day", "chainID", "position", "columnID")] |>
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
  mutate(log_ratio = log(value),
         variable_labels = factor(variable,  levels = c("ratio_cbh_ldopa.median", "ratio_glu.xyl_cbh.median",
                                                        "ratio_glu_ldopa.median", "ratio_glu_nag.median",
                                                        "ratio_glu_pep.median", "ratio_pep_pho.median",
                                                        "ratio_xyl_gly.median", "ratio_nag_ldopa.median"),
                                  labels = c("Cbh /\n L-DOPA", "Glu + Xyl / \n Cbh",
                                             "Glu /\n L-DOPA", "Glu / NAG",
                                             "Glu / Pep", "Pep / Pho",
                                             "Xyl / Glu", "NAG /\n L-DOPA")))

ribbon_info <- data |>
  group_by(variable, col_no, sample_date) |>
  mutate(max = max(log_ratio),
         min = min(log_ratio),
         median = median(log_ratio),
         mean = mean(log_ratio))


### LOG RATIO PLOTS ###
# log ratio plot by column posiiton as the model contrast
log_ratio_boxplots <- ggplot(data = ribbon_info, mapping = aes(x = sample_date, y = log_ratio, group = col_no, color = col_no, shape = col_no)) +
  facet_wrap(~variable_labels, nrow = 4, scale = "free", strip.position = "left", axes = "all", axis.labels = "all_y") +
  geom_line(aes(y = 0), colour = "#999", linewidth = 1, linetype = 1) +
  geom_point(size = 1.5, show.legend = FALSE) +
  geom_line(mapping = aes(y = mean), linewidth = 1.2) +
  theme_boxplot() + xlab("Days") + ylab("log ratio of dayX:day0") +
  color_column() +
  theme(legend.position = "bottom")

pdf('output/plots/log_ratio_enzyme_ratios.pdf', width = 5, height = 8)
plot(log_ratio_boxplots)
dev.off()

# log Ratio Plot with the days as the model contrst
enzme_ratios <- ggplot(data, aes(x = col_no, y = log_ratio, color = sample_date, group = sample_date)) +
  facet_wrap(~variable_labels, scales = "free", nrow = 4,
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

### MODEL PLOTs (without log ratios)
# Calculations for plotting the confidence intervaled model outputs
effects_list <- list()

for(i in seq_along(enzyme_ratios)){

  enzyme_name <- enzyme_ratios[[i]]
  effects <- list()
  for(j in seq_along(completed_datasets)){
    effects[[j]] <- effects::Effect(c("day", "position"),
                                    models_time_comparison[[enzyme_name]][[j]],
                                    data = completed_datasets[[j]]) |>
                                    as.data.frame()
  }

 effects_list[[enzyme_name]] <-  effects |>
                                  bind_rows() |>
                                  group_by(day, position) |>
                                  summarise_all(mean)
}

# Rearranging dataframes for the facet plots

ER_data_long <- ER_data |>
  tidyr::pivot_longer(
    cols = -c(sample, day, position, chainID, columnID, sample_date, replicate, col_no),
    names_to = "enzyme",
    values_to = "median_value"
  ) |>
  mutate(enzyme = factor(enzyme, levels = c("cbh_ldopa.median", "glu.xyl_cbh.median",
                                                "glu_ldopa.median", "glu_nag.median",
                                                "glu_pep.median", "pep_pho.median",
                                                "xyl_gly.median", "nag_ldopa.median"),
                            labels = c("Cbh /\n L-DOPA", "Glu + Xyl / \n Cbh",
                                       "Glu /\n L-DOPA", "Glu / NAG",
                                       "Glu / Pep", "Pep / Pho",
                                       "Xyl / Glu", "NAG /\n L-DOPA")))

model_data_long <- lapply(names(effects_list), function(enzyme) {
  df <- effects_list[[enzyme]]
  df$enzyme <- enzyme  # Add enzyme name as a column
  return(df)}) |>
  bind_rows() |>
  rename(median_value = fit, lower = lower, upper = upper) |>
  mutate(enzyme = factor(enzyme, levels = c("cbh_ldopa.median", "glu.xyl_cbh.median",
                                            "glu_ldopa.median", "glu_nag.median",
                                            "glu_pep.median", "pep_pho.median",
                                            "xyl_gly.median", "nag_ldopa.median"),
                         labels = c("Cbh /\n L-DOPA", "Glu + Xyl / \n Cbh",
                                    "Glu /\n L-DOPA", "Glu / NAG",
                                    "Glu / Pep", "Pep / Pho",
                                    "Xyl / Glu", "NAG /\n L-DOPA")))

# setting the dodge distance
pd <- position_dodge(width=0.4)

# add the significant p values from the output_time object (in pairs)
significant_p_values <- output_time %>%
  filter(average_p.value < 0.05) %>%
  mutate(enzyme = factor(ER, levels = c("cbh_ldopa.median", "glu.xyl_cbh.median",
                                            "glu_ldopa.median", "glu_nag.median",
                                            "glu_pep.median", "pep_pho.median",
                                            "xyl_gly.median", "nag_ldopa.median"),
                         labels = c("Cbh /\n L-DOPA", "Glu + Xyl / \n Cbh",
                                    "Glu /\n L-DOPA", "Glu / NAG",
                                    "Glu / Pep", "Pep / Pho",
                                    "Xyl / Glu", "NAG /\n L-DOPA")))

# Function to safely determine the maximum y position for a given enzyme and day
safe_max <- function(enzyme_val, day_val) {
  max_val <- max(ER_data_long$median_value[ER_data_long$enzyme == enzyme_val & ER_data_long$day == day_val], na.rm = TRUE)
  if (is.finite(max_val)) {
    return(max_val * 1.05)
  } else {
    return(0) # Default position if no valid max value is found
  }
}

# Create a data frame for line segments
line_segments <- significant_p_values %>%
  mutate(
    # Determine the y positions for the start and end of each line
    y_max = mapply(function(enzyme_val, contrast_val) {
      day_start <- gsub(" .*", "", contrast_val)
      day_end <- gsub(".* - ", "", contrast_val)
      max_y <- max(
        safe_max(enzyme_val, day_start),
        safe_max(enzyme_val, day_end)
      )
      return(max_y)
    }, enzyme, contrast),

    # Determine the x positions for the start and end of each line
    x_start = as.numeric(factor(gsub(" - .*", "", contrast), levels = levels(ER_data_long$day))),
    x_end = as.numeric(factor(gsub(".* - ", "", contrast), levels = levels(ER_data_long$day)))
  )

output_time_transformed <- significant_p_values |>
  inner_join(line_segments[,-c(4:8)], by =  join_by(ER == ER, contrast == contrast, position == position, enzyme == enzyme)) |>
  tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ") |>
   mutate(
    p_symbol = case_when(
      average_p.value < 0.001 ~ "***",
      average_p.value < 0.01 ~ "**",
      average_p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    x_start_jittered = case_when(
      position == "C1" ~ x_start - 0.15,
      position == "C2" ~ x_start,
      position == "C3" ~ x_start + 0.15
    ),
    x_end_jittered = case_when(
      position == "C1" ~ x_end - 0.15,
      position == "C2" ~ x_end ,
      position == "C3" ~ x_end + 0.15
    )
   )

enzyme_all <- ggplot(ER_data_long, aes(x = day, y = median_value, group = position, color = position, shape = position)) +
  facet_wrap(~enzyme, nrow = 4, scale = "free",
             strip.position = "left", axes = "all", axis.labels = "all_y") +
  geom_point(size = 1.5, position = pd, alpha = 0.3) +
  geom_point(data = model_data_long, aes(x = day, y = median_value), size = 2, position = pd) +
  geom_linerange(data = model_data_long, aes(ymin = lower, ymax = upper), position = pd, show.legend = TRUE) +
  theme_boxplot() + xlab("Days") + ylab("Enzyme Ratios") +
  color_column() + labs(color  = "Column Position", shape = "Column Position")+
  ggpubr::stat_pvalue_manual(data = output_time_transformed, label = "p_symbol", y.position = "y_max",
                             step.increase = 0.1, step.group.by = "ER",
                             xmin = "x_start_jittered", xmax = "x_end_jittered",
                             color = "position", show.legend = FALSE)

