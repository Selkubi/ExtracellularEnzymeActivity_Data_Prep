library(data.table)
library(ggplot2)
library(dplyr)

### MODEL PLOTs (without log ratios)
# Calculations for plotting the confidence intervaled model outputs
effects_list <- list()

for(i in seq_along(enzyme_ratios)){

  enzyme_name <- enzyme_ratios[[i]]
  effects <- list()
  for(j in seq_along(completed_datasets)){
    effects[[j]] <- effects::Effect(c("day", "position"),
                                    models_position_comparison[[enzyme_name]][[j]],
                                    data = completed_datasets[[j]]) |>
      as.data.frame()
  }

  effects_list[[enzyme_name]] <-  effects |>
    bind_rows() |>
    group_by(day, position) |>
    summarise_all(mean)
}



# add the significant p values from the output_time object (in pairs)
significant_p_values_position <- output_position %>%
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
  max_val <- max(ER_data_long$median_value[ER_data_long$enzyme == enzyme_val & ER_data_long$position == day_val], na.rm = TRUE)
  if (is.finite(max_val)) {
    return(max_val * 1.05)
  } else {
    return(0) # Default position if no valid max value is found
  }
}

ER_data_long <- ER_data_long |>
  mutate(col = factor(paste0("C", substr(col_no, 4, 4))))

# Create a data frame for line segments
line_segments_position <- significant_p_values_position %>%
  mutate(
    # Determine the y positions for the start and end of each line
    y_max = mapply(function(enzyme_val, contrast_val) {
      day_start <- paste0("Column ", sub("^C([0-9]+).*$", "\\1", contrast_val))
      day_end <- paste0("Column ", sub("^C[0-9]+.*C([0-9]+).*$", "\\1", contrast_val))
      max_y <- max(
        safe_max(enzyme_val, day_start),
        safe_max(enzyme_val, day_end)
      )
      return(max_y)
    }, enzyme, contrast),

    # Determine the x positions for the start and end of each line
    x_start = as.numeric(factor(gsub(" - .*", "", contrast), levels = levels(ER_data_long$col))),
    x_end = as.numeric(factor(gsub(".* - ", "", contrast), levels = levels(ER_data_long$col)))
  )

output_position_transformed <- significant_p_values_position |>
  inner_join(line_segments_position[,-c(4:8)], by =  join_by(ER == ER, contrast == contrast, day == day, enzyme == enzyme)) |>
  tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ") |>
  mutate(
    p_symbol = case_when(
      average_p.value < 0.001 ~ "***",
      average_p.value < 0.01 ~ "**",
      average_p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    x_start_jittered = case_when(
      day == "S09" ~ x_start - 0.15,
      day == "S13" ~ x_start,
      day == "S16" ~ x_start + 0.15,
      day == "S19" ~ x_start + 0.30
    ),
    x_end_jittered = case_when(
      day == "S09" ~ x_end - 0.15,
      day == "S13" ~ x_end,
      day == "S16" ~ x_end + 0.15,
      day == "S19" ~ x_end + 0.30
    ),
    sample_date =  case_when(
      day == "S09" ~ factor("0"),
      day == "S13" ~ factor("03"),
      day == "S16" ~ factor("10"),
      day == "S19" ~ factor("17")
    )
  )

enzyme_plot_position <- ggplot(ER_data_long, aes(x = position, y = median_value, group = sample_date, color = sample_date, shape = sample_date)) +
  facet_wrap(~enzyme, nrow = 4, scale = "free",
             strip.position = "left", axes = "all", axis.labels = "all_y") +
  geom_point(size = 1.5, position = pd, alpha = 0.3) +
  geom_point(data = model_data_long, aes(x = position , y = median_value), size = 2.2, position = pd) +
  geom_linerange(data = model_data_long, aes(ymin = lower, ymax = upper), position = pd, show.legend = TRUE) +
  theme_boxplot() + xlab("Days") + ylab("Enzyme Ratios") + scale_shape_manual(values = c(15, 16, 17, 18)) +
  color_sample_date() + labs(color  = "Sampling Date", shape = "Sampling Date") +
  ggpubr::stat_pvalue_manual(data = output_position_transformed, label = "p_symbol", y.position = "y_max",
                             step.increase = 0.1, step.group.by = "ER",
                             xmin = "x_start_jittered", xmax = "x_end_jittered",
                             color = "sample_date", show.legend = TRUE) + theme(legend.position = "right")

facet_labels <- data.frame(
  enzyme = factor(levels(ER_data_long$enzyme)),  # Columns (left-to-right)
  label = paste0( "(",letters[1:8], ")")
)

enzyme_all_position_with_p <- enzyme_plot_position +
  geom_text(data = facet_labels, aes(x = levels(ER_data_long$position)[3],  # Leftmost position
                                     y = Inf, label = label, group = enzyme),
            inherit.aes = FALSE, hjust = -0.5,  vjust = 1.7, size = 4)


pdf('output/plots/enzyme_all_position.pdf', width = 7.5, height = 8)
plot(enzyme_all_position_with_p)
dev.off()
