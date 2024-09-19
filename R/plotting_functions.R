
color_selected <- scale_color_manual(values = c("#DB2A2A", "#FFBF1C", "#E0E334"))

label_shape <- scale_shape_manual(
    values = c(22, 23, 24),
    guide = "none"
  )

fill_selected <- function() {
  scale_fill_manual(
    name =  "Column No",
    labels = c("Col1", "Col2", "Col3"),
    values = c("#DB2A2A", "#FFBF1C", "#E0E334"),
    guide = "legend")}

theme_boxplot <-  function() {
  ggplot2::theme(panel.background = element_rect(fill = NA),
                 panel.grid = element_blank(),
                 strip.background = element_blank(), 
                 strip.text = element_text(size = 11, color = "black"), 
                 strip.placement ="outside") +
    ggplot2::theme(text = element_text(size = 11),
                   axis.line.x =  element_line(color = "black"),
                   axis.line.y =  element_line(),
                   axis.text = element_text(size = 11, color = "black"), 
                   axis.title = element_blank(), 
                   legend.position = "bottom") 
}
  

set_coloring_column <- function(data) {
  data$highlight = factor(paste0(ifelse(data$sample_date == "0", "before", "after"), " C", substr(data$col_no, 4, 4)))
  return(data)
}

fill_col_no <- function() {
  scale_fill_manual(name =  "Column Location",
                     values = c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                     guide = "legend")
  }

color_col_no <- function() {
  scale_color_manual(name =  "Column Location",
                      values = c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                      guide = "legend")}

fill_sample_date <- function() {
  scale_fill_manual(name =  "Sampling Date",
                                 values = c("#b7cdb8", "#e7dff1", "#c9bbd8", "#ac98c1"),
                                 guide = "legend")
  }

color_sample_date <- function() {
  scale_color_manual(name =  "Sampling Date",
                                      values = c("#b7cdb8", "#e7dff1", "#c9bbd8", "#ac98c1"),
                                      guide = "legend")}

col_names <- c(
  `Reservoir` = "Res.",
  `Col1` = "Column 1",
  `Col2` = "Column 2",
  `Col3` = "Column 3"
)

n_fun <- function(x) {
  return(data.frame(y = max(x, na.rm = TRUE), label = paste0(length(x))))
  }

observation_numbers <- stat_summary(fun.data = n_fun, geom = "text", na.rm = TRUE, aes(vjust = 0))

fill_col_no2 <- scale_fill_manual(name = "Column Location",
                     values = c("#1741a3", "#4e8fc8", "white", "white"),
                     guide = "legend")

color_col_no2 <- scale_color_manual(name = "Column Location",
                      values = c("black", "black", "#1741a3", "#4e8fc8"),
                      guide = "legend")

enzyme_ratio_names <- list(
  "xyl_gly.median" = "Xyl / Glu",
  "glu.xyl_cbh.median" = "Glu + Xyl/ Cbh",
  "glu_pep.median" =  "Glu / Pep",
  "pep_pho.median" = "Pep / Pho",
  "glu_nag.median" = "Glu / NAG",
  "glu_ldopa.median" = "glu / L-DOPA",
  "cbh_ldopa.median" = "Cbh / L-DOPA",
  "nag_ldopa.median" = "NAG / L-DOPA"
)

enzyme_labeller <- function(variable,value){
  return(enzyme_ratio_names[value])
}

col_names <- c(
  `Col1` = "Column 1",
  `Col2` = "Column 2",
  `Col3` = "Column 3"
)

column_labeller <- function(variable,value){
  return(col_names[value])
}
