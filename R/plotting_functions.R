
color_selected <- scale_color_manual(values = c("#DB2A2A", "#FFBF1C", "#E0E334"))

label_shape <- scale_shape_manual(
    values = c(22, 23, 24),
    guide = "none"
  )

fill_selected <- scale_fill_manual(
    name =  "Column No",
    labels = c("Col1", "Col2", "Col3"),
    values = c("#DB2A2A", "#FFBF1C", "#E0E334"),
    guide = "legend")

theme_boxplot <- theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            text =  element_text(size = 12),
            axis.text.x = element_text(size = 12))

set_coloring_column <- function(data) {
  data$highlight = factor(paste0(ifelse(data$sample_date == "0", "before", "after"), " C", substr(data$col_no, 4, 4)))
  return(data)
}

fill_col_no <- scale_fill_manual(name =  "Column Location",
                     values = c("#1741a3", "#4e8fc8", "#a698cc", "white", "white", "white"),
                     guide = "legend")

color_col_no <- scale_color_manual(name =  "Column Location",
                      values = c("black", "black", "black", "#1741a3", "#4e8fc8", "#a698cc"),
                      guide = "legend")

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
