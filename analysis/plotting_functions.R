
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
