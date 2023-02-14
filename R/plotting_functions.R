
color_selected = function(){
  scale_color_manual(values=c( "#DB2A2A", "#FFBF1C", "#E0E334"))               
} 

label_shape = function(){
  scale_shape_manual(
    values=c(22,23,24), 
    guide="none"
  )
}

fill_selected = function(){
  scale_fill_manual(
    name =  "Column No",
    labels=c("Col1", "Col2", "Col3"), 
    values=c( "#DB2A2A", "#FFBF1C", "#E0E334"),
    guide="legend")
} 

theme_boxplot = function () {
  theme_bw()+
    theme(axis.text = element_text(size=10), 
          axis.title = element_text(size=12), 
          text =  element_text(size=10))
}

set_coloring_column = function(data) {
  data$highlight = factor(ifelse(data$sample_date=="Day0" & data$col_no=="Col1", "before C1", 
                                 ifelse(data$sample_date=="Day0" & data$col_no=="Col2", "before C2",
                                        ifelse(data$sample_date=="Day0" & data$col_no=="Col3", "before C3", 
    
                                    ifelse(data$sample_date != c("Day0") & data$col_no=="Col1", "after C1",
                                          ifelse(data$sample_date != c("Day0") & data$col_no=="Col2", "after C2",
                                              ifelse(data$sample_date != c("Day0") & data$col_no=="Col3", "after C3",
                                                                                         "Reservoirs")))))))
  return(data)
}



fill_col_no = function(){
  scale_fill_manual( name =  "Column Location",
                     #labels=c("Column 1", "Column 2", "Column 3", "Before Reversal"), 
                     values=c("#1741a3", "#4e8fc8", "#a698cc", "grey90", "grey90", "grey90"),
                     guide="legend")
}
                                                       