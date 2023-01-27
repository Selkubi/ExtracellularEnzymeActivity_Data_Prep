
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
    guide="none")
} 

theme_facets = function() {
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))
}
