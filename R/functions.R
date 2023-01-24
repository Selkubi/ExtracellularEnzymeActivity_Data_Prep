read_glucosidase <-  function(path) 
{sapply(excel_sheets(path), read_excel, path = path, 
         USE.NAMES = TRUE, simplify = FALSE, range = "Z52:AD52",  col_names=paste0("rep", seq_along(1:5)))}


read_L_DOPA = function(df){
  L_DOPA=read_excel(df,  range = "Z114:AD114",  col_names=paste0("rep", seq_along(1:5)))
  return(L_DOPA)
}
