read_glu <-  function(df){
  measurement=read_excel(df,  range = "Z52:AD52",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_xyl <-  function(df){
  measurement=read_excel(df,  range = "Z58:AD58",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_NAG <-  function(df){
  measurement=read_excel(df,  range = "Z64:AD64",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_Pho <-  function(df){
  measurement=read_excel(df,  range = "Z70:AD70",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_Cbh <-  function(df){
  measurement=read_excel(df,  range = "Z76:AD76",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_L_DOPA = function(df){
  measurement=read_excel(df,  range = "Z114:AD114",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

name_change = function(x) {
   substr(x$sample, 10, nchar(x$sample)-5)
}

enzyme_as_data_table = function(x, func) {
  df=as.data.table(t(mapply(x, FUN=func, USE.NAMES = TRUE)), keep.rownames="sample")
  df$sample = name_change(df)
  
  return(df)
}

convert_to_numeric = function(x){
  x[,`:=`(rep1 = as.numeric(rep1),
            rep2 = as.numeric(rep2),
            rep3 = as.numeric(rep3), 
            rep4 = as.numeric(rep4),
            rep5 = as.numeric(rep5))]
}

calculate_median = function (x) {
  x$median=apply(x[,-1], 1, median)
  return(x)
}

xyl_gly = function (x,y) {
  apply(x[,-1], 1, median)
}


