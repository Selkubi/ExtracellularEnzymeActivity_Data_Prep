read_glu <-  function(df){
  measurement=read_excel(df,  range = "Z52:AD52",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_xyl <-  function(df){
  measurement=read_excel(df,  range = "Z58:AD58",  col_names=paste0("rep", seq_along(1:5)))
  return(measurement)
}

read_NAG <-  function(df){
  measurement=fread(df,  skip=63, select = c(26:30), col_names=paste0("rep", seq_along(1:5)))
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
    names(x)=substr(names(x), 10, nchar(names(x))-5)
}

mean_value = function (x){
  result= mean(unlist(x))
  return(result)
}

