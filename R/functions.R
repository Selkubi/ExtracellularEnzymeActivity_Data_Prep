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

read_Pep = function(df){
  measurement=read_excel(df,  range = "Z84:AD84",  col_names=paste0("rep", seq_along(1:5)))
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

calculate_xyl_gly = function (x) {
  data.table(sample = x$Gly$sample, xyl_gly = map2_dfr(x$Gly[,"median"],x$Xyl[,"median"],"sample", 
                                       .f= ~.y / .x, ))
}

calculate_glu.xyl_cbh = function (x) {
  data.table(sample = x$Gly$sample, glu.xyl_cbh = map2_dfr(x$Gly[,"median"],x$Xyl[,"median"], x$Cbh[,"median"], "sample", 
                                       .f= ~((..1 + ..2)/..3)))
  
}

calculate_glu_pep = function (x) {
  data.table(sample = x$Gly$sample, glu_pep = map2_dfr(x$Gly[,"median"],x$Pep[,"median"], "sample", 
                               .f= ~.x /.y))
}

calculate_pep_pho = function (x) {
  data.table(sample = x$Pep$sample, pep_pho = map2_dfr(x$Pep[,"median"],x$Pho[,"median"], "sample", 
                               .f= ~.x /.y))
}

calculate_glu_nag = function (x) {
  data.table(sample = x$Gly$sample, glu_nag = map2_dfr(x$Gly[,"median"], x$NAG[,"median"], "sample", 
                               .f= ~.x /.y))
}

calculate_glu_ldopa = function (x) {
  data.table(sample = x$Gly$sample, glu_ldopa = map2_dfr(x$Gly[,"median"], x$Ldopa[,"median"], "sample", 
                               .f= ~.x /.y))
}

calculate_cbh_ldopa = function (x) {
  data.table(sample = x$Cbh$sample, cbh_ldopa = map2_dfr(x$Cbh[,"median"], x$Ldopa[,"median"], "sample", 
                               .f= ~.x /.y))
}

calculate_nag_ldopa = function (x) {
  data.table(sample = x$NAG$sample, nag_ldopa = map2_dfr(x$NAG[,"median"], x$Ldopa[,"median"], "sample", 
                               .f= ~.x /.y))
}




