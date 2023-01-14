setwd("..")
library(data.table)
library(readxl)

MUF<- read_excel("data/S09/S09_D_C3.xlsx", range = "A55:M63")

AMC <- read_excel("data/S09/S09_D_C3.xlsx", range = "A89:M97")

L_DOPA <- read_excel("data/S09/S09_D_C3.xlsx", range = "A112:M120")

S09_D_C3=list("MUF"=MUF, "AMC"=AMC, "L_DOPA"=L_DOPA) # Check the raw data file format. It should match the cells above

BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
meta=list.files(BiblioDir, full.names = T)

paths <- meta[substr(meta, 6,8)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

read_all_sheets <-  function(path) 
   { sapply(excel_sheets(path), read_excel, path = path, 
           USE.NAMES = TRUE, simplify = FALSE, range = "A112:M120")}

xl_list_community <- sapply(paths, read_all_sheets, USE.NAMES = TRUE, simplify = FALSE) 

xl_list_community[[1]][[1]]

take_L_DOPA = function(df){
  L_DOPA=t(df)[2:12,7]
  return(L_DOPA)
}

x=mapply(FUN=cbind, X=xl_list_community)
names(x)=substr(names(x), 10, nchar(names(x))-5)
#maybe it makes more sense to leave it as a list, it would be easier to do all the calculations with a list
data=lapply(x, take_L_DOPA) # This is the main data. The line G of each plate

biofilm_mean = function(x){
  return(mean(x[1],x[2],x[3], na.rm=T))
}

lapply(data, FUN=biofilm_mean)

data[1]
data[[]][1]
