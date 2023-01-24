setwd("..")
library(data.table)
library(readxl)

BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
meta=list.files(BiblioDir, full.names = T)

paths <- meta[substr(meta, 6,8)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

source("EEA_FLEE/R/functions.R")

Glucosidase <- sapply(paths, read_glucosidase, USE.NAMES = TRUE, simplify = FALSE) 

x=mapply(FUN=cbind, X=Glucosidase)
names(x)=substr(names(x), 10, nchar(names(x))-5)

L_DOPA=mapply(FUN=cbind, X=sapply(paths, read_L_DOPA))
