setwd("..")
library(data.table)
library(readxl)

BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
meta=list.files(BiblioDir, full.names = T)
paths <- meta[substr(meta, 6,8)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

source("EEA_FLEE/R/functions.R") # Load the functions to read each enzyme spesifically. Change this depending on your EEA read sheets

Glucosidase = sapply(paths, read_glu, USE.NAMES = TRUE)
names(Glucosidase) = name_change(Glucosidase)

mean(Glucosidase$S09_D_C1[1,])

purrr::map_dbl(Glucosidase, mean, progress=T)
mapply(FUN=mean, X=Glucosidase)

L_DOPA = mapply(FUN=read_L_DOPA, paths,  USE.NAMES = TRUE, SIMPLIFY  = TRUE)
names(L_DOPA) = name_change(L_DOPA)
purrr::map_vec(L_DOPA, mean)

paths[1]
paste0(substr(paths[1], 10, nchar(paths[1])-5)) = read_glu(paths[1])

View(lapply(X=paths, FUN=read_glu))
