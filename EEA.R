setwd("..")
library(data.table)
library(readxl)

MUF<- read_excel("data/S09/S09_D_C3.xlsx", range = "A55:M63")

AMC <- read_excel("data/S09/S09_D_C3.xlsx", range = "A89:M97")

L_DOPA <- read_excel("data/S09/S09_D_C3.xlsx", range = "A112:M120")

S09_D_C3=list("MUF"=MUF, "AMC"=AMC, "L_DOPA"=L_DOPA)

BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
meta=list.files(BiblioDir, full.names = T)

paths <- meta[substr(meta, 6,8)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water samples)

read_all_sheets <-  function(path) 
   { sapply(excel_sheets(path), read_excel, path = path, 
           USE.NAMES = TRUE, simplify = FALSE, range = "A112:M120")}

xl_list_community <- sapply(paths, read_all_sheets, USE.NAMES = TRUE, simplify = FALSE) 
