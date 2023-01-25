setwd("..")
library(data.table)
library(readxl)
library(purrr)

BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
meta=list.files(BiblioDir, full.names = T)
paths <- meta[substr(meta, 6,8)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

source("EEA_FLEE/R/functions.R") # Load the functions to read each enzyme spesifically. Change this depending on your EEA read sheets

# Import each enzyme, check the values individually to see if there is a crazy value. 
#If too much, use the median for now or trim when calculating the mean. 
Gly = enzyme_as_data_table(paths, func=read_glu)
Xyl = enzyme_as_data_table(paths, func=read_xyl)
NAG = enzyme_as_data_table(paths, func=read_NAG)
Pho = enzyme_as_data_table(paths, func=read_Pho)
Cbh = enzyme_as_data_table(paths, func=read_Cbh)
Ldopa = enzyme_as_data_table(paths, func=read_L_DOPA)


list_data=map(list(Gly=Gly, Xyl=Xyl, NAG=NAG, Pho=Pho, Cbh=Cbh, Ldopa=Ldopa), convert_to_numeric)
list_data=map(list_data, calculate_median)

map2(list_data$Gly[,7],list_data$Xyl[,7], .f= ~.y / .x)
