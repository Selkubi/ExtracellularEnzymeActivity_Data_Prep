#setwd("..")
library(data.table)
library(readxl)
library(purrr)

BiblioDir = list.dirs(path = "data2", full.names =T, recursive = F)
meta = list.files(BiblioDir, full.names = T)
paths = meta[substr(meta, 7,9)%in%c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

source("ExtracellularEnzymeActivity_Data_Prep/R/functions.R") # Load the functions to read each enzyme spesifically. Change this depending on your EEA read sheets

# Import each enzyme, check the values individually to see if there is a crazy value. 
#If too much, use the median for now or trim when calculating the mean. 
Gly = enzyme_as_data_table(paths, func=read_glu)
Xyl = enzyme_as_data_table(paths, func=read_xyl)
NAG = enzyme_as_data_table(paths, func=read_NAG)
Pho = enzyme_as_data_table(paths, func=read_Pho)
Cbh = enzyme_as_data_table(paths, func=read_Cbh)
Ldopa = enzyme_as_data_table(paths, func=read_L_DOPA)
Pep = enzyme_as_data_table(paths, func=read_Pep)


list_data=map(list(Gly=Gly, Xyl=Xyl, NAG=NAG, Pho=Pho, Cbh=Cbh, Ldopa=Ldopa, Pep=Pep), convert_to_numeric)
list_data=map(list_data, calculate_mean)

# Calculate each enzyme ratio separately. These functions are defined in the functions folder.
# For alterations, or changes in how the enzyme ratios calculations, check there 

ER_xyl_glu = calculate_xyl_gly (list_data)
ER_glu_xyl_cbh = calculate_glu.xyl_cbh (list_data)
ER_glu_pep = calculate_glu_pep(list_data)
ER_pep_pho = calculate_pep_pho(list_data)
ER_glu_nag = calculate_glu_nag(list_data)
ER_glu_ldopa = calculate_glu_ldopa(list_data)
ER_cbh_ldopa = calculate_cbh_ldopa(list_data)
ER_nag_ldopa = calculate_nag_ldopa(list_data)

list =  list(ER_xyl_glu, ER_glu_xyl_cbh, ER_glu_pep, ER_pep_pho, ER_glu_nag, ER_glu_ldopa, ER_cbh_ldopa, ER_nag_ldopa)
ER_data = Reduce(function (...)  merge(..., by="sample") , list) 

# Convert the NaN and Inf values to 0 since these are all below the detection limit values due to the negative measurements, indicating the real fluorescence is close to 0
ER_data[is.nan.data.frame(ER_data)] <- 0
ER_data[is.inf.data.frame(ER_data)] <- 0

