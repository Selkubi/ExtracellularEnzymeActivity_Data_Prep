setwd("..")
library(data.table)
source("ExtracellularEnzymeActivity_Data_Prep/R/functions.R")
source("ExtracellularEnzymeActivity_Data_Prep/R/EEA.R")

ER_data[,c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
gglot(ER_data)+
  geom
