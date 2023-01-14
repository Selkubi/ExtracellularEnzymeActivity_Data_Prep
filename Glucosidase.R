library(data.table)
library(ggplot2)

# Import data, make the necessary factors
Glucosidase_Jove <-fread("C:/Users/c7701233/Nextcloud/Column-Experiment/EEA/Gly_Jove.txt")
Glucosidase_Jove[,c("sample_date", "replicate", "column_no") := tstrsplit(Gly, "_")]

Glucosidase_Jove[sample_date=="S16" & column_no=="C1"]$Mean_Sample=Glucosidase_Jove[sample_date=="S16" & column_no=="C1"]$Mean_Sample/1.7902
Glucosidase_Jove$Glu_Consumed_total2 = (Glucosidase_Jove$Mean_Sample-Glucosidase_Jove$mean_slurry)/(Glucosidase_Jove$FluorPerMole*Glucosidase_Jove$QuenchPerMole*Glucosidase_Jove$Sample_volume_in_black_plate)

ggplot(Glucosidase_Jove)+
  facet_wrap(~column_no)+
  geom_boxplot(aes(x=sample_date, y=Glu_Consumed_total2))
