setwd("..")
library(data.table)
library(ggplot2)

source("ExtracellularEnzymeActivity_Data_Prep/R/functions.R")
source("ExtracellularEnzymeActivity_Data_Prep/R/EEA.R")
source("ExtracellularEnzymeActivity_Data_Prep/R/plotting_functions.R")

ER_data[,c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
ER_data$sample_date=factor(ER_data$sample_date,
                           levels=c("S09", "S13", "S16", "S19"),
                           labels=c("Day0", "Day3", "Day10", "Day17")) #Convert the sample date to days as a factor for ease in plotting
ER_data$col_no=factor(ER_data$col_no,
                                 levels=c("C1", "C2", "C3"),
                                 labels=c("Col1", "Col2", "Col3")) 


melted_ER=melt(ER_data, FUN=median, na.rm=T, by= ER_data[,c("sample_date", "col_no")])

ggplot(melted_ER)+
  facet_wrap(~variable, scale="free", nrow=2)+
  geom_boxplot(aes(x=as.factor(sample_date), y=(value),  fill=as.factor(col_no)))+
  color_selected() + fill_selected() + theme_facets()+
  geom_hline(yintercept = c(0), color="red", linetype="dashed")


# lineplot with the median values
cols=colnames(ER_data[,-c(1, 10:12)])
ER_data[, paste0(cols) := lapply(.SD, median, na.rm=T), .SDcols = cols, by = .(sample_date, col_no)]
ER_sum=aggregate(ER_data[,-c(1, 10:12)], FUN=median, na.rm=T, by = ER_data[,c("sample_date", "col_no")], data=ER_data)

melted_median_ER_data=melt(ER_sum, id.vars = c("sample_date", "col_no"), 
                    measure.vars = colnames(ER_sum)[endsWith(colnames(ER_sum),  c("median"))])

ggplot(melted_median_ER_data)+
  facet_wrap (~variable, scale="free")+
  geom_point(aes(x=sample_date, y=value,  fill=col_no, shape=col_no), size=3)+
  geom_line(aes(x=sample_date, y=value, group=col_no, col=col_no), linewidth=1.2)+
  color_selected() + label_shape() + fill_selected() + theme_facets()+
  geom_hline(yintercept = c(0), color="red", linetype="dashed")

# biplot
DOC=fread("C:/Users/c7701233/Nextcloud/Column-Experiment/DOC_measurements/DOC_git/Expmeriment_DOC_dataprep/DOC_consumption.csv", sep = ",")
DOC_consumed = melt(DOC[sample_date%in%c("S10", "S13", "S16", "S19")], 
                    id.vars=c("replicate", "sample_date"), 
                    measure.vars=c("C1consumed", "C2consumed", "C3consumed"))

DOC_consumed$variable = factor(DOC_consumed$variable, 
                               levels = c("C1consumed", "C2consumed", "C3consumed"), 
                               labels = c("Col1", "Col2", "Col3"))

DOC_consumed$sample_date = factor(DOC_consumed$sample_date, 
                               levels = c("S10", "S13", "S16", "S19"), 
                               labels = c("Day0", "Day3", "Day10", "Day17"))
colnames(DOC_consumed) = c("replicate", "sample_date","col_no", "DOC_consumed") 

ER_data = merge(ER_data, DOC_consumed, by.x=c("sample_date", "replicate", "col_no"),  by.y=c("sample_date", "replicate", "col_no"), all.x=T)

ggplot(ER_data, aes(x = pep_pho.median, y = glu_pep.median))+
  geom_point(aes(color=DOC_consumed), size=3)+
  scale_colour_gradient2(
    low = ("red"),
    mid = "green",
    high = ("blue"),
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )
