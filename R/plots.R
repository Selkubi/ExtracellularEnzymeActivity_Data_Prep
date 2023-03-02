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

melted_ER=melt(ER_data, id.vars= c("sample","sample_date", "col_no", "replicate"))

ggplot(melted_ER)+
  facet_wrap(~variable, scale="free", nrow=2)+
  geom_boxplot(aes(x=as.factor(sample_date), y=(value),  fill=as.factor(col_no)))+
  color_selected() + fill_selected() + theme_boxplot()+
  geom_hline(yintercept = c(0), color="red", linetype="dashed")

# Individual enzyme ratio boxplots
ER_data=set_coloring_column(ER_data)

#xyl_gly.median
ggplot(ER_data, mapping=aes(x=sample_date, y=xyl_gly.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Xyl/Glu")

#glu.xyl_cbh.median
ggplot(ER_data, mapping=aes(x=sample_date, y=glu.xyl_cbh.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Glu+Xyl/Cbh")

#glu_pep.median
ggplot(ER_data, mapping=aes(x=sample_date, y=glu_pep.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Glu/Pep")

#pep_pho.median
ggplot(ER_data, mapping=aes(x=sample_date, y=pep_pho.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Pep/Phosph")

#glu_nag.median
ggplot(ER_data, mapping=aes(x=sample_date, y=glu_nag.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Glu/Nag")

#glu_ldopa.median
ggplot(ER_data, mapping=aes(x=sample_date, y=glu_ldopa.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Glu/L-DOPA")

#cbh_ldopa.median
ggplot(ER_data, mapping=aes(x=sample_date, y=cbh_ldopa.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Cbh/L-DOPA")

#nag_ldopa.median
ggplot(ER_data, mapping=aes(x=sample_date, y=nag_ldopa.median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Nag/L-DOPA")

#Leucine aminopepdinase median
ER_data = ER_data[calculate_median(Pep)[,c(1,7)], on=.(sample=sample)]

ggplot(ER_data, mapping=aes(x=sample_date, y=median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("Pep")

#L-DOPA median
ER_data = ER_data[calculate_median(Ldopa)[,c(1,7)], on=.(sample=sample)]

ggplot(ER_data, mapping=aes(x=sample_date, y=median))+
  facet_grid(~col_no, labeller=as_labeller(col_names))+
  geom_boxplot(mapping=aes(fill=highlight, col=highlight))+
  fill_col_no()+ color_col_no()+
  theme_boxplot()+observation_numbers()+
  ylab("L-DOPA")



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
