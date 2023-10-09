library(data.table)
library(ggplot2)


bacterial_abundance <- fread("Bacterial_Abundance/Cell_Counts_standardized.csv")
bacterial_abundance$col_no <- factor(bacterial_abundance$col_no, 
                                levels = c("1", "2", "3"),
                                labels = c("Col1", "Col2", "Col3"))

bacterial_abundance$Sample_date <- factor(bacterial_abundance$Sample_date,
                                   levels = c("S09", "S13", "S16", "S19"),
                                   labels = c("0", "3", "10", "17"))
colnames(ER_data)[14:15] <- c("Pep", "ldopa")
ER_data2 <- ER_data
ER_data2 <- ER_data2[bacterial_abundance, on = .(sample_date = Sample_date, replicate = replicate, col_no = col_no)]

ER_data2[, ':=' (pep_norm = (Pep / (Cell_pro_ml / 10^8)), ldopa_norm = (ldopa / (Cell_pro_ml / 10^8)))]

ggplot(ER_data2, mapping = aes(x = sample_date, y = pep_norm)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Pep / (10^8 * cells)")

ggplot(ER_data2, mapping = aes(x = sample_date, y = ldopa_norm)) +
  facet_grid(~col_no, labeller = as_labeller(col_names)) +
  geom_boxplot(mapping = aes(fill = highlight, col = highlight)) +
  fill_col_no + color_col_no +
  theme_boxplot + observation_numbers +
  ylab("Pep / (10^8 * cells)")

# biplot
DOC <- fread("C:/Users/c7701233/Nextcloud/Column-Experiment/DOC_measurements/DOC_git/Expmeriment_DOC_dataprep/DOC_consumption.csv", sep = ",")
DOC_consumed <- melt(DOC[sample_date %in% c("S10", "S13", "S16", "S19")],
                     id.vars = c("replicate", "sample_date"),
                     measure.vars = c("C1consumed", "C2consumed", "C3consumed"))

DOC_consumed$variable <- factor(DOC_consumed$variable, 
                                levels = c("C1consumed", "C2consumed", "C3consumed"),
                                labels = c("Col1", "Col2", "Col3"))

DOC_consumed$sample_date <- factor(DOC_consumed$sample_date,
                                   levels = c("S10", "S13", "S16", "S19"),
                                   labels = c("Day0", "Day3", "Day10", "Day17"))
colnames(DOC_consumed) <- c("replicate", "sample_date", "col_no", "DOC_consumed")

ER_data <- merge(ER_data, DOC_consumed, by.x = c("sample_date", "replicate", "col_no"),  by.y = c("sample_date", "replicate", "col_no"), all.x = TRUE)

ggplot(ER_data, aes(x = pep_pho.median, y = glu_pep.median)) +
  geom_point(aes(color = DOC_consumed), size = 3) +
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
