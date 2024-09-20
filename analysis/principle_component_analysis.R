library(ggplot2)
source("ExtracellularEnzymeActivity_Data_Prep/R/EEA.R")

# PCA analysis

wine.pca = prcomp(ER_data[, -("sample")], scale. = TRUE)
summary(wine.pca)
PCAloadings = data.frame(Variables = rownames(wine.pca$rotation), wine.pca$rotation)

ER_data[, c("sample_date", "replicate", "col_no") := tstrsplit(sample,  "_")]
pca_results = cbind(ER_data, wine.pca$x)

ggplot(pca_results, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = sample_date,  fill = sample_date, shape = col_no),  size = 4) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1 * 10),
                                       yend = (PC2 * 10)), arrow = arrow(length = unit(1 / 2, "picas")), color = "black") +
  annotate("text", x = (PCAloadings$PC1 * 10.4), y = (PCAloadings$PC2 * 10.4),
           label = PCAloadings$Variables) +
  theme_classic() +
  guides(fill = "legend") +
  labs(color = "Sites", x = "PC 1 (32%)", y = "PC 2 (20%)", title = "PCA of PARAFAC Components and Other Optical Parameters")
