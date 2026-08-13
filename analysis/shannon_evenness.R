# Shannon evenness of the carbon-acquiring enzyme activities per column sample.
#
# Evenness describes how evenly a community distributes its enzymatic effort across the
# carbon-acquiring enzymes, i.e. the breadth of the carbon-degrading portfolio it maintains.
# It is calculated from proportions and is therefore scale invariant: normalising the
# activities by biomass would not change it, so it is unaffected by the biomass variation
# and quenching that motivated the use of enzyme ratios.
#
# Run analysis/01_EEA_Data_import.R (for list_data) and the first lines of
# analysis/02_mixed_model_anova.R (for day, chainID and position in ER_data) beforehand.

library(data.table)

# Carbon-acquiring enzymes measured with MUF-linked substrates, all in nmol-based units and
# therefore comparable within a sample: cellobiose (Gly), hemicellulose (Xyl), cellulose
# (Cbh) and chitin (NAG) degradation. NAG also serves N acquisition but targets a carbon
# polymer and is retained here. Pho and Pep are excluded because they acquire P and N rather
# than carbon, and because Pho is among the largest absolute activities in the data set and
# would dominate the proportions. Pox (L-DOPA) is added separately below.
c_enzymes <- c("Gly", "Xyl", "Cbh", "NAG")

#' Shannon entropy and Pielou evenness of one sample's activity profile
#'
#' Zero and missing activities are excluded, so richness counts only the enzymes
#' detected in that sample and evenness is scaled by log of that richness.
shannon_index <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 2) return(NA_real_)
  p <- x / sum(x)
  -sum(p * log(p))
}

shannon_evenness <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 2) return(NA_real_)
  shannon_index(x) / log(length(x))
}

# Activity matrix from the medians already calculated in 01_EEA_Data_import.R:
# one row per column sample, one column per enzyme
activity <- sapply(list_data[c_enzymes], `[[`, "median")
rownames(activity) <- list_data$Gly$sample

evenness_data <- data.table(
  sample   = rownames(activity),
  richness = apply(activity, 1, function(x) sum(!is.na(x) & x > 0)),
  shannon  = apply(activity, 1, shannon_index),
  evenness = apply(activity, 1, shannon_evenness)
)

# Attach the design factors already parsed in 02_mixed_model_anova.R
evenness_data <- merge(evenness_data,
                       ER_data[, .(sample, day, chainID, position)],
                       by = "sample")

# Variant including Pox, which extends the portfolio to the most recalcitrant substrate
# (lignin) and is the fraction H3 expects to distinguish the flowpath positions. Because the
# L-DOPA assay is on a different scale, each enzyme is first expressed relative to its
# maximum across all samples; evenness then describes the balance of relative rather than
# absolute activities and is not comparable in value to the four-enzyme index above.
activity_scaled <- sapply(list_data[c(c_enzymes, "Ldopa")], `[[`, "median")
rownames(activity_scaled) <- list_data$Gly$sample
activity_scaled <- sweep(activity_scaled, 2,
                         apply(activity_scaled, 2, max, na.rm = TRUE), "/")

evenness_data[, evenness_with_pox := apply(activity_scaled, 1, shannon_evenness)[sample]]

# Group means for a first look at the expected Day 0 gradient (C3 > C1)
evenness_summary <- evenness_data[, .(n = .N,
                                      evenness = mean(evenness, na.rm = TRUE),
                                      evenness_with_pox = mean(evenness_with_pox, na.rm = TRUE)),
                                  by = .(day, position)][order(day, position)]
