BiblioDir <- list.dirs(path = "data/EEA", full.names = TRUE, recursive = FALSE)
meta <- list.files(BiblioDir, full.names = TRUE)
paths <- meta[substr(meta, 10, 12) %in% c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

devtools::load_all() # Load the functions to read each enzyme spesifically. Change this depending on your EEA read sheets

# Import each enzyme, check the values individually to see if there is are outliers.
# If there are too many outliers, useage of the median instead of the mean would be beneficial in data exploration.
Gly <- enzyme_as_data_table(paths, func = read_glu)
Xyl <- enzyme_as_data_table(paths, func = read_xyl)
NAG <- enzyme_as_data_table(paths, func = read_NAG)
Pho <- enzyme_as_data_table(paths, func = read_Pho)
Cbh <- enzyme_as_data_table(paths, func = read_Cbh)
Ldopa <- enzyme_as_data_table(paths, func = read_L_DOPA)
Pep <- enzyme_as_data_table(paths, func = read_Pep)

# Data is stored as lists for ease of use when the sample number is high
list_data <- purrr::map(list(Gly = Gly, Xyl = Xyl, NAG = NAG, Pho = Pho, Cbh = Cbh, Ldopa = Ldopa, Pep = Pep), convert_to_numeric)
list_data <- purrr::map(list_data, calculate_median)

# combine the median values
EEA_data <- dcast(rbindlist(lapply(list_data, `[`, j = .(sample, median)), idcol = "enzyme"),
                   sample ~ enzyme, value.var = "median")

# Read in the biomass related data
### Biomass correction of c consumtion
Sample_biomass <- fread("data/biomass/Cell_Counts_standardized.csv")
Sample_biomass$col_no <- factor(Sample_biomass$col_no, levels = c("1", "2", "3"), labels = c("C1", "C2", "C3"))

