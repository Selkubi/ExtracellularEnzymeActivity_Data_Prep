BiblioDir <- list.dirs(path = "data", full.names = TRUE, recursive = FALSE)
meta <- list.files(BiblioDir, full.names = TRUE)
paths <- meta[substr(meta, 6, 8) %in% c("S09", "S13", "S16", "S19")]#The real community samples (others are water)

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

# Calculate pre-determined enzyme ratios. For more information the functions.R file. Set the column name as mean or median according to the preference in line above.
ER_data <- calculate_enzyme_ratios(list_data, column_name = "median")

# Convert the NaN and Inf values to 0 since these are all below the detection limit values due to the negative measurements, indicating the real fluorescence is close to 0
ER_data[is.nan.data.frame(ER_data)] <- NA
ER_data[is.inf.data.frame(ER_data)] <- NA
