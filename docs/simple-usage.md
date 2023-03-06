
<!-- README.md is generated from README.Rmd. Please edit that file -->
# ExtracellularEnzymeActivity\_Data\_Prep

The goal of ExtracellularEnzymeActivity\_Data\_Prep is to import/clean and explore FLEE lab Extracellular Enzyme Activity (EEA) data from microplate readings. The data from the reader must be saved as .xlsx files, named after the sample as SampleDayNumber\_replicate\_factor (ex: S09\_B\_C3 in the sample case represents 9th day sample, replicate B, factor level "C3") within the "data" folder.

``` r
library(data.table)
library(readxl)
library(purrr)
library(ggplot2)
```

Get the file names as sample names and the functions

``` r
BiblioDir = list.dirs(path = "../data", full.names = TRUE, recursive = FALSE)
paths = list.files(BiblioDir, full.names = TRUE)
source("../R/functions.R")
source("../R/plotting_functions.R")
```

Read the enzyme activity data from the plate readings

``` r
Gly = enzyme_as_data_table(paths, func = read_glu)
#> Error: Evaluation error: zip file '/Users/selinkubilay/Desktop/EEA/data/S13/~$S13_C_C2.xlsx' cannot be opened.
Xyl = enzyme_as_data_table(paths, func = read_xyl)
#> Error: Evaluation error: zip file '/Users/selinkubilay/Desktop/EEA/data/S13/~$S13_C_C2.xlsx' cannot be opened.
Cbh = enzyme_as_data_table(paths, func = read_Cbh)
#> Error: Evaluation error: zip file '/Users/selinkubilay/Desktop/EEA/data/S13/~$S13_C_C2.xlsx' cannot be opened.
head(Cbh)
#> Error in head(Cbh): object 'Cbh' not found
```

Data is stored as lists of dimensions (n x m x p) for ease of use when the sample number is high. n is the enzymes in the plate, within each has m number of samples with p variables ( in the following example 3 enzymes, 16 samples and 7 variables including sample name, 5 replicated and the mean/median of the replicates)

``` r
list_data = map(list(Gly = Gly, Xyl = Xyl, Cbh = Cbh), convert_to_numeric)
#> Error in vctrs_vec_compat(.x, .purrr_user_env): object 'Gly' not found
list_data = map(list_data, calculate_mean)
#> Error in vctrs_vec_compat(.x, .purrr_user_env): object 'list_data' not found
list_data = map(list_data, correct_name)
#> Error in vctrs_vec_compat(.x, .purrr_user_env): object 'list_data' not found
```

Calculate each enzyme ratio separately. Check the functions folder for more details

``` r
ER_xyl_glu = calculate_xyl_gly(list_data)
#> Error in data.table(sample = x$Gly$sample, xyl_gly = map2_dfr(x$Gly[, : object 'list_data' not found
ER_glu_xyl_cbh = calculate_glu.xyl_cbh(list_data)
#> Error in data.table(sample = x$Gly$sample, glu.xyl_cbh = map2_dfr(x$Gly[, : object 'list_data' not found
```

Convert the NaN and Inf values to 0 since these are all below the detection limit values due to the negative data in the measurements.

``` r
list = list(ER_xyl_glu, ER_glu_xyl_cbh)
#> Error in eval(expr, envir, enclos): object 'ER_xyl_glu' not found
ER_data = Reduce(function(...)  merge(..., by = "sample"), list)
ER_data[is.nan.data.frame(ER_data)] <- 0
ER_data[is.inf.data.frame(ER_data)] <- 0
head(ER_data)
#> numeric(0)
```

Cleaning and appropriately naming the factors

``` r
ER_data[, c("sample_date", "replicate", "col_no") := tstrsplit(sample, "_")]
#> Error in `:=`(c("sample_date", "replicate", "col_no"), tstrsplit(sample, : Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").
#Convert the sample date to days as a factor for ease in plotting
ER_data$sample_date = factor(ER_data$sample_date,
                           levels = c("S09", "S13", "S16", "S19"),
                           labels = c("Day0", "Day3", "Day10", "Day17"))
#> Error in ER_data$sample_date: $ operator is invalid for atomic vectors
ER_data$col_no = factor(ER_data$col_no,
                                 levels = c("C1", "C2"),
                                 labels = c("Col1", "Col2"))
#> Error in ER_data$col_no: $ operator is invalid for atomic vectors
# create a variable for the color scheme
ER_data = set_coloring_column(ER_data)
#> Error: $ operator is invalid for atomic vectors
```

    #> Error in `fortify()`:
    #> ! `data` must be a data frame, or other object coercible by `fortify()`, not a numeric vector
