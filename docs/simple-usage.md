
<!-- README.md is generated from README.Rmd. Please edit that file -->
# ExtracellularEnzymeActivity\_Data\_Prep

<!-- badges: start -->
a <!-- badges: end -->

The goal of ExtracellularEnzymeActivity\_Data\_Prep is to import/clean and explore FLEE lab Extracellular Enzyme Activity (EEA) data from microplate readings. The data from the reader must be saved as .xlsx files, named after the sample as SampleDayNumber\_replicate\_factor (ex: S09\_B\_C3 in the sample case represents 9th day sample, replicate B, factor level "C3") within the "data" folder.

``` r
library(data.table)
library(readxl)
library(purrr)
library(ggplot2)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

Get the file names as sample names and the functions

``` r
BiblioDir = list.dirs(path = "data", full.names =T, recursive = F)
paths = list.files(BiblioDir, full.names = T)
source("R/functions.R")
#> Warning in file(filename, "r", encoding = encoding): cannot open file
#> 'R/functions.R': No such file or directory
#> Error in file(filename, "r", encoding = encoding): cannot open the connection
source("R/plotting_functions.R")
#> Warning in file(filename, "r", encoding = encoding): cannot open file
#> 'R/plotting_functions.R': No such file or directory
#> Error in file(filename, "r", encoding = encoding): cannot open the connection
```

Read the enzyme activity data from the plate readings

``` r
Gly = enzyme_as_data_table(paths, func=read_glu)
#> Error in enzyme_as_data_table(paths, func = read_glu): could not find function "enzyme_as_data_table"
Xyl = enzyme_as_data_table(paths, func=read_xyl)
#> Error in enzyme_as_data_table(paths, func = read_xyl): could not find function "enzyme_as_data_table"
Cbh = enzyme_as_data_table(paths, func=read_Cbh)
#> Error in enzyme_as_data_table(paths, func = read_Cbh): could not find function "enzyme_as_data_table"
```

Data is stored as lists for ease of use when the sample number is high

``` r
list_data = map(list(Gly=Gly, Xyl=Xyl, Cbh=Cbh), convert_to_numeric)
#> Error in vctrs_vec_compat(.x, .purrr_user_env): object 'Gly' not found
list_data = map(list_data, calculate_mean)
#> Error in vctrs_vec_compat(.x, .purrr_user_env): object 'list_data' not found
```

Calculate each enzyme ratio separately. Check the functions folder for more details

``` r
ER_xyl_glu = calculate_xyl_gly (list_data)
#> Error in calculate_xyl_gly(list_data): could not find function "calculate_xyl_gly"
ER_glu_xyl_cbh = calculate_glu.xyl_cbh (list_data)
#> Error in calculate_glu.xyl_cbh(list_data): could not find function "calculate_glu.xyl_cbh"
```

Convert the NaN and Inf values to 0 since these are all below the detection limit values due to the negative data in the measurements.

``` r
list = list(ER_xyl_glu, ER_glu_xyl_cbh)
#> Error in eval(expr, envir, enclos): object 'ER_xyl_glu' not found
ER_data = Reduce(function (...)  merge(..., by="sample"), list) 

ER_data[is.nan.data.frame(ER_data)] <- 0
#> Error in ER_data[is.nan.data.frame(ER_data)] <- 0: could not find function "is.nan.data.frame"
ER_data[is.inf.data.frame(ER_data)] <- 0
#> Error in ER_data[is.inf.data.frame(ER_data)] <- 0: could not find function "is.inf.data.frame"
```

Cleaning and appropriately naming the factors

    #> Error in `:=`(c("sample_date", "replicate", "col_no"), tstrsplit(sample, : Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(":=").

``` r
ER_data = set_coloring_column(ER_data)
#> Error in set_coloring_column(ER_data): could not find function "set_coloring_column"
ggplot(ER_data, mapping = aes(x=sample_date, y = xyl_gly.median))+
  facet_grid(~col_no, labeller = as_labeller(col_names))+
  geom_boxplot(mapping = aes(fill = highlight, col = highlight))+
  fill_col_no2 () + color_col_no2() +
  theme_boxplot() +
  ylab("Xyl/Glu")
#> Error in `fortify()`:
#> ! `data` must be a data frame, or other object coercible by `fortify()`, not a list
```
