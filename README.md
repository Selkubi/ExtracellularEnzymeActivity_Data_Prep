
# Extracellular Enzyme Activity data import and cleaning

The goal of this script is to import/clean and explore FLEE lab Extracellular Enzyme Activity (EEA) data from microplate readings. The data from the reader must be saved as .xlsx files, named after the sample as SampleDayNumber_replicate_factor (ex: S09_B_C3 in the sample case represents 9th day sample, replicate B, factor level "C3") within the "data" folder.
### Data Used
Data used comes from FLEE lab Tecan SPARK plate reader in excel files. It is important to note that the default data export from the SparkControl software saves every fluroresence read in at different positions in the excel sheet and for that reason, for non-default setups, the initial functions needs to be altered. (An automation of this is planned for future use)

## Documentation

For sample usage see the example in the [Documentation](https://github.com/Selkubi/ExtracellularEnzymeActivity_Data_Prep/blob/main/docs/simple-usage.md)


## Badges

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)


## 🚀 About Me
I'm a researcher working at the intersection of microbiology, chemistry and rivers. 
My public repositories contain projects I wrote for automating my own workflow with data coming out of my lab. 


## Acknowledgements
 - [Awesome README](https://github.com/matiassingers/awesome-readme)
