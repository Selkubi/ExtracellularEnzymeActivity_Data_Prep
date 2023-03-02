# EEA_FLEE
Extracellular Enzyme Activity (EEA) data cleaning and exploration from TECAN Spark plate reader
Notes: 
Data needs to be in separate folders, each excel file name corresponds to the sample name. Different folders represend different measurement (&sample) dates.

If separate calibration curves is to be used for each plate, this calculation must be alread done on each excel sheet. Otherwise, same code can be used on raw data as well (with different cell ranges for each read_ function. )

The individual enzymes must be arranged from A to G following, Glucosidase (MUF), Leucine Aminopeptidase (AMC), Xylosidase (MUF), Glucoseaminidase (MUF), Phosphatase (MUF), cellobioside (MUF) L-DOPA (-). Otherwise, the read_functions's ranges must be adapted to the plate design at hand. 
