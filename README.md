These files are meant to help interpret MIMS raw output by converting the given currents into gas concentrations.
There are two sets of code which do slightly different versions of the same thing:
MIMS_with_raw.R uses the given excel sheet and a saved raw data output and calculates gas concentrations using all data points in between your two time points per sample.
MIMS_no_raw.R uses the given excel sheet only

The code runs in R, with the library 'readxl'

The provided excel sheet is a template for MIMS runs. It can be run with the provided .csv raw file to test out code once you've downloaded everything. All files (the desired R script, the gas_functions R script, and your excel/csv files) should be stored in the same folder.

The code calculates currents for N2:Ar (column labeled N2.Ar), O2:Ar (O2.Ar), Ar (X40), O2 (X32), N2 (X28), and 29:28 (X29.28). It can be easily modified to add additional gases by including their concentration functions in the gas_functions sheet and modifying the targCols and satCols lists in the MIMS_*.R lists. Any missing columns will not be run and a warning will be given, but the code will run as expected.

Data will be saved to a csv file ('saveFile').

Please open up the desired R script once downloaded and update the 'MIMSdata', 'rawFile', 'saveFile', and 'pressure' lines as needed.

I run this in R version 3.6.2 (nicknamed 'Dark and Stormy Night'), usually in RStudio, version 1.2.5019. Barring drastic deviances, this should not matter, but please let me know what R version you are running if you are emailing me about a bug for my own sanity.

Any questions, comments, concerns, feature requests, please email evans.kate27@gmail.com 

Thanks everybody!!
