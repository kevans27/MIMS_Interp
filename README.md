These files are meant to help interpret MIMS raw output by converting the given currents into gas concentrations.
There are two sets of code which do slightly different versions of the same thing:
MIMS_with_raw.R uses the given excel sheet and a saved raw data output and calculates gas concentrations using all data points in between your two time points per sample.
MIMS_no_raw.R uses the given excel sheet only

The code runs in R, with the library 'readxl'

Any questions, comments, concerns, feature requests, please email evans.kate27@gmail.com
