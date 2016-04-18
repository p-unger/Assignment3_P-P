# Collaborative Social Science Data Analysis: Pair Assignment 3
In this repository you can find the third pair assignment for 'Introduction to Collaborative Data Science' at the Hertie School of Governance.

**Students:** Philipp Ständer and Philip Unger. 

In the assignment we gather and combine the data sources we need for the final research project and present decriptive statistics and correlations that relate to our research question. 

**File description**
- GSS_data_downloader.R: Downloads the General Social Survey (GSS) from 1972-2014.
- deflator.R: Downloads the Personal Consumption Expenditure (PCE) index from the St. Louis Federal Reserve and loads it as a data-frame in a format that can be merged with the GSS.
- CPS_Bertrand.R: Downloads the replication file from Bertrand (2013) and extracts data on income percentiles from the Current Population Survey (CPS). 
- GSS_prep_dynamic.R: Combines the data from the GSS, the PCE index and the CPS and performs data-transformations to create the final dataset that is used for the descriptive statistics and analyses presented in the markdown file. 
- data_combine.R: Sources the four above mentioned files sequentially. 
- xx: Markdown file that presents descriptive statistics and correlations of the final dataset that relate to our research question. 

The "data" folder contains the data-sets used for the analysis. Downloading and transforming the data-sets is time-consuming, thus "data_combine.R" only downloads the files when they are not present in the data-folder. 

**How to reproduce the paper**
The markdown-file 'xx.Rmd' is dynamically reproducible and sources 'data_combine.R', which downloads and transforms all relevant sub-datasets into the final dataset. Note, however, that it is computational demanding and can take 15-35 minutes to run. To ease computation, each of the data-generation files saves a dataset after their first run, and if they are present in the "data" folder, the 'data_combine.R' skips the phase. Further, if the final dataset is present in the working directory, the markdown-file simply loads the final dataset rather than re-download and re-transform all the sub-datasets. 
