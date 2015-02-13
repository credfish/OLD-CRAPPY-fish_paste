# fish_paste
Standard preprocessing to create analysis ready data

* requires the R package ProjectTemplate
* in addition all the packages specified in the config/global text file
        (reshape, plyr, ggplot2, stringr, lubridate, gdata, gstat, gmt, lattice)
* open R console
* set working directory to fish_paste
* library(ProjectTemplate)
* load.project()

The ProjectTemplate package autoloads the data and functions and preprocessing files in the munge folder.

On the first pass the preprocessing files should be edited:

02_Fish_Base_select_summary_fish_metrics should be edited for whichever summary metrics required

03_Fish_Base_standardizing_and_pooling should be edited for whichever pooling level and scheme required.

Once these have been edited, then the data files are cached for quicker loading in future.


# Folder structure
* lib - local functions
* data - files read in
* cache - stored processed files for speedier loading
* scr - working scripts for analysis

The rest are pretty self explanatory





