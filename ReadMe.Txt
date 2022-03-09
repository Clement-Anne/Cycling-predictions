#######################################################
#                                                     #
#         Cycling predictions using ML                #
#                       -                             #
#                     ReadMe                          #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date:
##########################################

############General comments

This project aims at developping a Machine Learning algorithm to 
  predict one-day World Tour road cycling races over 2014-2021.

############Folder structure

-R codes are directly in the project folder with the report
-The rda subfolder includes processed data used for this analysis
-Raw data coming directly from scrapping the HTML code of the
  https://www.procyclingstats.com website, I did not include a
  raw data subdirectory.

############R codes

-This project features 2 R codes to ease the reading.
-R codes should be run in order to provide the outputs.
-Key extracted and/or processed datasets have been saved in the rda subfolder.

###1. Data extraction

-The computation of statistics ("abilities") from past results for a given rider
  before a given race is time-consuming.
  ->To limit the running time of the R codes I computed directly statistics
    on windows we would like to test as tuning parameters.
  ->A prefered approach would have been to compute those statistics for any 
    window size inputed in the corresponding functions. 
  ->However, such approach would have skyrocketted the run time of the code.

###2_3. Data wrangling and analysis

-Datasets processed thanks to the preceding R code are loaded at the
  beginning of the code
-Wrangling part before model assessment
-Assessment of models and results processing


