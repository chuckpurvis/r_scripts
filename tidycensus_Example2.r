######################################################
#  tidycensus_Example2.r
# Example #2 
# More Complex Examples of TIDYCENSUS
# Multiple Years, Multiple Geographies, Multiple Variables
# Each record (row) is one geography, with columns for
#  each variable/year combination, eg, TotalPop_05_E, TotalPop_07_E, etc.
# Eight variables by 14 years (2005-2018)
#  Prepared by Chuck Purvis, Hayward, California
######################################################

# Step 0: Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)

# The get_acs function is run for each year of the single-year ACS data, from 2005 to 2018.
#  Note that group quarters data was not collected in 2005, but started in 2006.
#  Note the "_05_" included in the variable name in the first data "pull". That's a mnemonic
#    device that tells us it's for the year 2005.

#  Example 2.1 through 2.14: Run get_acs for large California Places, 2005-2018
#  Example 2.15:             Merge together data frames into a VERY wide database...lots of columns!
#  Example 2.16:             Merge in a file of Large San Francisco Bay Area places, and subset file.
#-------------------------------------------------------------------------------------------

place05  <- get_acs(survey="acs1", year=2005, geography = "place",   state = "CA", 
                       show_call = TRUE,output="wide",
                       variables = c(TotalPop_05_   = "B06001_001", # Total Population
                                     Med_HHInc_05_  = "B19013_001", # Median Household Income
                                     Agg_HHInc_05_  = "B19025_001", # Aggregate Household Income
                                     HHldPop_05_    = "B11002_001", # Population in Households
                                     Househlds_05_  = "B25003_001", # Total Households 
                                     Owner_OccDU_05_= "B25003_002", # Owner-Occupied Dwelling Units
                                     Rent_OccDU_05_ = "B25003_003", # Renter-Occupied Dwelling Units
                                     Med_HHVal_05_  = "B25077_001")) # Median Value of Owner-Occ DUs
place05$Avg_HHSize_05 <- place05$HHldPop_05_E / place05$Househlds_05_E
place05$MeanHHInc_05  <- place05$Agg_HHInc_05_E / place05$Househlds_05_E
#-------------------------------------------------------------------------------------------
place06  <- get_acs(survey="acs1", year=2006, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_06_   = "B06001_001", # Total Population
                                  Med_HHInc_06_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_06_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_06_    = "B11002_001", # Population in Households
                                  Househlds_06_  = "B25003_001", # Total Households 
                                  Owner_OccDU_06_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_06_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_06_  = "B25077_001")) # Median Value of Owner-Occ DUs
place06$Avg_HHSize_06 <- place06$HHldPop_06_E / place06$Househlds_06_E
place06$MeanHHInc_06  <- place06$Agg_HHInc_06_E / place06$Househlds_06_E
#-------------------------------------------------------------------------------------------
place07  <- get_acs(survey="acs1", year=2007, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_07_   = "B06001_001", # Total Population
                                  Med_HHInc_07_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_07_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_07_    = "B11002_001", # Population in Households
                                  Househlds_07_  = "B25003_001", # Total Households 
                                  Owner_OccDU_07_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_07_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_07_  = "B25077_001")) # Median Value of Owner-Occ DUs
place07$Avg_HHSize_07 <- place07$HHldPop_07_E / place07$Househlds_07_E
place07$MeanHHInc_07  <- place07$Agg_HHInc_07_E / place07$Househlds_07_E
#-------------------------------------------------------------------------------------------
place08  <- get_acs(survey="acs1", year=2008, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_08_   = "B06001_001", # Total Population
                                  Med_HHInc_08_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_08_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_08_    = "B11002_001", # Population in Households
                                  Househlds_08_  = "B25003_001", # Total Households 
                                  Owner_OccDU_08_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_08_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_08_  = "B25077_001")) # Median Value of Owner-Occ DUs
place08$Avg_HHSize_08 <- place08$HHldPop_08_E / place08$Househlds_08_E
place08$MeanHHInc_08  <- place08$Agg_HHInc_08_E / place08$Househlds_08_E
#-------------------------------------------------------------------------------------------
place09  <- get_acs(survey="acs1", year=2009, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_09_   = "B06001_001", # Total Population
                                  Med_HHInc_09_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_09_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_09_    = "B11002_001", # Population in Households
                                  Househlds_09_  = "B25003_001", # Total Households 
                                  Owner_OccDU_09_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_09_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_09_  = "B25077_001")) # Median Value of Owner-Occ DUs
place09$Avg_HHSize_09 <- place09$HHldPop_09_E / place09$Househlds_09_E
place09$MeanHHInc_09  <- place09$Agg_HHInc_09_E / place09$Househlds_09_E
#-------------------------------------------------------------------------------------------
place10  <- get_acs(survey="acs1", year=2010, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_10_   = "B06001_001", # Total Population
                                  Med_HHInc_10_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_10_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_10_    = "B11002_001", # Population in Households
                                  Househlds_10_  = "B25003_001", # Total Households 
                                  Owner_OccDU_10_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_10_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_10_  = "B25077_001")) # Median Value of Owner-Occ DUs
place10$Avg_HHSize_10 <- place10$HHldPop_10_E / place10$Househlds_10_E
place10$MeanHHInc_10  <- place10$Agg_HHInc_10_E / place10$Househlds_10_E
#-------------------------------------------------------------------------------------------
place11  <- get_acs(survey="acs1", year=2011, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_11_   = "B06001_001", # Total Population
                                  Med_HHInc_11_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_11_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_11_    = "B11002_001", # Population in Households
                                  Househlds_11_  = "B25003_001", # Total Households 
                                  Owner_OccDU_11_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_11_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_11_  = "B25077_001")) # Median Value of Owner-Occ DUs
place11$Avg_HHSize_11 <- place11$HHldPop_11_E / place11$Househlds_11_E
place11$MeanHHInc_11  <- place11$Agg_HHInc_11_E / place11$Househlds_11_E
#-------------------------------------------------------------------------------------------
place12  <- get_acs(survey="acs1", year=2012, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_12_   = "B06001_001", # Total Population
                                  Med_HHInc_12_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_12_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_12_    = "B11002_001", # Population in Households
                                  Househlds_12_  = "B25003_001", # Total Households 
                                  Owner_OccDU_12_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_12_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_12_  = "B25077_001")) # Median Value of Owner-Occ DUs
place12$Avg_HHSize_12 <- place12$HHldPop_12_E / place12$Househlds_12_E
place12$MeanHHInc_12  <- place12$Agg_HHInc_12_E / place12$Househlds_12_E
#-------------------------------------------------------------------------------------------
place13  <- get_acs(survey="acs1", year=2013, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_13_   = "B06001_001", # Total Population
                                  Med_HHInc_13_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_13_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_13_    = "B11002_001", # Population in Households
                                  Househlds_13_  = "B25003_001", # Total Households 
                                  Owner_OccDU_13_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_13_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_13_  = "B25077_001")) # Median Value of Owner-Occ DUs
place13$Avg_HHSize_13 <- place13$HHldPop_13_E / place13$Househlds_13_E
place13$MeanHHInc_13  <- place13$Agg_HHInc_13_E / place13$Househlds_13_E
#-------------------------------------------------------------------------------------------
place14  <- get_acs(survey="acs1", year=2014, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_14_   = "B06001_001", # Total Population
                                  Med_HHInc_14_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_14_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_14_    = "B11002_001", # Population in Households
                                  Househlds_14_  = "B25003_001", # Total Households 
                                  Owner_OccDU_14_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_14_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_14_  = "B25077_001")) # Median Value of Owner-Occ DUs
place14$Avg_HHSize_14 <- place14$HHldPop_14_E / place14$Househlds_14_E
place14$MeanHHInc_14  <- place14$Agg_HHInc_14_E / place14$Househlds_14_E
#-------------------------------------------------------------------------------------------
place15  <- get_acs(survey="acs1", year=2015, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_15_   = "B06001_001", # Total Population
                                  Med_HHInc_15_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_15_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_15_    = "B11002_001", # Population in Households
                                  Househlds_15_  = "B25003_001", # Total Households 
                                  Owner_OccDU_15_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_15_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_15_  = "B25077_001")) # Median Value of Owner-Occ DUs
place15$Avg_HHSize_15 <- place15$HHldPop_15_E / place15$Househlds_15_E
place15$MeanHHInc_15  <- place15$Agg_HHInc_15_E / place15$Househlds_15_E
#-------------------------------------------------------------------------------------------
place16  <- get_acs(survey="acs1", year=2016, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_16_   = "B06001_001", # Total Population
                                  Med_HHInc_16_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_16_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_16_    = "B11002_001", # Population in Households
                                  Househlds_16_  = "B25003_001", # Total Households 
                                  Owner_OccDU_16_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_16_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_16_  = "B25077_001")) # Median Value of Owner-Occ DUs
place16$Avg_HHSize_16 <- place16$HHldPop_16_E / place16$Househlds_16_E
place16$MeanHHInc_16  <- place16$Agg_HHInc_16_E / place16$Househlds_16_E
#-------------------------------------------------------------------------------------------
place17  <- get_acs(survey="acs1", year=2017, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_17_   = "B06001_001", # Total Population
                                  Med_HHInc_17_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_17_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_17_    = "B11002_001", # Population in Households
                                  Househlds_17_  = "B25003_001", # Total Households 
                                  Owner_OccDU_17_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_17_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_17_  = "B25077_001")) # Median Value of Owner-Occ DUs
place17$Avg_HHSize_17 <- place17$HHldPop_17_E / place17$Househlds_17_E
place17$MeanHHInc_17  <- place17$Agg_HHInc_17_E / place17$Househlds_17_E
#-------------------------------------------------------------------------------------------
place18  <- get_acs(survey="acs1", year=2018, geography = "place",   state = "CA", 
                    show_call = TRUE,output="wide",
                    variables = c(TotalPop_18_   = "B06001_001", # Total Population
                                  Med_HHInc_18_  = "B19013_001", # Median Household Income
                                  Agg_HHInc_18_  = "B19025_001", # Aggregate Household Income
                                  HHldPop_18_    = "B11002_001", # Population in Households
                                  Househlds_18_  = "B25003_001", # Total Households 
                                  Owner_OccDU_18_= "B25003_002", # Owner-Occupied Dwelling Units
                                  Rent_OccDU_18_ = "B25003_003", # Renter-Occupied Dwelling Units
                                  Med_HHVal_18_  = "B25077_001")) # Median Value of Owner-Occ DUs
place18$Avg_HHSize_18 <- place18$HHldPop_18_E / place18$Househlds_18_E
place18$MeanHHInc_18  <- place18$Agg_HHInc_18_E / place18$Househlds_18_E
#####################################################################################
#  Example 2.15: Merge together data frames into a VERY wide database...lots of columns!
# Merge the dataframes, adding a year in each step. All=TRUE is needed if # of places is different.
#
# (R-language newbie script...There are probably more terse/exotic ways of doing this!)

place0506 <- merge(place05,  place06, by = c('GEOID','NAME'), all=TRUE)
place0507 <- merge(place0506,place07, by = c('GEOID','NAME'), all=TRUE)
place0508 <- merge(place0507,place08, by = c('GEOID','NAME'), all=TRUE)
place0509 <- merge(place0508,place09, by = c('GEOID','NAME'), all=TRUE)
place0510 <- merge(place0509,place10, by = c('GEOID','NAME'), all=TRUE)
place0511 <- merge(place0510,place11, by = c('GEOID','NAME'), all=TRUE)
place0512 <- merge(place0511,place12, by = c('GEOID','NAME'), all=TRUE)
place0513 <- merge(place0512,place13, by = c('GEOID','NAME'), all=TRUE)
place0514 <- merge(place0513,place14, by = c('GEOID','NAME'), all=TRUE)
place0515 <- merge(place0514,place15, by = c('GEOID','NAME'), all=TRUE)
place0516 <- merge(place0515,place16, by = c('GEOID','NAME'), all=TRUE)
place0517 <- merge(place0516,place17, by = c('GEOID','NAME'), all=TRUE)
place0518 <- merge(place0517,place18, by = c('GEOID','NAME'), all=TRUE)

place_all <- place0518

View(place_all)

# The following functions output useful lists to the R-studio console which can then be edited
names(place_all)
dput(names(place_all)) # most useful for subsetting variables

# The purpose here is to re-order and select variables into a much more compact 
#  database, for eventual exporting into a CSV file, and then into Excel for finishing touches.

selvars <- c("GEOID", "NAME",
             "TotalPop_05_E", "TotalPop_06_E", "TotalPop_07_E", "TotalPop_08_E",
             "TotalPop_09_E", "TotalPop_10_E", "TotalPop_11_E", "TotalPop_12_E",
             "TotalPop_13_E", "TotalPop_14_E", "TotalPop_15_E", "TotalPop_16_E",
             "TotalPop_17_E", "TotalPop_18_E")

# note the brackets for outputing new data frame from previous data frame....
place_all2 <- place_all[selvars]

# View the Selected Variables Table
View(place_all2)

# Set directory for exported data files, MacOS directory style

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel, and applying finishing touches

write.csv(place_all2,"ACS_AllYears_TotalPop_Calif_Places.csv")
write.csv(place_all, "ACS_AllYears_BaseVar_Calif_Places.csv")
#####################################################################################
#  Example 2.16: Merge in a file of Large San Francisco Bay Area places, and subset file.
# Read in a file with the Large SF Bay Area Places, > 65,000 population
# and merge with the All Large California Places

bayplace <- read.csv("BayArea_Places_65K.csv")

Bayplace1 <- merge(bayplace,place_all,  by = c('NAME'))
Bayplace2 <- merge(bayplace,place_all2, by = c('NAME'))

write.csv(Bayplace1,"ACS_AllYears_BaseVar_BayArea_Places.csv")
write.csv(Bayplace2,"ACS_AllYears_TotalPop_BayArea_Places.csv")

#####################################################################################