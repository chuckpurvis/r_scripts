######################################################
#  tidycensus_Example3.r
# Example #3 
# More Complex Examples of TIDYCENSUS
# Multiple Years, Multiple Geographies, Multiple Variables "Stacking"
# Each record (row) is one geography/year combination, with columns for
#  each variable, eg, TotalPop_05_E, TotalPop_07_E, etc.
# Eight variables by 14 years (2005-2018)
#
# This is the multiple year "stacking" approach: less wide, more records
#
#  Prepared by Chuck Purvis, Hayward, California
######################################################

# Step 0: Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)
library(janitor)
library(plyr) # This is needed for a function to concatenate a lot of files in one statement!

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)

#  Example 3.1 through 3.14: Run get_acs for large California Places, 2005-2018
#  Example 3.15:             Concatenate (pancake) data frames: lots of records 
#  Example 3.16:             Merge in a file of Large San Francisco Bay Area places, and subset file.
#  Example 3.17:             Extract data for one place using a string search on the place name
#-------------------------------------------------------------------------------------------

# Set a list of variables to extract in each iteration of get_acs
#  This is a LOT more efficient for variable naming!!!

selvars  <- c(TotalPop_   = "B06001_001", # Total Population
              Med_HHInc_  = "B19013_001", # Median Household Income
              Agg_HHInc_  = "B19025_001", # Aggregate Household Income
              HHldPop_    = "B11002_001", # Population in Households
              Househlds_  = "B25003_001", # Total Households 
              Owner_OccDU_= "B25003_002", # Owner-Occupied Dwelling Units
              Rent_OccDU_ = "B25003_003", # Renter-Occupied Dwelling Units
              Med_HHVal_  = "B25077_001")
#--------------------------------------------------------------------------------------------
temp2005  <- get_acs(survey="acs1", year=2005, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2005$Year       <- "2005"
#--------------------------------------------------------------------------------------------
temp2006  <- get_acs(survey="acs1", year=2006, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2006$Year       <- "2006"
#--------------------------------------------------------------------------------------------
temp2007  <- get_acs(survey="acs1", year=2007, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2007$Year       <- "2007"
#--------------------------------------------------------------------------------------------
temp2008  <- get_acs(survey="acs1", year=2008, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2008$Year       <- "2008"
#--------------------------------------------------------------------------------------------
temp2009  <- get_acs(survey="acs1", year=2009, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2009$Year       <- "2009"
#--------------------------------------------------------------------------------------------
temp2010  <- get_acs(survey="acs1", year=2010, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2010$Year       <- "2010"
#--------------------------------------------------------------------------------------------
temp2011  <- get_acs(survey="acs1", year=2011, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2011$Year       <- "2011"
#--------------------------------------------------------------------------------------------
temp2012  <- get_acs(survey="acs1", year=2012, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2012$Year       <- "2012"
#--------------------------------------------------------------------------------------------
temp2013  <- get_acs(survey="acs1", year=2013, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2013$Year       <- "2013"
#--------------------------------------------------------------------------------------------
temp2014  <- get_acs(survey="acs1", year=2014, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2014$Year       <- "2014"
#--------------------------------------------------------------------------------------------
temp2015  <- get_acs(survey="acs1", year=2015, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2015$Year       <- "2015"
#--------------------------------------------------------------------------------------------
temp2016  <- get_acs(survey="acs1", year=2016, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2016$Year       <- "2016"
#--------------------------------------------------------------------------------------------
temp2017  <- get_acs(survey="acs1", year=2017, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2017$Year       <- "2017"
#--------------------------------------------------------------------------------------------
temp2018  <- get_acs(survey="acs1", year=2018, geography = "place",   state = "CA", 
                     show_call = TRUE,output="wide", variables = selvars)
temp2018$Year       <- "2018"
#--------------------------------------------------------------------------------------------
# temp2019  <- get_acs(survey="acs1", year=2019, geography = "place",   state = "CA", 
#                     show_call = TRUE,output="wide", variables = selvars)
# temp2019$Year       <- "2019"
#--------------------------------------------------------------------------------------------

#  Example 3.15:             Concatenate (pancake) data frames: lots of records 
#  Concatenate All Years .....
#  rbind can only concatenate two dataframes at a time. rbind.fill can do 2-or-more data
#   frames to concatenate. It's a plyr function.
# temp0506 <- rbind(temp2005,temp2006)
# temp0507 <- rbind(temp0506,temp2007)

tempall <- rbind.fill(temp2005,temp2006,temp2007,temp2008,temp2009,
                      temp2010,temp2011,temp2012,temp2013,temp2014,
                      temp2015,temp2016,temp2017,temp2018)

# Add a couple of useful variables!
# need to have a if/then to catch zero values.. work on this later.
# tempall$Avg_HHSize <- tempall$HHldPop_E / tempall$Househlds_E
# tempall$MeanHHInc  <- tempall$Agg_HHInc_E / tempall$Househlds_E

# Sort the Results by GEOID and then by Year

tempalls <- tempall[order(tempall$GEOID,tempall$Year),] 

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel, and applying finishing touches

write.csv(tempalls,"ACS_AllYears_Calif_Places_Stacked.csv")

#  Example 3.16: Merge in a file of Large San Francisco Bay Area places, and subset file.
# Read in a file with the Large SF Bay Area Places, > 65,000 population
# and merge with the All Large California Places

bayplace <- read.csv("BayArea_Places_65K.csv")

Bayplace1 <- merge(bayplace,tempalls,  by = c('NAME'))
Bayplace1 <- Bayplace1[order(Bayplace1$GEOID.x,Bayplace1$Year),] 

write.csv(Bayplace1,"ACS_AllYears_BaseVar_BayArea_Places_Stacked.csv")

dput(names(Bayplace1))

#  Example 3.17:  Extract data for one place using a string search on the place name
#     Extract one place at a time from Bayplace1 
#
Hayward <- filter(Bayplace1, grepl("Hayward",NAME,fixed=TRUE))
Hayward <- Hayward[order(Hayward$Year),] 

Hayward$Avg_HHSize <- Hayward$HHldPop_E / Hayward$Househlds_E
Hayward$MeanHHInc  <- Hayward$Agg_HHInc_E / Hayward$Househlds_E

selvarxxx <- c("Year","NAME", "GEOID.x", "NAME2",  
             "TotalPop_E", "Med_HHInc_E", 
             "Agg_HHInc_E", "HHldPop_E", "Househlds_E", 
             "Owner_OccDU_E", "Rent_OccDU_E", 
             "Med_HHVal_E", "Avg_HHSize", "MeanHHInc" )

Hayward2 <- Hayward[selvarxxx]
write.csv(Hayward2,"ACS_AllYears_BaseVar_Hayward_Stacked.csv")

#####################################################################################