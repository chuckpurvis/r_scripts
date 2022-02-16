# Initial upload to GITHUB on 2/16/2022

########################################################################
#  ACS_AllYears_Journey2Work_purrr.r
#  Use the purrr function map_dfr to iterate tidycensus call for 1-year, 5-year ACS data.
#   Data on Means of Transportation to Work Variables, All ACS years     
#   Nation and US States and counties and places (added 1/28/22)
#
#   American Community Survey, 1-year, 2005-2019 data
#   ACS 5-year data: 2005-2009, 2010-2014, 2015-2019
#   ACS 5-year data: 2006-2010, 2010-2014, 2015-2019 (this works for B08006....)
##  ACS 5-year data: 2006-2010, 2011-2015, 2016-2020 @@ should work after 3/17/22.
#
#  Data is meant to be "stacked" with one row/record for each geography/year combination
#     -- January 18, 2022 --
##    -- January 28, 2022 -- (Add County and Place!!!)
##    -- February 7, 2022 -- "skip-a-year" is working. And new sets of
##                           nonoverlapping 5-yrs when 2016-2020 is released 3/17/22
########################################################################

# These R packages were installed in previous sessions, so no need to re-install
# install.packages("tidyverse")
# install.packages("tidycensus")
# install.packages("janitor")
# install.packages("purrr")

# Load relevant libraries into R-session.

library(purrr)
library(tidyverse)
library(tidycensus)
library(janitor)
library(plyr)
library(dplyr)
library(xlsx)

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)
Sys.getenv("CENSUS_API_KEY")

# Set a list of variables to extract in each iteration of get_acs

# Total Population is problematic for 2005-09 since GQ pop wasn't collected in 2005....

# selvars2 was a previous "best of" variables for introductory analyses....
selvars2  <- c(# TotalPop_   = "B06001_001", # Total Population
              Med_HHInc_  = "B19013_001", # Median Household Income
              Agg_HHInc_  = "B19025_001", # Aggregate Household Income
              HHldPop_    = "B11002_001", # Population in Households
              Househlds_  = "B25003_001", # Total Households 
              Owner_OccDU_= "B25003_002", # Owner-Occupied Dwelling Units
              Rent_OccDU_ = "B25003_003", # Renter-Occupied Dwelling Units
              Med_HHVal_  = "B25077_001")

selvars <- c(total_pop_     = "B06001_001", # Total Population
             hh_pop_        = "B11002_001", # Population in Households
             households_    = "B25003_001", # Total Households
             agg_commtime_  = "B08013_001", # Aggregate Travel Time to Work
             agg_vehicle_   = "B08015_001", # Aggregate Vehicles Used in Journey-to-Work
             total_worker_  = "B08006_001",
             drove_alone_   = "B08006_003",
             carpool_2_     = "B08006_005",
             carpool_3_     = "B08006_006",
             carpool_4p_    = "B08006_007",
             transit_       = "B08006_008",
             bicycle_       = "B08006_014",
             walked_        = "B08006_015",
             other3_        = "B08006_016", # taxicab, motorcycle, other means
             at_home_       = "B08006_017",
             bus_           = "B08006_009",
             subway_        = "B08006_010",
             railroad_      = "B08006_011",
             lightrail_     = "B08006_012",
             ferry_         = "B08006_013")

###################################################################
# Iterate on ACS 1-year data, using the purrr function "map_dfr"
###################################################################
test1 <- get_acs(survey="acs1",geography="place",state="CA",variables=selvars,
                year=2019,output='wide') 

# This doesn't work for 2005 since the ACS 2005 B08006 table
#  had extra records for Carpools: carpool 4, carpool 5-6, and carpool 7+

# years <- 2005:2019
years <- 2006:2019

# Testing skip-a-year to see what how we'll handle 2020-X data.....
years <- c(2009:2017,2019)

names(years) <- years

# United States, 2006-2019, single year ACS
zeta1 <- map_dfr(years, ~{ get_acs(survey = "acs1",
    geography = "us", # state = "CA",
    variables = selvars, output='wide',
    year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta2 <- zeta1[order(zeta1$GEOID,zeta1$year),] 

# All US States, 2006-2019, single year ACS
zeta3 <- map_dfr(years, ~{ get_acs(survey = "acs1",
    geography = "state", # state = "CA",
    variables = selvars, output='wide',
    year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta4 <- zeta3[order(zeta3$GEOID,zeta3$year),] 

# Large US Counties, 2006-2019, single year ACS (Population 65K+)
zeta5 <- map_dfr(years, ~{ get_acs(survey = "acs1",
    geography = "county", # state = "CA",
    variables = selvars, output='wide',
    year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta6 <- zeta5[order(zeta5$GEOID,zeta5$year),] 

# Large US Places, 2006-2019, single year ACS (Population 65K+)
zeta7 <- map_dfr(years, ~{ get_acs(survey = "acs1",
    geography = "place", # state = "CA",
    variables = selvars, output='wide',
    year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta8 <- zeta7[order(zeta7$GEOID,zeta7$year),] 
########################################################################
## Output the ACS single year databases....
########################################################################

# note that my working directory (wd) is for a Mac computer.
setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel

write.csv(zeta2,"ACS_AllYears_US_meansoftransportation_2006_2019.csv")
write.csv(zeta4,"ACS_AllYears_states_meansoftransportation_2006_2019.csv")
write.csv(zeta6,"ACS_AllYears_counties_meansoftransportation_2006_2019.csv")
write.csv(zeta8,"ACS_AllYears_places_meansoftransportation_2006_2019.csv")

# just write them in a master XLSX workbook.....
#    Grrrr... this doesn't work... java heap space error, whatever that is.....
write.xlsx2(zeta2, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="nation", append=FALSE)
write.xlsx2(zeta4, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="states", append=TRUE)
write.xlsx2(zeta6, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="counties", append=TRUE)
write.xlsx2(zeta8, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="places", append=TRUE)
####################################################################################

###################################################################
# Iterate on ACS 5-year data, using the purrr function "map_dfr"
###################################################################

# this doesn't work....
test5 <- get_acs(survey="acs5",year=2009,
                 geography="us", variables="B08006001", output="wide")

# with years 2010, 2014 and 2019, this is a one-year overlap between
#   the first two periods.... 2009 --- bombed for some reason.....

# The new 2016-2020 ACS 5-year data is scheduled for release 3/17/22.....
#  Data for 2006-2010, 2011-2015, 2016-2020:
# years5 <- c(2010,2015,2020)

years5 <- c(2010,2014,2019)
names(years5) <- years5

# This code is choking on selvars (means of transportation to work)
#  getting an "unknown variable 'B08006_001E'" error??? :(

# re-ran... it works on 2006-2010; 2010-2014; 2015-2019..... weird!!

# National, ACS 5-years (2006-2010, 2010-2014, 2015-2019)
zeta9 <- map_dfr(years5, ~{ get_acs(survey = "acs5",
            geography = "us", # state = "CA",
            variables = selvars, output='wide',
            year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta10 <- zeta9[order(zeta9$GEOID,zeta9$year),] 

# State, ACS 5-years (2006-2010, 2010-2014, 2015-2019)
zeta11 <- map_dfr(years5, ~{ get_acs(survey = "acs5",
            geography = "state", # state = "CA",
            variables = selvars, output='wide',
            year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta12 <- zeta11[order(zeta11$GEOID,zeta11$year),] 

# County, ACS 5-years (2006-2010, 2010-2014, 2015-2019)
zeta13 <- map_dfr(years5, ~{ get_acs(survey = "acs5",
             geography = "county", # state = "CA",
             variables = selvars, output='wide',
             year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta14 <- zeta13[order(zeta13$GEOID,zeta13$year),] 

# Place, ACS 5-years (2006-2010, 2010-2014, 2015-2019)
zeta15 <- map_dfr(years5, ~{ get_acs(survey = "acs5",
             geography = "place", # state = "CA",
             variables = selvars, output='wide',
             year = .x)}, .id = "year")

# Sort the Results by GEOID and then by year
zeta16 <- zeta15[order(zeta15$GEOID,zeta15$year),] 

# This last dataframe, zeta16, has 88,636 records... pretty big!

########################################################################
## Output the ACS five year databases....
########################################################################

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel

write.csv(zeta10,"ACS_5Years_US_meansoftransportation_2006_2014_2019.csv")
write.csv(zeta12,"ACS_5Years_states_meansoftransportation_2006_2014_2019.csv")
write.csv(zeta14,"ACS_5Years_counties_meansoftransportation_2006_2014_2019.csv")
write.csv(zeta16,"ACS_5Years_places_meansoftransportation_2006_2014_2019.csv")

########################################################################
