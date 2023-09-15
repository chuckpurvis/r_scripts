########################################################################
#  ACS_AllYears_jtw_add2022.r
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
##    -- March 17, 2022 --
##    -- September 15, 2022 -- Add the new 1-year 2021 ACS data!!!
##    -- September 14, 2023 -- Add the new 1-year 2022 ACS data.
########################################################################

# These R packages were installed in previous sessions, so no need to re-install
# install.packages("tidyverse")
# install.packages("tidycensus")
# install.packages("janitor")
# install.packages("purrr")

# Load relevant libraries into R-session.

library(tidyverse)
library(tidycensus)
library(janitor)
library(xlsx)

# setwd("~/Desktop/tidycensus_work/output")
setwd("~/Desktop/tidycensus_work")
getwd()

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)
Sys.getenv("CENSUS_API_KEY")

acstabs <- load_variables(2022,"acs1")
acs_profiletabs <- load_variables(2022,"acs1/profile")

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

# selected variables from the DP table 3 - socio-economic characteristics
selvars_p <- c(total_worker_ = "DP03_0018",
               drove_alone_  = "DP03_0019",
               carpool_2p_   = "DP03_0020",
               transit_      = "DP03_0021",
               walked_       = "DP03_0022",
               other_        = "DP03_0023",
               at_home_      = "DP03_0024")
###################################################################
# Iterate on ACS 1-year data, using the purrr function "map_dfr"
###################################################################
test1 <- get_acs(survey="acs1",geography="place",state="CA",variables=selvars,
                year=2022,output='wide') %>% 
         dplyr::arrange(GEOID)

# This doesn't work for 2005 since the ACS 2005 B08006 table
#  had extra records for Carpools: carpool 4, carpool 5-6, and carpool 7+

# years <- 2005:2020
# Testing 2020 to see what kind of error message is generated....

# Running 2020 single year ACS gives an "Error: Your API call has errors".
#   Perhaps add a WARNING to users to NOT to ask for 2020 ACS1 data!

# years <- 2006:2020  # Asking for 2020 ACS1 will bomb.

# years <- 2006:2019  

years <- c(2006:2019,2021,2022)
names(years) <- years

# Testing skip-a-year to see what how we'll handle 2020-X data.....
# years <- c(2009:2017,2019)
#  This does indeed work AOK.....

# United States, 2006-2022, single year ACS
zeta1a <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                                           geography = "us", # state = "CA",
                                           variables = selvars, output='wide',
                                           year = .x)}, .id = "year")

zeta1a <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                 geography = "us", # state = "CA",
                 variables = selvars_p, output='wide',
                 year = .x)}, .id = "year")

# All US States, 2006-2022, single year ACS
zeta2 <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                 geography = "state", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

zeta2a <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                                          geography = "state", # state = "CA",
                                          variables = selvars_p, output='wide',
                                          year = .x)}, .id = "year") %>% 
  dplyr::arrange(GEOID,year)

# Large US Counties, 2006-2022, single year ACS (Population 65K+)
zeta3 <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                 geography = "county", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

zeta3a <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                                          geography = "county", # state = "CA",
                                          variables = selvars_p, output='wide',
                                          year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

# Large US Places, 2006-2022, single year ACS (Population 65K+)
zeta4 <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                 geography = "place", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

zeta4a <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                                          geography = "place", # state = "CA",
                                          variables = selvars_p, output='wide',
                                          year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

########################################################################
## Output the ACS single year databases....
########################################################################

# note that my working directory (wd) is for a Mac computer.
setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel

write.csv(zeta1,"ACS_AllYears_US_jtw_2006_2022.csv")
write.csv(zeta2,"ACS_AllYears_states_jtw_2006_2022.csv")
write.csv(zeta3,"ACS_AllYears_counties_jtw_2006_2022.csv")
write.csv(zeta4,"ACS_AllYears_places_jtw_2006_2022.csv")

# The 9/15/22 edits / run ends at this location..... 5 year data (2017-2021) isn't yet available

# just write them in a master XLSX workbook.....
#    Grrrr... this doesn't work... java heap space error, whatever that is.....
write.xlsx2(zeta1, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="nation", append=FALSE)
write.xlsx2(zeta2, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="states", append=TRUE)
write.xlsx2(zeta3, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="counties", append=TRUE)
write.xlsx2(zeta4, "ACS_AllYears_journey_to_work_master_2006_2019.xlsx",
           sheetName="places", append=TRUE)

###################################################################
# Iterate on ACS 5-year data, using the purrr function "map_dfr"
###################################################################

# This works ok:
test2 <- get_acs(survey="acs5",year=2020,
                 geography="us", variables="B08006_001", output="wide")

# with years 2010, 2014 and 2019, this is a one-year overlap between
#   the first two periods.... 2009 --- bombed for some reason.....

# The new 2016-2020 ACS 5-year data is scheduled for release 3/17/22.....
#  Data for 2006-2010, 2011-2015, 2016-2020:
years5 <- c(2010,2015,2020)
names(years5) <- years5

## years5 <- c(2010,2014,2019) ... this works, too, but overlapping periods.


# This code is choking on selvars (means of transportation to work)
#  getting an "unknown variable 'B08006_001E'" error??? :(

# re-ran... it works on 2006-2010; 2010-2014; 2015-2019..... weird!!

# National, ACS 5-years (2006-2010, 2011-2015, 2016-2020)
zeta5 <- purrr::map_dfr(years5, ~{ get_acs(survey = "acs5",
                 geography = "us", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year")

# State, ACS 5-years (2006-2010, 2011-2015, 2016-2020)
zeta6 <- purrr::map_dfr(years5, ~{ get_acs(survey = "acs5",
                 geography = "state", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

# County, ACS 5-years (2006-2010, 2011-2015, 2016-2020)
zeta7 <- purrr::map_dfr(years5, ~{ get_acs(survey = "acs5",
                 geography = "county", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

# Place, ACS 5-years (2006-2010, 2011-2015, 2016-2020)
zeta8 <- purrr::map_dfr(years5, ~{ get_acs(survey = "acs5",
                 geography = "place", # state = "CA",
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
        dplyr::arrange(GEOID,year)

# This last dataframe, zeta8, has 90,996 records... pretty big!

########################################################################
## Output the ACS five year databases....
########################################################################

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel

write.csv(zeta5,"ACS_5Years_US_meansoftransportation_2010_2015_2020.csv")
write.csv(zeta6,"ACS_5Years_states_meansoftransportation_2010_2015_2020.csv")
write.csv(zeta7,"ACS_5Years_counties_meansoftransportation_2010_2015_2020.csv")
write.csv(zeta8,"ACS_5Years_places_meansoftransportation_2010_2015_2020.csv")

########################################################################
