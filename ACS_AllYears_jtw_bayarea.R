########################################################################
#  ACS_AllYears_jtw_bayarea2.r
#  Use the purrr function map_dfr to iterate tidycensus call for 1-year, 5-year ACS data.
#   Data on Means of Transportation to Work Variables, All ACS years     
#   San Francisco Bay Area, nine-county region
#
#   American Community Survey, 1-year, 2005-2019, 2021-2022 data
#
#  Data is meant to be "stacked" with one row/record for each geography/year combination
##    -- September 14, 2023 -- 
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

years <- c(2006:2019,2021,2022)
names(years) <- years

# Bay Area Counties, 2006-2022, single year ACS (Population 65K+)
bayco1 <- purrr::map_dfr(years, ~{ get_acs(survey = "acs1",
                 geography = "county", state = "CA",
                 county=c("01","13","41","55","75","81","85","95","97"),
                 variables = selvars, output='wide',
                 year = .x)}, .id = "year") %>% 
         dplyr::arrange(GEOID,year)

bayco2 <- bayco1 %>% 
    group_by(year) %>% 
    mutate(carpool_2p = carpool_2_E + carpool_3_E + carpool_4p_E) %>% 
    summarize(totpop =sum(total_pop_E),
              hhpop  =sum(hh_pop_E),
              households = sum(households_E),
              total_worker = sum(total_worker_E),
              drove_alone  = sum(drove_alone_E),
              carpool_2p   = sum(carpool_2p),
              transit      = sum(transit_E),
              bicycle      = sum(bicycle_E),
              walked       = sum(walked_E),
              other3       = sum(other3_E),
              at_home      = sum(at_home_E))

# This bind_rows appends a sum total to every variable, including the MOEs.
#   and gives the name "Bay Area Total" to the summaries

bayco3 <- bayco1 %>% 
  group_by(year) %>% 
  mutate(carpool_2p = carpool_2_E + carpool_3_E + carpool_4p_E) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Bay Area Total')))

########################################################################
## Output the ACS single year databases....
########################################################################
# note that my working directory (wd) is for a Mac computer.
setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel

write.csv(bayco3,"ACS_bayarea_jtw_2006_2022.csv")

###############################################################
