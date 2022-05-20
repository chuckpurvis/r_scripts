########################################################################
#  ACS_Bicycle_Commuting.r
#  Extract most recent five year ACS data on bicycle commuting, 2016-2020
#    for US, states, counties, places
#  subset for large places with 1,000+ workers
#  subset for large counties with 200,000 total population
#   -- May 18, 2022 --
########################################################################
# Load relevant libraries into R-session.

library(tidyverse)
library(tidycensus)

# setwd("~/Desktop/tidycensus_work/output")
setwd("~/Desktop/tidycensus_work")
getwd()

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)
Sys.getenv("CENSUS_API_KEY")

# Set a list of variables to extract in get_acs

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

# Large US Place, 65K+ population, Ranked by Bicycle Share, 2019
place1 <- tidycensus::get_acs(survey="acs1",geography="place",variables=selvars,
                  year=2019,output='wide') %>% 
          dplyr::arrange(GEOID) %>% 
          dplyr::mutate(share_bicycle = bicycle_E / total_worker_E)

place1a <- place1 %>% 
          dplyr::arrange(desc(share_bicycle)) %>% 
          dplyr::relocate(total_worker_E, bicycle_E, share_bicycle, total_worker_M, bicycle_M, 
                          .after=NAME) %>% 
          dplyr::select(GEOID:total_pop_E) %>% 
          dplyr::mutate(bicycle_SE=bicycle_M/1.645, bicycle_CV = bicycle_SE / bicycle_E)

# All US Places (> 1,000 Workers), Ranked by Bicycle Share, 2016-2020
place2 <- tidycensus::get_acs(survey="acs5",geography="place",variables=selvars,
                  year=2020,output='wide') %>% 
          dplyr::arrange(GEOID) %>% 
          dplyr::mutate(share_bicycle = bicycle_E / total_worker_E)

place2a <- place2 %>% 
  dplyr::arrange(desc(share_bicycle)) %>% 
  dplyr::relocate(total_worker_E, bicycle_E, share_bicycle, total_worker_M, bicycle_M,
                    .after=NAME) %>% 
  dplyr::select(GEOID:total_pop_E) %>% 
  dplyr::filter(total_worker_E > 1000.) %>% 
  dplyr::mutate(bicycle_SE=bicycle_M/1.645, bicycle_CV = bicycle_SE / bicycle_E)

## Given how large the standard errors are, associated with the single year ACS, 
##   I'll focus the remainder of this analysis on the five-year (2016-2020) ACS.....

# All US Counties, Ranked by Bicycle Share, 2016-2020
county1 <- tidycensus::get_acs(survey="acs5",geography="county",variables=selvars,
                              year=2020,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(share_bicycle = bicycle_E / total_worker_E)

county1a <- county1 %>% 
  dplyr::arrange(desc(share_bicycle)) %>% 
  dplyr::relocate(total_worker_E, bicycle_E, share_bicycle, total_worker_M, bicycle_M,
                  .after=NAME) %>% 
  dplyr::select(GEOID:total_pop_E) %>% 
#  dplyr::filter(total_worker_E > 1000.) %>% 
  dplyr::mutate(bicycle_SE=bicycle_M/1.645, bicycle_CV = bicycle_SE / bicycle_E)

# Large US Counties (> 200,000 Total Population), Ranked by Bicycle Share, 2016-2020
county1b <- county1a %>% 
  dplyr::filter(total_pop_E > 200000.)

# All US states, Ranked by Bicycle Share, 2016-2020
state1 <- tidycensus::get_acs(survey="acs5",geography="state",variables=selvars,
                               year=2020,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(share_bicycle = bicycle_E / total_worker_E)

state1a <- state1 %>% 
  dplyr::arrange(desc(share_bicycle)) %>% 
  dplyr::relocate(total_worker_E, bicycle_E, share_bicycle, total_worker_M, bicycle_M,
                  .after=NAME) %>% 
  dplyr::select(GEOID:total_pop_E) %>% 
  #  dplyr::filter(total_worker_E > 1000.) %>% 
  dplyr::mutate(bicycle_SE=bicycle_M/1.645, bicycle_CV = bicycle_SE / bicycle_E)

# Export the data frames to CSV files, for importing to Excel
setwd("~/Desktop/tidycensus_work/output")
write.csv(place2a, "ACS201620_large_places_bicycle_share.csv")
write.csv(county1a,"ACS201620_all_counties_bicycle_share.csv")
write.csv(county1b,"ACS201620_large_counties_bicycle_share.csv")
write.csv(state1a, "ACS201620_us_states_bicycle_share.csv")

########################################################################
