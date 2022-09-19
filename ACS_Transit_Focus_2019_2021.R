########################################################################
#  ACS_Transit_Focus_2019_2021.r
#  Focus on the Transit Commuter Characteristics from the
#    single-year ACS for 2019 and 2021
#  nation, state, county, place
#  IMPORTANT: Recommend using the Means of Transport from DP03, NOT B08006 or C08006
#    because DP03 is more likely to catch data that's suppressed in B or C tables!!
#      -- September 18, 2022 --
########################################################################

library(tidyverse)
library(tidycensus)

# setwd("~/Desktop/tidycensus_work/output")
setwd("~/Desktop/tidycensus_work")
getwd()

# need to show a variable list for the DP tables....
varlist19p <- load_variables(dataset="acs1/profile",year="2019")

varlist21p <- load_variables(dataset="acs1/profile",year="2021")

# Set a list of variables to extract in each iteration of get_acs
# This code works, but defer to the DP03 table.

selvar19 <- c(total_pop_2019_     = "B06001_001", 
              totworker_2019_     = "B08006_001",
              transit_2019_       = "B08006_008")
selvar21 <- c(total_pop_2021_     = "B06001_001", 
              totworker_2021_     = "B08006_001",
              transit_2021_       = "B08006_008")

# Alternatively, use the C08006 data on workers by means of transport!
# This code works, but defer to the DP03 table.

selvar19 <- c(total_pop_2019_     = "B06001_001", 
              totworker_2019_     = "C08006_001",
              transit_2019_       = "C08006_008")
selvar21 <- c(total_pop_2021_     = "B06001_001", 
              totworker_2021_     = "C08006_001",
              transit_2021_       = "C08006_008")

# This is the total population (DP02_0087) and workers by means
#  of transportation from the DP03 table. Least suppression in this table!

selvar19 <- c(total_pop_2019_     = "DP02_0087",
              totworker_2019_     = "DP03_0018",
              transit_2019_       = "DP03_0021")
selvar21 <- c(total_pop_2021_     = "DP02_0088", 
              totworker_2021_     = "DP03_0018",
              transit_2021_       = "DP03_0021")

# tidycensus for 2019 and 2021, single year estimates

##  United States

us_2019 <- get_acs(survey="acs1",geography="us", variables=selvar19,
                   year=2019,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

us_2021 <- get_acs(survey="acs1",geography="us", variables=selvar21,
                   year=2021,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

us_join <- full_join(us_2019,us_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## States

state_2019 <- get_acs(survey="acs1",geography="state", variables=selvar19,
                      year=2019,output='wide') %>% 
             dplyr::arrange(GEOID) %>% 
             dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

state_2021 <- get_acs(survey="acs1",geography="state", variables=selvar21,
                      year=2021,output='wide') %>% 
              dplyr::arrange(GEOID) %>% 
              dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

state_join <- full_join(state_2019,state_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Counties (Large, 65,000+)

county_2019 <- get_acs(survey="acs1",geography="county", variables=selvar19,
                      year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

county_2021 <- get_acs(survey="acs1",geography="county", variables=selvar21,
                      year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

county_join <- full_join(county_2019,county_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Places (Large, 65,000+)

place_2019 <- get_acs(survey="acs1",geography="place", variables=selvar19,
                       year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

place_2021 <- get_acs(survey="acs1",geography="place", variables=selvar21,
                       year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

place_join <- full_join(place_2019,place_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
#  Export these data frames into CSV format for further analysis/reporting in Excel
###################################################################################

setwd("~/Desktop/tidycensus_work/output")

write.csv(us_join,    "ACS_us_transit_2019_2021.csv")
write.csv(state_join, "ACS_state_transit_2019_2021.csv")
write.csv(county_join,"ACS_county_transit_2019_2021.csv")
write.csv(place_join, "ACS_place_transit_2019_2021.csv")

###################################################################################
