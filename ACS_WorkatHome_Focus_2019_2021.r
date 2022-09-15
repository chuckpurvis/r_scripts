########################################################################
#  ACS_WorkatHome_Focus_2019_2021.r
#  Focus on the Work-at-Home Characteristics from the
#    single-year ACS for 2019 and 2021
#  nation, state, county, place
#  IMPORTANT: Recommend using the Means of Transport from DP03, NOT B08006 or C08006
#    because DP03 is more likely to catch data that's suppressed in B or C tables!!
#      -- September 15, 2022 --
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

selvar19 <- c(total_pop_2019_     = "B06001_001", 
              totworker_2019_     = "B08006_001",
              at_home_2019_       = "B08006_017")
selvar21 <- c(total_pop_2021_     = "B06001_001", 
              totworker_2021_     = "B08006_001",
              at_home_2021_       = "B08006_017")

# Alternatively, use the C08006 data on workers by means of transport!

selvar19 <- c(total_pop_2019_     = "B06001_001", 
              totworker_2019_     = "C08006_001",
              at_home_2019_       = "C08006_012")
selvar21 <- c(total_pop_2021_     = "B06001_001", 
              totworker_2021_     = "C08006_001",
              at_home_2021_       = "C08006_012")

# Alternatively, use the DP02/DP03 table values... Why are they different???
#   My guess is that the DP tables serve as a "collapsed" version of the detailed "B" tables!
# This is the table Kyle Walker used in this morning's tweet.

selvar19 <- c(total_pop_2019_     = "DP02_0087",  # oops... should be 0087.....
              totworker_2019_     = "DP03_0018",
              at_home_2019_       = "DP03_0024")
selvar21 <- c(total_pop_2021_     = "DP02_0088", 
              totworker_2021_     = "DP03_0018",
              at_home_2021_       = "DP03_0024")

# tidycensus for 2019 and 2021, single year estimates

##  United States

us_2019 <- get_acs(survey="acs1",geography="us", variables=selvar19,
                   year=2019,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(at_home_share_2019 = at_home_2019_E / totworker_2019_E)

us_2021 <- get_acs(survey="acs1",geography="us", variables=selvar21,
                   year=2021,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(at_home_share_2021 = at_home_2021_E / totworker_2021_E)

us_join <- full_join(us_2019,us_2021,by="GEOID") %>% 
           dplyr::relocate(at_home_share_2021,.after=GEOID) %>% 
           dplyr::relocate(at_home_share_2019,.after=GEOID)

##################################################################################
## States

state_2019 <- get_acs(survey="acs1",geography="state", variables=selvar19,
                      year=2019,output='wide') %>% 
             dplyr::arrange(GEOID) %>% 
             dplyr::mutate(at_home_share_2019 = at_home_2019_E / totworker_2019_E)

state_2021 <- get_acs(survey="acs1",geography="state", variables=selvar21,
                      year=2021,output='wide') %>% 
              dplyr::arrange(GEOID) %>% 
              dplyr::mutate(at_home_share_2021 = at_home_2021_E / totworker_2021_E)

state_join <- full_join(state_2019,state_2021,by="GEOID") %>% 
              dplyr::relocate(at_home_share_2021,.after=GEOID) %>% 
              dplyr::relocate(at_home_share_2019,.after=GEOID)

##################################################################################
## Counties (Large, 65,000+)

county_2019 <- get_acs(survey="acs1",geography="county", variables=selvar19,
                      year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(at_home_share_2019 = at_home_2019_E / totworker_2019_E)

county_2021 <- get_acs(survey="acs1",geography="county", variables=selvar21,
                      year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(at_home_share_2021 = at_home_2021_E / totworker_2021_E)

county_join <- full_join(county_2019,county_2021,by="GEOID") %>% 
  dplyr::relocate(at_home_share_2021,.after=GEOID) %>% 
  dplyr::relocate(at_home_share_2019,.after=GEOID)

##################################################################################
## Places (Large, 65,000+)

place_2019 <- get_acs(survey="acs1",geography="place", variables=selvar19,
                       year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(at_home_share_2019 = at_home_2019_E / totworker_2019_E)

place_2021 <- get_acs(survey="acs1",geography="place", variables=selvar21,
                       year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(at_home_share_2021 = at_home_2021_E / totworker_2021_E)

place_join <- full_join(place_2019,place_2021,by="GEOID") %>% 
  dplyr::relocate(at_home_share_2021,.after=GEOID) %>% 
  dplyr::relocate(at_home_share_2019,.after=GEOID)

##################################################################################
#  Export these data frames into CSV format for further analysis/reporting in Excel
###################################################################################

setwd("~/Desktop/tidycensus_work/output")

write.csv(us_join,    "ACS_us_workathome_2019_2021.csv")
write.csv(state_join, "ACS_state_workathome_2019_2021.csv")
write.csv(county_join,"ACS_county_workathome_2019_2021.csv")
write.csv(place_join, "ACS_place_workathome_2019_2021.csv")

###################################################################################
