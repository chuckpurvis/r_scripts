###############################################################
#  popestimates_calif_01.r
#  Get Census Bureau population estimates for California & US
#   -- May 23, 2023 --
###############################################################
library(tidyverse)
library(tidycensus)

# Time series for 2010 through 2019.
temp1 <- tidycensus::get_estimates(geography="state", state="CA",
                                   year=2019, 
         # product="population",
         # product="characteristics", breakdown="SEX",
         # product="characteristics", breakdown=c("SEX","AGEGROUP","RACE","HISP"),
           product="housing",
                                   output="wide",
                                   time_series=TRUE)

temp2 <- tidycensus::get_estimates(geography="county", state="CA",
                                   year=2019, 
           #    product="components", # sort by PERIOD, not DATE
                product="characteristics", breakdown=c("RACE","HISP"),
                                   output="wide",
                                   time_series=TRUE) %>% 
             dplyr::arrange(NAME,DATE) # DATE or PERIOD sort

temp3 <- tidycensus::get_estimates(geography="place", state="CA",
                                   year=2019, 
                                   product="population",
                                  #  product="housing", # not available for places
                                   output="wide",
                                   time_series=TRUE) %>% 
        dplyr::arrange(NAME,DATE) %>% 
        dplyr::  mutate(time_est = case_when(
                  DATE == 1 ~    "April 1, 2010 Census",
                  DATE == 2 ~    "April 1, 2010 Est Base",
                  DATE == 3 ~    "July 1, 2010 Estimate",
                  DATE == 4 ~    "July 1, 2011 Estimate",
                  DATE == 5 ~    "July 1, 2012 Estimate",
                  DATE == 6 ~    "July 1, 2013 Estimate",
                  DATE == 7 ~    "July 1, 2014 Estimate",
                  DATE == 8 ~    "July 1, 2015 Estimate",
                  DATE == 9 ~    "July 1, 2016 Estimate",
                  DATE ==10 ~    "July 1, 2017 Estimate",
                  DATE ==11 ~    "July 1, 2018 Estimate",
                  DATE ==12 ~    "July 1, 2019 Estimate" ))

temp3a <- temp3 %>% 
  select (-DENSITY,-DATE) # delete some extra columns

# pivot wide the dataframe so each column is a different year estimate!
temp3b <- pivot_wider(temp3a,names_from=c(time_est),
                     values_from=c(POP))

############################################################################
#  Get population estimates for 2020-2022
############################################################################
temp4 <- tidycensus::get_estimates(geography="state", state="CA",
                                   year=2022, output="wide",
             variables=c("ESTIMATESBASE","POPESTIMATE","BIRTHS","DEATHS","NATURALCHG",
                        "INTERNATIONALMIG","DOMESTICMIG","NETMIG"),
            #                       variables=c("ESTIMATESBASE","POPESTIMATE"),
                                   time_series=TRUE)

temp5 <-tidycensus::get_estimates(geography="county", state="CA",
                                  year=2022, output="wide",
            #                     variables="all", #this doesn't work!
            #                     variables=c("ESTIMATESBASE","POPESTIMATE"),
       variables=c("ESTIMATESBASE","POPESTIMATE","BIRTHS","DEATHS","NATURALCHG",
                   "INTERNATIONALMIG","DOMESTICMIG","NETMIG"),
                                  time_series=TRUE)

temp6 <-tidycensus::get_estimates(geography="place", state="CA",
                                  year=2022, output="wide",
                                  variables=c("ESTIMATESBASE","POPESTIMATE"),
                                  time_series=TRUE)

############################################################################
# Get all places in the United States
temp7 <-tidycensus::get_estimates(geography="place", # state="CA",
                                  year=2022, output="wide",
                                  variables=c("ESTIMATESBASE","POPESTIMATE"),
                                  time_series=TRUE)
temp7sort <- temp7 %>% 
    arrange(-POPESTIMATE2022)

temp7b <- temp7 %>% 
    mutate(change2020_22 = POPESTIMATE2022 - POPESTIMATE2020,
           pctchange = change2020_22 / POPESTIMATE2020 * 100.0) %>% 
    arrange(change2020_22)
#############################################################################
# Get all COUNTIES in the United States
temp8 <-tidycensus::get_estimates(geography="county", # state="CA",
                                  year=2022, output="wide",
                                  variables=c("ESTIMATESBASE","POPESTIMATE"),
                                  time_series=TRUE)
temp8sort <- temp8 %>% 
  arrange(-POPESTIMATE2022)

temp8b <- temp8 %>% 
  mutate(change2020_22 = POPESTIMATE2022 - POPESTIMATE2020,
         pctchange = change2020_22 / POPESTIMATE2020 * 100.0) %>% 
  arrange(change2020_22)

#############################################################################