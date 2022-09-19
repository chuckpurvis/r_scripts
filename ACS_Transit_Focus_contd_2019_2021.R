########################################################################
#  ACS_Transit_Foccsa_contd_2019_2021.r
#  Focus on the Transit Commuter Characteristics from the
#    single-year ACS for 2019 and 2021
#   CSA, MSA/MiSA, congressional district, PUMA, urbanized area
#  IMPORTANT: Recommend using the Means of Transport from DP03, NOT B08006 or C08006
#    because DP03 is more likely to catch data that's suppressed in B or C tables!!
#      -- September 18, 2022 --
########################################################################

library(tidyverse)
library(tidycensus)

# setwd("~/Desktop/tidycenscsa_work/output")
setwd("~/Desktop/tidycenscsa_work")
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

##  Combined Statistical Area

csa_2019 <- get_acs(survey="acs1",geography="combined statistical area", variables=selvar19,
                   year=2019,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

csa_2021 <- get_acs(survey="acs1",geography="combined statistical area", variables=selvar21,
                   year=2021,output='wide') %>% 
           dplyr::arrange(GEOID) %>% 
           dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

csa_join <- full_join(csa_2019,csa_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Metropolitan Statistical Area (MSA) and Micropolitan Statistical Area (MiSA)

msa_2019 <- get_acs(survey="acs1",
                    geography="metropolitan statistical area/micropolitan statistical area", 
                    variables=selvar19,
                      year=2019,output='wide') %>% 
             dplyr::arrange(GEOID) %>% 
             dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

msa_2021 <- get_acs(survey="acs1",
                    geography="metropolitan statistical area/micropolitan statistical area", 
                    variables=selvar21,
                      year=2021,output='wide') %>% 
              dplyr::arrange(GEOID) %>% 
              dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

msa_join <- full_join(msa_2019,msa_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Congressional District (117th Congress, 2021-2023)

cong_2019 <- get_acs(survey="acs1",geography="congressional district", variables=selvar19,
                      year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

cong_2021 <- get_acs(survey="acs1",geography="congressional district", variables=selvar21,
                      year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

cong_join <- full_join(cong_2019,cong_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Public Use Microdata Areas (2010-Census-based)

urban_2019 <- get_acs(survey="acs1",geography="public use microdata area", variables=selvar19,
                       year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

urban_2021 <- get_acs(survey="acs1",geography="public use microdata area", variables=selvar21,
                       year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

urban_join <- full_join(urban_2019,urban_2021,by=c("GEOID","NAME")) %>% 
  relocate(transit_share_2021,.after=NAME) %>% 
  relocate(transit_share_2019,.after=NAME) %>% 
  relocate(transit_2019_E,.after=transit_share_2021) %>% 
  relocate(transit_2021_E,.after=transit_2019_E) %>% 
  relocate(totworker_2019_E,.after=transit_2021_E) %>% 
  relocate(totworker_2021_E,.after=totworker_2019_E) %>% 
  relocate(total_pop_2021_E,.after=total_pop_2019_E)

##################################################################################
## Urbanized Areas

urban_2019 <- get_acs(survey="acs1",geography="urban area", variables=selvar19,
                     year=2019,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2019 = transit_2019_E / totworker_2019_E)

urban_2021 <- get_acs(survey="acs1",geography="urban area", variables=selvar21,
                     year=2021,output='wide') %>% 
  dplyr::arrange(GEOID) %>% 
  dplyr::mutate(transit_share_2021 = transit_2021_E / totworker_2021_E)

urban_join <- full_join(urban_2019,urban_2021,by=c("GEOID","NAME")) %>% 
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

write.csv(csa_join,    "ACS_csa_transit_2019_2021.csv")
write.csv(msa_join,    "ACS_msa_transit_2019_2021.csv")
write.csv(cong_join,   "ACS_cong_transit_2019_2021.csv")
write.csv(puma_join,   "ACS_puma_transit_2019_2021.csv")
write.csv(urban_join,  "ACS_urban_transit_2019_2021.csv")

###################################################################################
