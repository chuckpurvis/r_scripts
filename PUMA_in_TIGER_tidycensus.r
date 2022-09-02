########################################################################
##  PUMA_in_TIGER_tidycensus.r
##  Analyze the different PUMAs from Census Bureau TIGER/Line files
##   using tidycensus
##  ACS 2006/2010 using the 2000 Census-based PUMAs
##  ACS 2016/2020 using the 2010 Census-based PUMAs
##     -- September 2, 2022 --
########################################################################

library(tidyverse)
library(tidycensus)

# setwd("~/Desktop/tidycensus_work/output")
setwd("~/Desktop/tidycensus_work")
getwd()

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)
Sys.getenv("CENSUS_API_KEY")

# Set a list of variables to extract in each iteration of get_acs

selvars <- c(households_     = "B25044_001", # Total Households
             totalpop_      = "B01001_001", # Total Population (HHPOP+GQPOP)
             hhpop_total_   = "B25008_001") # Population in Households, Total

# Retrieve ACS 2006-2010 Data at PUMA level, 2000 Census-Based PUMAs!
puma2010 <- tidycensus::get_acs(survey="acs5", variables=selvars, geography="puma", # state="CA",
                             year=2010,output='wide') %>% 
            dplyr::arrange(GEOID)  %>% 
            dplyr::mutate(state =substr(GEOID,1,2))

state2010 <- puma2010 %>% 
             dplyr::arrange(state) %>% 
             dplyr::group_by(state) %>% 
             dplyr::summarize(TOTHH_0610  = sum(households_E),
                              TOTPOP_0610 = sum(totalpop_E),
                              HHPOP_0610  = sum(hhpop_total_E),
                              numpuma_2000Census = n())

# Retrieve ACS 2016-2020 Data at PUMA level, 2010 Census-Based PUMAs!
puma2020 <- tidycensus::get_acs(survey="acs5", variables=selvars, geography="puma", # state="CA",
                                year=2020,output='wide') %>% 
            dplyr::arrange(GEOID) %>% 
            dplyr::mutate(state =substr(GEOID,1,2))

state2020 <- puma2020 %>% 
  dplyr::arrange(state) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(TOTHH_1620  = sum(households_E),
                   TOTPOP_1620 = sum(totalpop_E),
                   HHPOP_1620  = sum(hhpop_total_E),
                   numpuma_2010Census = n())

############################################################################
# Retrieve 2020 Decennial Total Population

selvars20  <- c(TotalPop_2020   = "P2_001N",   # Total Population
                GQ_Total_2020   = "P5_001N",   # Group Quarters Population, Total
                Total_HH_2020   = "H1_002N") # Occupied Housing Units

us_states2020  <- tidycensus::get_decennial(year=2020,  sumfile="pl", 
                                  geography = "state",
                                  show_call = TRUE,output="wide", variables = selvars20) %>% 
                  dplyr::mutate(state=GEOID) %>% 
                  dplyr::arrange(state) %>% 
                  dplyr::mutate(HH_Pop_2020 = TotalPop_2020 - GQ_Total_2020) %>% 
                  dplyr::relocate(state,.before=GEOID)

###########################################################################
## Stitch together the 2006/10 ACS, 2016/20 ACS and 2020 Decennial

combined1 <- us_states2020 %>% 
             left_join(state2010,by='state') %>% 
             left_join(state2020,by='state') %>% 
             dplyr::relocate(numpuma_2000Census,.before=numpuma_2010Census)

###########################################################################
# Write out data into csv files!
setwd("~/Desktop/tidycensus_work/output")
## setwd("~/Desktop/pl94171/output")

write.csv (combined1,"PUMAs_2000_2010_us_states.csv")
