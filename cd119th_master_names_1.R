######################################################################
##    cd119th_master_names_1.r
##  Read in US House Clerk's excel file with names of 119th Congress
##     https://clerk.house.gov/Votes
##    -- January 8, 2025 --
######################################################################
library(tidyverse)
library(readxl)
library(janitor)
library(tidycensus)
library(jsonlite)

setwd("~/Desktop/Politics_Elections")

# Trying to read the file directly from the House Clerk site.... doesn't work.....
#temp0 <- read_excel("https://clerk.house.gov/Members/ExcelMemberData/Member Data.xlsx") %>% 
#  clean_names()
#temp0 <- read_excel("https://clerk.house.gov/Members/ExcelMemberData/") %>% 
#  clean_names()

#  renamed excel file, as downloaded from clerk.house.gov site
#  "member_data_119th_congress_hr_jan0325.xlsx"

temp1 <- read_excel("member_data_119th_congress_hr_jan0325.xlsx") %>% 
      clean_names()

temp2 <- temp1 %>% 
  mutate(party2=case_when(party=="D" ~"(D)",
                          party=="R" ~ "(R)")) %>% 
  unite(fullname,c("first_name","middle_name","last_name","suffix"), na.rm=TRUE, 
          sep=" ", remove=FALSE) %>%
  unite(fullname2,c("first_name","middle_name","last_name","suffix","party2"), na.rm=TRUE, 
        sep=" ", remove=FALSE) %>%
  unite(fulladdress,c("address","city","state","zip_4"), na.rm=TRUE, 
        sep=",", remove=FALSE) %>% 
  separate_wider_position(st_dis,c(stusps=2,cdnum=2), cols_remove=FALSE)

# install.packages("toOrdinal")
library(toOrdinal)

# Create a simple data frame with Cardinal and Ordinal numbers, 1 thru 55
tempx <- data.frame(1:55) %>% 
          data.frame(toOrdinal::toOrdinal(1:55)) %>% 
          rename(cdnum2 = 1, cdnum_ord = 2) # rename columns 1 and 2 to something useful

temp3 <- temp2 %>% 
         mutate(cdnum2 = as.numeric(cdnum))

temp4 <- left_join(temp3,tempx,by="cdnum2") %>% 
         replace_na(list(cdnum_ord = 'At Large'))
####################################################################################
# Create a simple file of state (fips code), state (USPS), and state name
library(tigris)
states1 <- tigris::states() %>% 
    clean_names()

states2 <- states1 %>% 
  sf::st_drop_geometry() %>% 
  select(statefp,stusps,name) %>% 
  rename(state_name=name)
####################################################################################
# Merge master house clerk file (temp4) with state equivalency file
# This file includes the 435 regular HR seats, plus each seat for the delegates for
#  District of Columbia, Puerto Rico, US Virgin Islands, Guam, American Samoa, 
#   and the Northern Marianas N=441

master_441 <- left_join(temp4,states2,by="stusps") %>% 
  unite(geoid,c("statefp","cdnum"), na.rm=TRUE, sep="", remove=FALSE) %>% 
  unite(statename_cd,c("state_name","cdnum_ord"), na.rm=TRUE, sep=" ", remove=FALSE ) %>% 
  unite(fullname3,c("fullname2","statename_cd"),  na.rm=TRUE, sep=" - ", remove=FALSE )

master_437 <- master_441 %>%   # retains DC + PR
  filter(!statefp %in% c("60","66","69","78"))

master_435 <- master_441 %>%   # retains only the 50 states, n=435
  filter(!statefp > 56) %>% 
  filter(!statefp == 11)

####################################################################################
# This uses the API call to create a json file, 
#  which is subsequently turned into data frame using the as.data.frame function

# Population and Housing Variables for All US Congressional Districts
#   from the US Decennial Census, 2020, File CD119

temp0 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=congressional%20district:*&in=state:*")

cd_base <- as.data.frame(temp0)

cd_base1 <- cd_base %>% 
  filter(!V1=="NAME") %>% 
  rename(cd_name     = V1,
         totpop_2020 = V2,
         votingage   = V3,
         hhpop       = V4,
         tothh       = V5,
         totdu       = V6,
         owner_hh    = V7,
         renter_hh   = V8,
         hispanic    = V9,
         nh_white    = V10,
         nh_black    = V11,
         nh_aian     = V12,
         nh_asian    = V13,
         nh_nhopi    = V14,
         nh_other    = V15,
         nh_multi    = V16,
         state_fips  = V17,
         cong_dist   = V18) %>% 
  #   mutate_at("totpop_2020",as.numeric) %>% 
  mutate_at(c("totpop_2020","votingage","hhpop","tothh","totdu","owner_hh",
              "renter_hh","hispanic","nh_white","nh_black","nh_aian","nh_asian",
              "nh_nhopi","nh_other","nh_multi"), as.numeric) %>% 
  unite(GEOID,c("state_fips","cong_dist"),sep="", remove=FALSE) %>% 
  relocate(GEOID:cong_dist, .before=cd_name) %>% 
  filter(!totpop_2020==0) %>%  
  clean_names()
#  filter(!cong_dist=="98")   # Delete Washington DC  and Puerto Rico Delegate.
################################################################################
#  Merge the Master Name file (master_437) with the
#  Census 2020, CD119 data file.
#

masterx <- left_join(master_437, cd_base1, by="geoid")
################################################################################
# Write out these files into googlesheets, or csv?
#

#temp3 <- read_sheet("https://docs.google.com/spreadsheets/d/1zBk# #-LakSmWl3i4jdQ0_efpfj9LVCHMCDcfPxWQhrZeE/edit#gid=1178631925")

# Write out these dataframes to my Google Drive
#  googlesheets4 assigns a unique name to each sheet. See the log.

library(googlesheets4)

spec1 <- gs4_create("cd119th_master_names_441_extended",sheets=master_441)
googlesheets4::write_sheet(master_441, ss=spec1, sheet=NULL)

spec2 <- gs4_create("cd119th_master_names_census2020_437_extended",sheets=masterx)
googlesheets4::write_sheet(masterx, ss=spec2, sheet=NULL)

############################################################################
# write these out to csv and rda files
############################################################################
setwd("~/Desktop/Politics_Elections")

write.csv(master_441, "cd119th_master_names_441_extended.csv") 
  saveRDS(master_441, "cd119th_master_names_441_extended.rda")

write.csv(masterx, "cd119th_master_names_census2020_437_extended.csv") 
  saveRDS(masterx, "cd119th_master_names_census2020_437_extended.rda")
  
############################################################################