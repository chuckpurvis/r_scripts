############################################################
#  cd119_congdist_county_place_allstates.r
#    Use the Census Bureau's API to obtain uncommon summary levels
#      such as county-congressional districts and place-congress districts
#    sumlev= 500 = congressional districts
#    sumlev= 510 = congressional districts by county
#    sumlev= 531 = congressional districts by place
#
#   State = ALL STATES
#
#  12/5/2024 revisions: Add extra decennial census variables to these files,
#    including tothh, totdu, hhpop, owner_hh, renter_hh, and pop by race/ethnicity
#
#  Convert JSON files into data frames
#   JSON = JavaScript Object Notation.....
#  Clean up variable names
#  Use the current congresspersons from the script:
#    cd119th_masternames_1.r   NOT the Daily Kos (118th Congress)
#  Formerly: Merge with Daily Kos data on names/characteristcs of congressmen
#  Rank the top 5 places, by total population, within each CD
#
#       -- December 12, 2024 --
#       -- December 14, 2024 --
#       -- January 27, 2025 --
###########################################################
library(jsonlite)
library(tidyverse)
library(tidycensus)
###############################################################
# Import two master files: State-CD-County and State-CD-Place
#  Created using the scripts:
#    cd119_allcounties_allstates_master.r
#    cd119_allplaces_allstates_master.r
###############################################################
setwd("~/Desktop/cd118_file")
master_county <- readRDS("cd119_allstates_allcounties.rda")
master_place  <- readRDS("cd119_allstates_allplaces.rda")

setwd("~/Desktop/Politics_Elections")
#   Created using the script:
#   cd119th_master_names_1.r
master_names  <- readRDS("cd119th_master_names_census2020_437_extended.rda")
###############################################################

#  setwd("~/Desktop/Census_2020")

dhc_variable_list <- load_variables(year = 2020, dataset = "dhc", cache = TRUE)
cd118_variable_list <- load_variables(year = 2020, dataset = "cd118", cache = TRUE)

##############################################################################
# Create a clean file of place names for joining later in the process!
#   A simple call using the tidycensus "get_decennial" function.
selvars <- c(totpop_place_2020 = "P1_001N",
             vappop_place_2020 = "P10_001N",
             hhpop_place_2020  = "P15_001N",
             tothh_place_2020  = "P16_001N",
             totdu_place_2020  = "H1_001N",
             ownerhh_place_2020 = "H12_002N",
             renterhh_place_2020= "H12_010N",
             hispanic_place_2020      = "P5_010N",
             nh_white_place_2020      = "P5_003N",
             nh_black_place_2020      = "P5_004N",
             nh_aian_place_2020       = "P5_005N",
             nh_asian_place_2020      = "P5_006N",
             nh_nhopi_place_2020      = "P5_007N",
             nh_other_place_2020      = "P5_008N",
             nh_multi_place_2020      = "P5_009N")

place_name <- get_decennial(year=2020,  sumfile="dhc", 
                            geography = "place", # state="CA",
                            show_call = TRUE,output="wide", 
                            variables = selvars) %>% 
   #   mutate(across("NAME",str_replace,", California","")) %>% 
      mutate(across("NAME",str_replace," CDP","")) %>% 
      mutate(across("NAME",str_replace," city","")) %>% 
      mutate(across("NAME",str_replace," town","")) %>% 
      separate_wider_position(GEOID,c(state=2,place_fips=5), cols_remove=FALSE) %>% 
      arrange(GEOID) %>% 
      separate_wider_delim(NAME,delim=",",names=c("placename","statename"), too_many="drop",
                           cols_remove=FALSE)

state <- get_decennial(year=2020, sumfile="dhc",
                       geography="state", show_call=TRUE,output="wide",
                       variables = "P1_001N")
####################################################################################
# This uses the API call to create a json file, 
#  which is subsequently turned into data frame using the as.data.frame function

# Population and Housing Variables for All US Congressional Districts

temp0 <- fromJSON("https://api.census.gov/data/2020/dec/cd118?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=congressional%20district:*&in=state:*")

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
    filter(!cong_dist=="98")   # Delete Washington DC  and Puerto Rico Delegate.

####################################################################################
#  The API was adapted from the Census Bureau's samples:
#     https://api.census.gov/data/2020/dec/cd118/examples.html 
#   Don't use linefeeds to make this more legible. The API won't work.

#  See the scripts: cd118_allcounties_allstates_master.r and
#                   cd118_allplaces_allstates_master.r
#    for the full set of API calls on all 50 states.
#  Haven't figured out a method to "purrr" iterate using the Census Bureau's API....
######################################################################################
# Alabama places within congressional districts
temp01 <- fromJSON("https://api.census.gov/data/2020/dec/cd118?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=place/remainder%20(or%20part):*&in=state:01%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
cd_state01 <- as.data.frame(temp01)

# Alabama counties within congressional districts
temp1 <- fromJSON("https://api.census.gov/data/2020/dec/cd118?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:01%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state01 <- as.data.frame(temp1)

#################################################################################
setwd("~/Desktop/Politics_Elections")
# this file was created with the script: kos_daily_prep1.r
# It has 435 records, and 104 variables!!!
# kos1 <- readRDS("cd118_dailykos_edited_revised.rda")
# kos2 <- kos1 %>% 
  #   filter(STATEFP=="06") %>% 
#     select(GEOID,namepartyplace,Geographic.Description)


# Simplify the 119th Congress Names!!
#  Select some of the 43 variables on this master names file....

master_names2 <- master_names %>% 
  rename(GEOID = geoid) %>% 
  filter(!cong_dist=="98") %>%   # Delete Washington DC  and Puerto Rico Delegate.
  select(GEOID,fullname,fullname2,fullname3,party,state_name)
################################################################################
# The GEOID join variable is a four digit character variable: state FIPS + Cong Dist
cd_base2 <- full_join(master_names2,cd_base1, by="GEOID")

cd_county2 <- full_join(master_names2,master_county, by="GEOID")

cd_place2  <- full_join(master_names2,master_place,  by="GEOID") %>% 
         arrange(GEOID)
################################################################################
#  Subset the Congressional Districts wholly or partially in the Bay Area
# sfbay_cd <- cd_county2 %>% 
#  filter(county_fips=="001" | county_fips=="013" | county_fips=="041" | 
#           county_fips=="055" | county_fips=="075" | county_fips=="081" | 
#           county_fips=="085" | county_fips=="095" | county_fips=="097" )
###############################################################################
# Ranks of Places by Population within each Congressional District
#  Save the top 5 places within each CD....
cd_place3 <- cd_place2 %>%
#  mutate(NAME=placename2) %>% 
  filter(!str_detect(placename2,"Remainder")) %>% # remove "remainder" place
  group_by(GEOID) %>%   # GEOID = State + Congressional District!!
  mutate(pop_ranks = order(order(totpop_2020, decreasing=TRUE))) %>% 
  filter(pop_ranks <= 5)  %>%  # retain the top 5 places within each CD
#  str_replace_all("placename2","(part)","") %>% 
#  mutate(across('placename2', str_replace, '(part)','')) %>% 
#  mutate(across("placename2", str_replace, "()","")) %>% 
#  mutate(placename2 = str_replace(NAME,"(part)","")) %>% 
  select(GEOID,cong_dist,pop_ranks,placename3,placename2,totpop_2020) %>% 
  arrange(GEOID,pop_ranks)

###############################################################################
rank1 <- cd_place3 %>% 
  filter(pop_ranks==1) %>% 
  select(GEOID,pop_ranks,placename3,totpop_2020) %>% 
  rename(rank_1st=pop_ranks,
         name_1st = placename3,
         totpop_2020_1st = totpop_2020)

rank2 <- cd_place3 %>% 
  filter(pop_ranks==2) %>% 
  select(GEOID,pop_ranks,placename3,totpop_2020) %>% 
  rename(rank_2nd=pop_ranks,
         name_2nd = placename3,
         totpop_2020_2nd = totpop_2020)

rank3 <- cd_place3 %>% 
  filter(pop_ranks==3) %>% 
  select(GEOID,pop_ranks,placename3,totpop_2020) %>% 
  rename(rank_3rd=pop_ranks,
         name_3rd = placename3,
         totpop_2020_3rd = totpop_2020)

rank4 <- cd_place3 %>% 
  filter(pop_ranks==4) %>% 
  select(GEOID,pop_ranks,placename3,totpop_2020) %>% 
  rename(rank_4th=pop_ranks,
         name_4th = placename3,
         totpop_2020_4th = totpop_2020)

rank5 <- cd_place3 %>% 
  filter(pop_ranks==5) %>% 
  select(GEOID,pop_ranks,placename3,totpop_2020) %>% 
  rename(rank_5th=pop_ranks,
         name_5th = placename3,
         totpop_2020_5th = totpop_2020)

combine1 <- rank1 %>% 
   left_join(rank2, by="GEOID") %>% 
   left_join(rank3, by="GEOID") %>% 
   left_join(rank4, by="GEOID") %>% 
   left_join(rank5, by="GEOID") %>% 
   unite(top3places,c("name_1st","name_2nd","name_3rd"), 
      sep=" / ", remove=FALSE) %>% 
  unite(top5places,c("name_1st","name_2nd","name_3rd","name_4th","name_5th"), 
        sep=" / ", remove=FALSE)

# Combine the congressional (435) master file with the top 5 places file

cd_base3 <- left_join(cd_base2, combine1, by="GEOID") %>% 
 # relocate(GEOID,.after=cong_dist) %>% 
  relocate(top3places:top5places, .after=totpop_2020) # %>% 
 # relocate(Geographic.Description:cd_name, .after=cong_dist) 
  
#############################################################################
#  Export many of these data frames to CSV format.....
# setwd("~/Desktop/Politics_Elections")
setwd("~/Desktop/cd118_file")

# tidy up, get everything in sort order
cd_base3 <- cd_base3 %>% 
    arrange(GEOID)
cd_county2 <- cd_county2 %>% 
    arrange(GEOID,county_fips)
cd_place2 <- cd_place2 %>% 
    arrange(GEOID,place_fips)

write.csv(cd_base3, "usa_cd119_congdist_addvars_top3_top5.csv") 
saveRDS(cd_base3,   "usa_cd119_congdist_addvars_top3_top5.rda")

write.csv(cd_county2, "usa_cd119_by_county_addvars.csv")
saveRDS(cd_county2,   "usa_cd119_by_county_addvars.rda")

write.csv(cd_place2, "usa_cd119_by_place_addvars.csv") 
saveRDS(cd_place2, "usa_cd119_by_place_addvars.rda") 

# Export these to my googlesheets, as well
library(googlesheets4)

spec1 <- gs4_create("usa_cd119th_congdist_addvars_top3_top5",sheets=cd_base3)
googlesheets4::write_sheet(cd_base3, ss=spec1, sheet=NULL)

spec2 <- gs4_create("usa_cd119th_congdist_by_county_addvars",sheets=cd_county2)
googlesheets4::write_sheet(cd_county2, ss=spec2, sheet=NULL)

spec3 <- gs4_create("usa_cd119th_congdist_by_place_addvars",sheets=cd_place2)
googlesheets4::write_sheet(cd_place2, ss=spec3, sheet=NULL)

# This concludes the R analysis of USA Congressional Districts
#  further table cleaning and preparation in Excel.
#############################################################################