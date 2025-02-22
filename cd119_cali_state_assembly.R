############################################################
#  cd119_calif_state_assembly.r
#    Use the Census Bureau's API to obtain uncommon summary levels
#      such as county-state assembly and place-state assembly
#    sumlev= 620 = state lower house
#    sumlev= 622 = state lower house by county
#    sumlev= 624 = state lower house by place
#
#   State = California
#
#  Convert JSON files into data frames
#   JSON = JavaScript Object Notation.....
#  Clean up variable names
#  Merge with wikipedia scraped data on names/characteristcs of state assemblypersons
#  Rank the top 5 places, by total population, within each assembly district
#
#       -- September 20, 2023 --
#       -- February 6, 2025 -- (2025-27 legislators)
###########################################################
library(jsonlite)
library(tidyverse)
library(tidycensus)
#############################################################################
# Create a clean file of place names for joining later in the process!
#   A simple call using the tidycensus "get_decennial" function.
place_name <- get_decennial(year=2020,  sumfile="dhc", 
                            geography = "place",  state="CA",
                            show_call = TRUE,output="wide", 
                            variables = "P1_001N") %>% 
              rename(totpop_place_2020 = "P1_001N") %>% 
      mutate(across("NAME",str_replace,", California","")) %>% 
      mutate(across("NAME",str_replace,"CDP","")) %>% 
      mutate(across("NAME",str_replace,"city","")) %>% 
      mutate(across("NAME",str_replace,"town","")) %>% 
      separate_wider_position(GEOID,c(state=2,place_fips=5), cols_remove=FALSE) %>% 
      mutate(state_place_fips = GEOID)
#############################################################################
# This style uses the API call to create a json file, 
#  which is subsequently turned into data frame using as.data.frame function

# Population/Household Characteristics by California's 80 State Assembly Districts, sumlev=620

temp1 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=state%20legislative%20district%20(lower%20chamber):*&in=state:06")

assembly_calif <- as.data.frame(temp1)

assembly_calif1 <- assembly_calif %>% 
  filter(!V1=="NAME") %>% 
  rename(assembly_name = V1,
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
         assembly_dist   = V18) %>% 
  mutate_at(c("totpop_2020","votingage","hhpop","tothh","totdu","owner_hh",
              "renter_hh","hispanic","nh_white","nh_black","nh_aian","nh_asian",
              "nh_nhopi","nh_other","nh_multi"), as.numeric) %>% 
    unite(GEOID,c("state_fips","assembly_dist"),sep="", remove=FALSE) 
###########################################################################
#  The API was adapted from the Census Bureau's samples:
#     https://api.census.gov/data/2020/dec/cd118/examples.html 
# Population/Housing Characteristics by California's 80 Assembly Districts by COUNTY

#   Don't use linefeeds to make this more legible. The API won't work.

#  Sumlev=622
# California counties within Assembly Districts

temp2 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:06%20state%20legislative%20district%20(lower%20chamber):001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,020,021,022,023,024,025,026,027,028,029,030,031,032,033,034,035,036,037,038,039,040,041,042,043,044,045,046,047,048,049,050,051,052,053,054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069,070,071,072,073,074,075,076,077,078,079,080")

assembly_county_calif <- as.data.frame(temp2)

assembly_county_calif1 <- assembly_county_calif %>% 
  filter(!V1=="NAME") %>% 
  rename(assembly_name = V1,
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
         assembly_dist   = V18,
         county_fips  = V19) %>% 
  mutate_at(c("totpop_2020","votingage","hhpop","tothh","totdu","owner_hh",
              "renter_hh","hispanic","nh_white","nh_black","nh_aian","nh_asian",
              "nh_nhopi","nh_other","nh_multi"), as.numeric) %>% 
  unite(GEOID,c("state_fips","assembly_dist"),sep="", remove=FALSE) %>% 
  unite(state_county_fips,c("state_fips","county_fips"),sep="", remove=FALSE)
##############################################################################
# Total Population by California's 80 Assembly Districts by PLACE
#  sumlev=624

temp3 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=place/remainder%20(or%20part):*&in=state:06%20state%20legislative%20district%20(lower%20chamber):001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,020,021,022,023,024,025,026,027,028,029,030,031,032,033,034,035,036,037,038,039,040,041,042,043,044,045,046,047,048,049,050,051,052,053,054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069,070,071,072,073,074,075,076,077,078,079,080")

assembly_place_calif <- as.data.frame(temp3)

assembly_place_calif1 <- assembly_place_calif %>% 
  filter(!V1=="NAME") %>% 
  rename(assembly_name = V1,
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
         assembly_dist   = V18,
         place_fips  = V19) %>% 
  mutate_at(c("totpop_2020","votingage","hhpop","tothh","totdu","owner_hh",
              "renter_hh","hispanic","nh_white","nh_black","nh_aian","nh_asian",
              "nh_nhopi","nh_other","nh_multi"), as.numeric) %>% 
  unite(GEOID,c("state_fips","assembly_dist"),sep="", remove=FALSE) %>% 
  unite(state_place_fips,c("state_fips","place_fips"),sep="", remove=FALSE)

assembly_place_calif2 <- full_join(place_name,assembly_place_calif1, by="place_fips") %>% 
    mutate(GEOID = GEOID.y)

###############################################################################
# These files were created using the script:
#   scraping_wikipedia_calif_assembly_assembly.r

setwd("~/Desktop/Politics_Elections")

assembly_wiki <-readRDS("wikipedia_calif_state_assembly_feb2025.rda")

assembly_wiki2 <- assembly_wiki %>% 
     select(GEOID,namepartydisthome,district,dist_padded)
###############################################################################
# The GEOID join variable is a four digit character variable: state FIPS +
#   Assembly Dist
assembly_calif2 <- full_join(assembly_wiki2,assembly_calif1, by="GEOID")
assembly_county_calif2 <- full_join(assembly_wiki2,assembly_county_calif1, 
                                  by="GEOID")
assembly_place_calif3  <- full_join(assembly_wiki2,assembly_place_calif2,  
                                  by="GEOID") %>% 
         arrange(assembly_dist,-totpop_2020)

###############################################################################
# Ranks of Places by Population within each State Assembly District
#  Save the top 5 places within each assembly....
assembly_place_calif4 <- assembly_place_calif3 %>% 
  filter(!str_detect(assembly_name,"Remainder")) %>%
  group_by(assembly_dist) %>% 
  mutate(pop_ranks = order(order(totpop_2020, decreasing=TRUE))) %>% 
  filter(pop_ranks <= 5) # retain the top 5 places within each state assembly
###############################################################################
rank1 <- assembly_place_calif4 %>% 
  filter(pop_ranks==1) %>% 
  select(assembly_dist,pop_ranks,NAME,totpop_2020) %>% 
  rename(rank_1st=pop_ranks,
         name_1st = NAME,
         totpop_2020_1st = totpop_2020)

rank2 <- assembly_place_calif4 %>% 
  filter(pop_ranks==2) %>% 
  select(assembly_dist,pop_ranks,NAME,totpop_2020) %>% 
  rename(rank_2nd=pop_ranks,
         name_2nd = NAME,
         totpop_2020_2nd = totpop_2020)

rank3 <- assembly_place_calif4 %>% 
  filter(pop_ranks==3) %>% 
  select(assembly_dist,pop_ranks,NAME,totpop_2020) %>% 
  rename(rank_3rd=pop_ranks,
         name_3rd = NAME,
         totpop_2020_3rd = totpop_2020)

rank4 <- assembly_place_calif4 %>% 
  filter(pop_ranks==4) %>% 
  select(assembly_dist,pop_ranks,NAME,totpop_2020) %>% 
  rename(rank_4th=pop_ranks,
         name_4th = NAME,
         totpop_2020_4th = totpop_2020)

rank5 <- assembly_place_calif4 %>% 
  filter(pop_ranks==5) %>% 
  select(assembly_dist,pop_ranks,NAME,totpop_2020) %>% 
  rename(rank_5th=pop_ranks,
         name_5th = NAME,
         totpop_2020_5th = totpop_2020)

combine1 <- rank1 %>% 
   left_join(rank2, by="assembly_dist") %>% 
   left_join(rank3, by="assembly_dist") %>% 
   left_join(rank4, by="assembly_dist") %>% 
   left_join(rank5, by="assembly_dist") %>% 
   unite(top3places,c("name_1st","name_2nd","name_3rd"), 
      sep=" / ", remove=FALSE) %>% 
  unite(top5places,c("name_1st","name_2nd","name_3rd","name_4th","name_5th"), 
        sep=" / ", remove=FALSE) %>% 
  relocate(rank_1st, .before=name_1st)

# Combine the assembly level (40) master file with the top places file

assembly_calif3 <- left_join(assembly_calif2, combine1, by="assembly_dist") %>% 
  relocate(GEOID,.after=assembly_dist) %>% 
  relocate(top3places:top5places, .after=totpop_2020) %>% 
  select(-dist_padded)
#############################################################################
#  Subset the State assembly Districts wholly or partially in the Bay Area
sfbay_assembly <- assembly_county_calif2 %>% 
  filter(county_fips=="001" | county_fips=="013" | county_fips=="041" | 
           county_fips=="055" | county_fips=="075" | county_fips=="081" | 
           county_fips=="085" | county_fips=="095" | county_fips=="097" )
###############################################################################
#  Export many of these data frames to XLSX or CSV format.....
setwd("~/Desktop/Politics_Elections")

write.csv(assembly_calif3, "calif_state_assembly_2025_master1.csv") 
write.csv(assembly_county_calif2, "calif_state_assembly_2025_by_county.csv")
write.csv(assembly_place_calif3, "calif_state_assembly_2025_by_place.csv")  
write.csv(sfbay_assembly, "calif_bayarea_assembly_2025_by_county.csv")

# This concludes the R analysis of California State Assembly Districts
#  further table cleaning and preparation will be made in Excel, I think!
#############################################################################