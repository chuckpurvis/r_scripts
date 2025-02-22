#########################################################################
#   cd119_top5_counties_by_cd.r
#   Largest 3 to 5 counties per each congressional district, 119th Congress
#   This will be used primarily for showing New York Boroughs and NYC Cong Districts
#      -- January 28, 2025 --
#####################################################################################
library(tidyverse)
library(tidycensus)

setwd("~/Desktop/cd118_file")

county <- readRDS("usa_cd119_by_county_addvars.rda")
master_cd <- readRDS("usa_cd119_congdist_addvars_top3_top5.rda")
###############################################################################
# Ranks of Counties by Population within each Congressional District
#  Save the top 5 county within each CD....
county2 <- county %>%
  group_by(GEOID) %>%   # GEOID = State + Congressional District!!
  mutate(pop_ranks = order(order(totpop_2020, decreasing=TRUE))) %>% 
  filter(pop_ranks <= 5)  %>%  # retain the top 5 counties within each CD
  select(GEOID,cong_dist,pop_ranks,countyname,countyname2,totpop_2020) %>% 
  arrange(GEOID,pop_ranks)

rank1 <- county2 %>% 
  filter(pop_ranks==1) %>% 
  select(GEOID,pop_ranks,countyname2,totpop_2020) %>% 
  rename(rank_1st=pop_ranks,
         name_1st = countyname2,
         totpop_2020_1st = totpop_2020)

rank2 <- county2 %>% 
  filter(pop_ranks==2) %>% 
  select(GEOID,pop_ranks,countyname2,totpop_2020) %>% 
  rename(rank_2nd=pop_ranks,
         name_2nd = countyname2,
         totpop_2020_2nd = totpop_2020)

rank3 <- county2 %>% 
  filter(pop_ranks==3) %>% 
  select(GEOID,pop_ranks,countyname2,totpop_2020) %>% 
  rename(rank_3rd=pop_ranks,
         name_3rd = countyname2,
         totpop_2020_3rd = totpop_2020)

rank4 <- county2 %>% 
  filter(pop_ranks==4) %>% 
  select(GEOID,pop_ranks,countyname2,totpop_2020) %>% 
  rename(rank_4th=pop_ranks,
         name_4th = countyname2,
         totpop_2020_4th = totpop_2020)

rank5 <- county2 %>% 
  filter(pop_ranks==5) %>% 
  select(GEOID,pop_ranks,countyname2,totpop_2020) %>% 
  rename(rank_5th=pop_ranks,
         name_5th = countyname2,
         totpop_2020_5th = totpop_2020)

combine1 <- rank1 %>% 
  left_join(rank2, by="GEOID") %>% 
  left_join(rank3, by="GEOID") %>% 
  left_join(rank4, by="GEOID") %>% 
  left_join(rank5, by="GEOID") %>% 
  unite(top3counties,c("name_1st","name_2nd","name_3rd"), 
        sep=" / ", remove=FALSE) %>% 
  unite(top5counties,c("name_1st","name_2nd","name_3rd","name_4th","name_5th"), 
        sep=" / ", remove=FALSE)

# Combine the congressional (435) master file with the top 5 counties file

master_cd2 <- left_join(master_cd, combine1, by="GEOID") %>% 
  relocate(top3counties:top5counties, .after=totpop_2020) # %>% 

# Reset the variables names in NYC, and replace / NA with ""

master_cd3 <- master_cd2 %>% 
  mutate(top3counties = str_replace(top3counties, "/ NA", "")) %>% 
  mutate(top3counties = str_replace(top3counties, "/ NA", "")) %>% 
  mutate(top5counties = str_replace(top5counties, "/ NA", "")) %>% 
  mutate(top5counties = str_replace(top5counties, "/ NA", "")) %>% 
  mutate(top5counties = str_replace(top5counties, "/ NA", "")) %>% 
  mutate(top5counties = str_replace(top5counties, "/ NA", ""))

new_york_cd <- master_cd3 %>%  
  filter(state_name=="New York") %>% 
  mutate(top3counties = str_replace(top3counties, "New York", "Manhattan")) %>% 
  mutate(top3counties = str_replace(top3counties, "Kings", "Brooklyn")) %>% 
  mutate(top3counties = str_replace(top3counties, "Richmond", "Staten Island")) %>% 
  mutate(top5counties = str_replace(top5counties, "New York", "Manhattan")) %>% 
  mutate(top5counties = str_replace(top5counties, "Kings", "Brooklyn")) %>% 
  mutate(top5counties = str_replace(top5counties, "Richmond", "Staten Island"))
  
setwd("~/Desktop/cd118_file")

write.csv(master_cd3, "usa_cd119_top3_top5_counties.csv") 
saveRDS(master_cd3,   "usa_cd119top3_top5_counties.rda") 

write.csv(new_york_cd, "newyork_cd119_top3_top5_counties.csv") 
saveRDS(new_york_cd,   "newyork_cd119top3_top5_counties.rda") 

#############################################################################