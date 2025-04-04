########################################################################
#  ctpp1721_hayward_b302103.r
#   Analyze CTPP 2017-2021 data for the City of Hayward
#   Hayward-to-County, County-to-Hayward,
#   Hayward-to-Place, Place-to-Hayward
#   Total workers by Means of Transportation (18), 
#      intra-state California, only
#   Table B302103
#     -- March 23, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B302103_hayward")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302103_e1 as numeric....

hay2place1 <- read_csv("B302103_hayward2allplaces_2025-03-27.csv") # This is the preferred import!!
place2hay1 <- read_csv("B302103_allplaces2hayward_2025-03-27.csv") # This is the preferred import!!

hay2county1 <- read_csv("B302103_hayward2allcounties_2025-03-27.csv")
county2hay1 <- read_csv("B302103_allcounties2hayward_2025-03-27.csv")

##############################################################################################
# From Hayward City to All California Places
hay2place2 <- hay2place1 %>% 
  clean_names() %>%  # janitor function to clean up variable names!
  rename(GEOID=x1,
         destination_name=x2,
         origin_name=x3) %>% 
  filter(!GEOID=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
  separate_wider_position(GEOID,c(dummy1=7,state_res=2,place_res=5,
                                  state_work=2,place_work=5), cols_remove=FALSE) %>%
  select(-dummy1) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
#  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_remove_all(., "California"))) %>%
  mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% # shorten variable names
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>%        # shorten variable names
  relocate(origin_name:destination_name, .before=state_res) %>% 
  arrange(origin_name,destination_name) # sort records by home place, work place
  
# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(hay2place2,"temp1.csv")
hay2place3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
############################################################################################
# From All California Places to Hayward City
place2hay2 <- place2hay1 %>% 
  clean_names() %>%  # janitor function to clean up variable names!
  rename(GEOID=x1,
         destination_name=x2,
         origin_name=x3) %>% 
  filter(!GEOID=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
  separate_wider_position(GEOID,c(dummy1=7,state_res=2,place_res=5,
                                  state_work=2,place_work=5), cols_remove=FALSE) %>%
  select(-dummy1) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  #  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_remove_all(., "California"))) %>%
  mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% # shorten variable names
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>%        # shorten variable names
  relocate(origin_name:destination_name, .before=state_res) %>% 
  arrange(origin_name,destination_name) # sort records by home place, work place

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(place2hay2,"temp1.csv")
place2hay3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
############################################################################################
# From Hayward City to All California Counties
hay2county2 <- hay2county1 %>% 
  clean_names() %>%  # janitor function to clean up variable names!
  rename(GEOID=x1,
         destination_name=x2,
         origin_name=x3) %>% 
  filter(!GEOID=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
  separate_wider_position(GEOID,c(dummy1=7,state_res=2,place_res=5,
                                  state_work=2,county_work=3), cols_remove=FALSE) %>%
  select(-dummy1) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  #  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_remove_all(., "California"))) %>%
  mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% # shorten variable names
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>%        # shorten variable names
  relocate(origin_name:destination_name, .before=state_res) %>% 
  arrange(origin_name,destination_name) # sort records by home place, work place

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(hay2county2,"temp1.csv")
hay2county3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
############################################################################################
# From All California Counties to Hayward City
county2hay2 <- county2hay1 %>% 
  clean_names() %>%  # janitor function to clean up variable names!
  rename(GEOID=x1,
         destination_name=x2,
         origin_name=x3) %>% 
  filter(!GEOID=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
  separate_wider_position(GEOID,c(dummy1=7,state_res=2,county_res=3,
                                  state_work=2,place_work=5), cols_remove=FALSE) %>%
  select(-dummy1) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  #  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_remove_all(., "California"))) %>%
  mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% # shorten variable names
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>%        # shorten variable names
  relocate(origin_name:destination_name, .before=state_res) %>% 
  arrange(origin_name,destination_name) # sort records by home place, work place

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(county2hay2,"temp1.csv")
county2hay3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
############################################################################################

## Export these files into csv format
setwd("~/Desktop/CTPP1721_Data/B302103_hayward")

write.csv(hay2place3,"hayward2allplaces_workermeans_b302103.csv") 
write.csv(place2hay3,"allplaces2hayward_workermeans_b302103.csv") 
write.csv(hay2county3,"hayward2allcounties_workermeans_b302103.csv") 
write.csv(county2hay3,"allcounties2hayward_workermeans_b302103.csv") 

########################################################################################
