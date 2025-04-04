########################################################################
#  ctpp1721_california_b102106_PUMA.r
#   Analyze CTPP 2017-2021 data for California     
#   PUMA-of-residence, workers by detailed means of transportation
#   Table B102106
#     -- April 3, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B102106_CalPUMAs")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302100_e1 as numeric....

## county1 <- read.csv("B102106_2025-03-22.csv",skip=1) 

## county2 <- read_csv("B102106_2025-03-22.csv",skip=1)

puma1 <- read_csv("B102106_2025-03-31.csv") # This is the preferred import!!!

puma2 <- puma1 %>% 
   clean_names() %>%  # janitor function to clean up variable names!
   filter(!x1=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
   rename(GEOID=x1,
          puma_name=x2) %>% 
   separate_wider_position(GEOID,c(dummy1=7,statefips=2,puma=5), cols_remove=FALSE) %>% 
   select(-dummy1) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  mutate(across(.cols=everything(), ~str_replace(., "--"," __ "))) %>% # for PUMA names...
  mutate(across(.cols=everything(), ~str_remove_all(., "PUMA;"))) %>% # for PUMA names...
  mutate(across(.cols=everything(), ~str_remove_all(., "PUMA"))) %>% # for PUMA names...
  mutate(across(.cols=everything(), ~str_remove_all(., "California"))) %>% # for PUMA names...
   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
#   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
   mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]")))
 #   mutate_if(is.character, as.numeric) # converts all variables from character to numeric....

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(puma2,"temp1.csv")
puma3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
###########################################################################################
## Export the California PUMA data files to csv format!
###########################################################################################
setwd("~/Desktop/CTPP1721_Data/B102106_CalPUMAs")
write.csv(puma3,"ctpp1721_california_puma_b102106.csv")
###########################################################################################
