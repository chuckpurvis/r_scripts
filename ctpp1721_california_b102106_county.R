########################################################################
#  ctpp1721_california_b102106_county.r
#   Analyze CTPP 2017-2021 data for California     
#   county-of-residence, workers by detailed means of transportation
#   Table B102106
#     -- March 22, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B102106")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302100_e1 as numeric....

county1 <- read.csv("B102106_2025-03-22.csv",skip=1) 

county2 <- read_csv("B102106_2025-03-22.csv",skip=1)

countyx <- read_csv("B102106_2025-03-22.csv") # This is the preferred import!!!

countyx2 <- countyx %>% 
   clean_names() %>%  # janitor function to clean up variable names!
   filter(!x1=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
   rename(GEOID=x1,
          county_name=x2) %>% 
   separate_wider_position(GEOID,c(dummy1=7,statefips=2,countyfips=3), cols_remove=FALSE) %>% 
   select(-dummy1) %>% 
   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
   mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]")))
 #   mutate_if(is.character, as.numeric) # converts all variables from character to numeric....

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(countyx2,"temp1.csv")
countyx3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))

new=c("moe")
old=c("margin_of_error")

# shorten variable names to est and moe!
countyx4 <- countyx3 %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE))

###########################################################################################
# Create simple correspondence files for Bay Area counties of residence and work
baycounty_r <- data.frame(countyfips = c('001','013','041','055','075','081','085','095','097')) %>% 
  mutate(bayco_flag=1)
countyx5 <- full_join(countyx4,baycounty_r, by="countyfips") %>% 
  replace(is.na(.), 0) %>% 
  relocate(bayco_flag, .before=county_name)
baydata <- countyx5 %>% 
  filter(bayco_flag=="1") %>% 
  adorn_totals  # janitor function to sum up data. MOE sum should NOT be used!!
baydata$county_name[10]="San Francisco Bay Area"
###########################################################################################
## Export the California counties and Bay Area counties data files to csv format!
###########################################################################################
setwd("~/Desktop/CTPP1721_Data/B102106")
write.csv(countyx5,"ctpp1721_california_counties_b102106.csv")
write.csv(baydata, "ctpp1721_bayarea_counties_b102106.csv")
###########################################################################################



