########################################################################
#  ctpp1721_california_b202105_place.r
#   Analyze CTPP 2017-2021 data for California     
#   place-of-work, workers by detailed means of transportation
#   Table B202105
#     -- April 2, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B202105")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302100_e1 as numeric....

# county1 <- read.csv("B102106_2025-03-22.csv",skip=1) 

# county2 <- read_csv("B102106_2025-03-22.csv",skip=1)

# The downloaded csv files were renamed as part1 and part2. The table was too large
#   to be downloaded in one piece!!

place_part1 <- read_csv("B202105_2025-03-31_part1.csv") # This is the preferred import!!!
place_part2 <- read_csv("B202105_2025-03-31_part2.csv") # This is the preferred import!!!

place1 <- left_join(place_part1,place_part2)

place2 <- place1 %>% 
   clean_names() %>%  # janitor function to clean up variable names!
   filter(!x1=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
   rename(GEOID=x1,
          place_name=x2) %>% 
   separate_wider_position(GEOID,c(dummy1=7,statefips=2,placefips=5), cols_remove=FALSE) %>% 
   select(-dummy1) %>% 
   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
   mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
   mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE))

#   mutate_if(is.character, as.numeric) # converts all variables from character to numeric....

# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(place2,"temp1.csv")
place3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))

# Create new variables for transit total, and carpool 3+ total
place3 <- place3 %>% 
  mutate(transit_est = bus_est + subway_or_elevated_rail_est + 
                       long_distance_train_or_commuter_rail_est + 
                       light_rail_streetcar_or_trolley_est + 
                       ferryboat_est,
         transit_share = transit_est / total_means_of_transportation_est,
         transit_share = ifelse(is.na(transit_share),0,transit_share),
         carpool3p_est = car_truck_or_van_in_a_3_person_carpool_est + 
                         car_truck_or_van_in_a_4_person_carpool_est +
                         car_truck_or_van_in_a_5_or_6_person_carpool_est +
                        car_truck_or_van_in_a_7_or_more_person_carpool_est) %>% 
  relocate(transit_est:carpool3p_est, .after=place_name)
###########################################################################################
## Export the California Place data files to csv format!
###########################################################################################
setwd("~/Desktop/CTPP1721_Data/B202105")
write.csv(place3,"ctpp1721_california_places_B202105.csv")
###########################################################################################
