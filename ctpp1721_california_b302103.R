########################################################################
#  ctpp1721_california_b302103.r
#   Analyze CTPP 2017-2021 data for California     
#   county-to-county total workers by Means of Transportation (18), 
#      intra-state California
#   Table B302103
#     -- March 23, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B302103")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302103_e1 as numeric....

coco1 <- read.csv("B302103_2025-03-23.csv",skip=1) 

coco2 <- read_csv("B302103_2025-03-23.csv",skip=1)

cocox <- read_csv("B302103_2025-03-23.csv") # This is the preferred import!!

cocox2 <- cocox %>% 
  clean_names() %>%  # janitor function to clean up variable names!
  rename(GEOID=x1,
         destination_name=x2,
         origin_name=x3) %>% 
  filter(!GEOID=="geoid") %>% # deletes the 2nd row of imported data (original variable names)
  separate_wider_position(GEOID,c(dummy1=7,state_res=2,county_res=3,
                                  state_work=2,county_work=3), cols_remove=FALSE) %>%
  select(-dummy1) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_replace(., ",",""))) %>% 
  mutate(across(.cols=everything(), ~str_remove_all(., "[+/-]"))) %>% 
  rename_with(~ gsub("_carro_publico_in_puerto_rico", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("margin_of_error", "moe", .x, fixed = TRUE)) %>% # shorten variable names
  rename_with(~ gsub("estimate", "est", .x, fixed = TRUE)) %>%        # shorten variable names
  relocate(origin_name:destination_name, .before=state_res) %>% 
  arrange(origin_name,destination_name) # sort records by home county, work county
  
# This little trick is used to convert the appropriate variables from character to numeric.
write.csv(cocox2,"temp1.csv")
cocox3 <- read_csv("temp1.csv") %>% 
  select(-c(...1))
############################################################################################
# Create simple correspondence files for Bay Area counties of residence and work
baycounty_r <- data.frame(county_res = c('001','013','041','055','075','081','085','095','097')) %>% 
     mutate(bayco_r_flag=1)
baycounty_w <- data.frame(county_work = c('001','013','041','055','075','081','085','095','097')) %>% 
     mutate(bayco_w_flag=1)
###########################################################################################
coco4 <- full_join(cocox3,baycounty_r, by="county_res") %>% 
         replace(is.na(.), 0)
coco4 <- full_join(coco4,baycounty_w, by="county_work") %>% 
         replace(is.na(.), 0)

coco4 <- coco4 %>% 
   mutate(bay_type = case_when((bayco_r_flag=="1" & bayco_w_flag=="1") ~ "intraregional",
                               (bayco_r_flag=="1" & bayco_w_flag=="0") ~ "outcommute",
                               (bayco_r_flag=="0" & bayco_w_flag=="1") ~ "incommute"))
intraregional <- coco4 %>% 
   filter(bay_type=="intraregional")
outcommute    <- coco4 %>% 
  filter(bay_type=="outcommute")
incommute    <- coco4 %>% 
  filter(bay_type=="incommute")
##########################################################################################
## Export these files into csv format
setwd("~/Desktop/CTPP1721_Data/B302103")

write.csv(coco4,"california_county2county_workermeans_b302103.csv") 
write.csv(intraregional,"bayarea_c2c_intraregional_workermeans_b302103.csv") 
write.csv(outcommute,"bayarea_c2c_outcommute_workermeans_b302103.csv") 
write.csv(incommute,"bayarea_c2c_incommute_workermeans_b302103.csv") 

########################################################################################
