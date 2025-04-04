########################################################################
#  ctpp1721_california_b302100.r
#   Analyze CTPP 2017-2021 data for California     
#   county-to-county total workers, intra-state California
#   Table B302100
#     -- March 22, 2025 --
########################################################################
library(tidyverse)
library(tidycensus)
library(janitor)

#  Data file downloaded from AASHTO site: 
#     https://ctppdata.transportation.org/#/index

setwd("~/Desktop/CTPP1721_Data/B302100")

# subtle differences between read.csv and read_csv.... 
#  read_csv imports the variable b302100_e1 as numeric....

coco1 <- read.csv("B302100_2025-03-22.csv",skip=1) 

coco2 <- read_csv("B302100_2025-03-22.csv",skip=1)


coco3 <- coco2 %>% 
  separate_wider_position(geoid,c(dummy1=7,state_res=2,county_res=3,
                                  state_work=2,county_work=3), cols_remove=FALSE) %>%
  mutate(moe = str_remove_all(b302100_m1,",")) %>% 
  mutate(moe = str_remove_all(moe,"[+/-]")) %>% 
  mutate(moe_num = as.numeric(moe)) %>% 
    rename(totworker     = b302100_e1,
           totworker_moe = moe_num) %>% 
  relocate(origin_name, .before=destination_name) %>% 
  select(-dummy1, -moe) %>% 
  relocate(origin_name:destination_name, .before=state_res) %>% 
  relocate(totworker, .after=destination_name) %>% 
  relocate(totworker_moe, .after=totworker) %>% 
  arrange(origin_name,destination_name)

###########################################################################################
# Create simple correspondence files for Bay Area counties of residence and work
baycounty_r <- data.frame(county_res = c('001','013','041','055','075','081','085','095','097')) %>% 
     mutate(bayco_r_flag=1)
baycounty_w <- data.frame(county_work = c('001','013','041','055','075','081','085','095','097')) %>% 
     mutate(bayco_w_flag=1)
###########################################################################################
coco4 <- full_join(coco3,baycounty_r, by="county_res") %>% 
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
setwd("~/Desktop/CTPP1721_Data/B302100")

write.csv(coco4,"california_county2county_totworker_b302100.csv") 
write.csv(intraregional,"bayarea_c2c_intraregional_totworker_b302100.csv") 
write.csv(outcommute,"bayarea_c2c_outcommute_totworker_b302100.csv") 
write.csv(incommute,"bayarea_c2c_incommute_totworker_b302100.csv") 

########################################################################################
