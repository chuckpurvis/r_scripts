########################################################################
#  cd119_ACS1923_RaceEthnicDiversity_CongDist.r
#   Analyze Population by Race by Hispanic/Latino, Table C03002     
#   All US Congressional Districts, USA
#   American Community Survey, 5-year, 2019-2023 data
#     -- December 15, 2020 --
#     -- December 12, 2024 --
#     -- February 21, 2025 --
########################################################################
# Load relevant libraries into R-session.

library(tidyverse)
library(tidycensus)
library(janitor)

# Read in Master 119th Congressional District File
# created in:
# "~/Desktop/tidycensus_work/cd119th_master_names_1.R"

setwd("~/Desktop/Politics_Elections")
masterx <- readRDS("cd119th_master_names_census2020_437_extended.rda")

setwd("~/Desktop/cd118_file")
cd_base3 <- readRDS("usa_cd119_congdist_addvars_top3_top5.rda")

#####

selvars <- c(total_    = "B03002_001",  # Universe is Total Population
               nh_white_ = "B03002_003",  # White, NH = Non-Hispanic
               nh_black_ = "B03002_004",  # Black
               nh_aian_  = "B03002_005",  # American Indian & Alaskan Native
               nh_asian_ = "B03002_006",  # Asian
               nh_nhopi_ = "B03002_007",  # Native Hawaiian & Other Pacific Islander
               nh_other_ = "B03002_008",  # Other Race
               nh_multi_ = "B03002_009",  # Two-or-More Races, Non-Hispanic
               hispanic_ = "B03002_012")

# Pull 5-year 2019-2023 data: All US Congressional Districts

congress1  <- get_acs(survey="acs5", year=2023, geography = "congressional district",  
                        show_call = TRUE, output="wide",
                        variables = selvars) %>% 
              arrange(GEOID) %>% 
              clean_names()

# Pull 5-year 2018-23 data: USA nationwide

usa1  <- get_acs(survey="acs5", year=2023, geography = "us", 
                   show_call = TRUE, output="wide",
                   variables = selvars) %>% 
              clean_names()

###
# Concatenate these output dataframes....congress + USA
  congress2 <- rbind(congress1,usa1)
  
# tidyverse / dplyr script to calculate a shannon diversity index. mutate-at-work!!
  congress3 <- mutate(congress2,
                      # Calculate two new variables for a five-group analysis
     nh_asianpi_e     = nh_asian_e + nh_nhopi_e,
     nh_other3_e      = nh_aian_e  + nh_other_e + nh_multi_e,
                      
  # Share of Population by Five Race/Ethnicity Categories
     share_nh_white    = nh_white_e / total_e,
     share_nh_black    = nh_black_e / total_e,
     share_nh_asianpi  = nh_asianpi_e / total_e,
     share_nh_other3   = nh_other3_e   / total_e,
     share_hispanic     = hispanic_e / total_e,
                      
  # This is preferred if there are shares == 0.000 
     ln_g120 = ifelse(share_nh_white > 0, log(share_nh_white) * share_nh_white,0),
     ln_g220 = ifelse(share_nh_black > 0, log(share_nh_black) * share_nh_black,0),
     ln_g320 = ifelse(share_nh_asianpi > 0, log(share_nh_asianpi) * share_nh_asianpi,0),
     ln_g420 = ifelse(share_nh_other3 > 0, log(share_nh_other3) * share_nh_other3,0),
     ln_g520 = ifelse(share_hispanic > 0, log(share_hispanic) * share_hispanic,0),
                      
  # Calculate a Shannon Diversity Index, based on the Five Race/Ethnic Categories
  #   Multiplying the Diversity Index by 100 is optional
  #   It now ranges from 0.0 (only one kind of race/ethnicity) 
  #      to 1.0 (totally equal)
                      
  diversity = 100 * ((ln_g120 + ln_g220 + ln_g320 + ln_g420 + ln_g520)/(log(0.2)))) %>% 
    dplyr::relocate(diversity, .before= total_e) %>% 
    relocate(share_nh_white:share_hispanic, .after=total_e) %>% 
    mutate(share_minority = 1-share_nh_white) %>% 
    relocate(share_minority, .after=total_e)
# End of Mutate Step (Create Diversity Variables!)
####################################################################################
# Merge ACS 2019-2023 with master name file for 119th Congress
congress3 <- congress3 %>% 
      rename(GEOID=geoid)
congress4 <- left_join(cd_base3,congress3, by="GEOID") %>% 
   relocate(diversity:share_hispanic, .after=GEOID)

congress5 <- congress4 %>% 
   select(GEOID:share_hispanic,fullname3,top3places) %>% 
   relocate(fullname3, .before=GEOID) %>% 
   relocate(diversity, .before=GEOID) %>% 
   relocate(top3places, .before=GEOID)

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel, and applying finishing touches
write.csv(congress5,"cd119_acs1923_diversity_congdist.csv")

#####################################################################################