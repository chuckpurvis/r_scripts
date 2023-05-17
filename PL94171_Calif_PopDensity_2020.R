###################################################################
# PL94171_Calif_PopDensity_2020.r
#  Use the R-package TIDYCENSUS to extract PL 94-171 
#   data from the Year 2020 Decennial Censuses
#  California State, different geographies: state, county, tract,
#     block group and tract
#
# The intention is to complement my LEHDR analysis on workers-at-work
#    density, started on 5/15/2023 using the LODES8 version of lehdr
#      -- May 17, 2023 --
###################################################################
library(tidyverse)
library(tidycensus)

options(tigris_use_cache=TRUE)
# load the variable list for 2020 into a dataframe
varlist20 <- load_variables(2020,"pl",cache=FALSE)

selvars20  <- c(TotalPop20   = "P2_001N",   # Total Population
                Hispanic20   = "P2_002N",   # Hispanic or Latino
                NH_White20   = "P2_005N",   # Non-Hispanic, White alone
                NH_Black20   = "P2_006N",   # Non-Hispanic, Black or African American alone
                NH_AIAN20    = "P2_007N",   # Non-Hispanic, American Indian, Alaskan Native alone
                NH_Asian20   = "P2_008N",   # Non-Hispanic, Asian alone
                NH_NHOPI20   = "P2_009N",   # Non-Hispanic, Native Hawaiian, Other Pac Islander alone
                NH_Other20   = "P2_010N",   # Non-Hispanic, Other race alone
                NH_Multi20   = "P2_011N",   # Non-Hispanic, Two-or-More Races
                
                HousingUnits20  = "H1_001N", # Total Housing Units
                Occ_DU20        = "H1_002N", # Occupied Housing Units
                Vacant_DU20     = "H1_003N") # Vacant Housing Units
###############################################################################################
# Get selected PL94-171 variables for California block groups, Census 2020, TIDYCENSUS program
#   and calculate population density variables

# Step 1a. just a simply tidycensus pull
calif_bg  <- get_decennial(year=2020,  sumfile="pl", 
                               geography = "block group", state="CA",
                               geometry=TRUE, keep_geo_vars=TRUE,
                               show_call = TRUE,output="wide", variables = selvars20) 
 
# Step 1b. Cleanup, and add density variables. 
calif_bg2 <- calif_bg %>% 
  filter(STATEFP=="06") %>%  # deletes some phantom block groups with zero land area
  replace(is.na(.),0) %>%  # replace all NA with 0
  dplyr::mutate(totpop = TotalPop20,
                landarea_sqmi = ALAND / 2589988.1,
                totpop_sqmi  = totpop / landarea_sqmi) %>% 
  replace(is.na(.),0)  %>%  # replace all NA with 0
  dplyr::  mutate(density_grp = case_when(
    totpop_sqmi < 500 ~    "1.rural",
    totpop_sqmi < 1000 ~   "2.rural-suburb",
    totpop_sqmi < 6000 ~   "3.suburb-disperse",
    totpop_sqmi < 10000 ~  "4.suburb-dense",
    totpop_sqmi < 20000 ~  "5.urban",
    totpop_sqmi >= 20000 ~ "6.urban core",
    TRUE ~ "0.missing data")) %>% 
  dplyr:: relocate(totpop:density_grp, .after= NAME.y)
#####################################################################################
#  Select SF Bay Area block groups for mapping.....
bayarea_bg_resid_2020 <- calif_bg2 %>% 
  dplyr::filter(COUNTYFP %in% c("001","013","041","055","075","081","085","095","097"))

suggest_crs(bayarea_bg_resid_2020) # get a listing of suggested coord refs for this area
suggest_top_crs(bayarea_bg_resid_2020) # top suggested for this area

bayarea_bg_resid_2020_p <- st_transform(bayarea_bg_resid_2020,26942) # NAD83, Calif Zone 2

ggplot() + 
  geom_sf(data=bayarea_bg_resid_2020_p, alpha=1.0, aes(fill=density_grp), color=NA) + 
  scale_fill_manual(values=c("seagreen", "seagreen1","yellow","tan1","red","purple")) +
  theme_void() + 
  labs(title = "Population Density, San Francisco Bay Area: 2020",
       subtitle = "Census Bureau PL 94-171 for 2020: Block Group level",
       caption = "Created: CPurvis; Data sources: 2020 Census, PL 94-171",
       fill = "Population Density Level")

setwd("~/Desktop/LEHD Research")

ggsave("sfbayarea_population_density_2020.png", plot=last_plot(),
       dpi=300,width=8,height=7) 
###################################################################################
#  Select San Francisco County block groups for mapping.....
county75_bg_resid_2020 <- calif_bg2 %>% 
  dplyr::filter(COUNTYFP %in% c("075")) # %>% 
  #   st_transform(26942)
  
ggplot() + 
  geom_sf(data=county75_bg_resid_2020, alpha=1.0, aes(fill=density_grp), color=NA) + 
  scale_fill_manual(values=c("seagreen", "seagreen1","yellow","tan1","red","purple")) +
  theme_classic() + 
  labs(title = "Population Density, San Francisco County: 2020",
       subtitle = "Census Bureau PL 94-171 for 2020: Block Group level",
       caption = "Created: CPurvis; Data sources: 2020 Census, PL 94-171",
       fill = "Population Density Level") +
  xlim(-122.55,-122.32) + ylim(37.70,37.83) # This is needed to censor farwest Farralon Islands!

setwd("~/Desktop/LEHD Research")

ggsave("sfcounty_population_density_2020.png", plot=last_plot(),
       dpi=300,width=8,height=7) 

###################################################################################

