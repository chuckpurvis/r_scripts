################################################
#  PL94171_places_called_eden.r
#  Pull places, counties with "Eden" in their name
#  Detailed Township Analysis by CCDs.
#    - April 3, 2023 --
#    - August 25, 2023 --
##################################################

library(tidyverse)
library(tidycensus)

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
#####################################################################
usa_place  <- get_decennial(year=2020,  sumfile="pl", 
                            geography = "place",
                            geometry=TRUE, keep_geo_vars=TRUE,
                            show_call = TRUE,output="wide", 
                            variables = selvars20) %>% 
              replace(is.na(.),0)

usa_county  <- get_decennial(year=2020,  sumfile="pl", 
                            geography = "county",
                            geometry=TRUE, keep_geo_vars=TRUE,
                            show_call = TRUE,output="wide", 
                            variables = selvars20) %>% 
              replace(is.na(.),0)

allstates <- c("AK","AL","AR","AZ","CA","CO","CT","DE","FL","GA",
               "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD",
               "ME","MI","MN","MO","MS","MT","NC","ND","NE","NH",
               "NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC",
               "SD","TN","TX","UT","VA","VT","WA","WI","WV","WY",
               "DC","PR")

usa_cousub  <- get_decennial(year=2020,  sumfile="pl", 
                             state= allstates,
                             geography = "county subdivision",
                          #   geometry=TRUE, keep_geo_vars=TRUE,
                             show_call = TRUE,output="wide", 
                             variables = selvars20) %>% 
              replace(is.na(.),0)

usa_cousub <- usa_cousub %>% 
           separate_wider_position(GEOID,c(state=2,county=3,cousub=5))

stusps_code <- usa_county %>% 
               sf::st_drop_geometry()  %>%   # drop the geometry 
              arrange(STUSPS) %>% 
              group_by(STUSPS) %>% 
              count(STUSPS)  # tally of counties by state!

######################################################################
# Filter places, counties by particular NAMEs of places, counties.

eden1 <- usa_place  %>% 
    filter(NAME.x %in% c("Eden"," Eden","Eden-"))

eden2 <- usa_place %>% 
    filter(str_detect(NAME.x,"Eden"))

eden3 <- usa_place %>% 
    filter(grepl("Eden", NAME.x))

olive1 <- usa_place %>% 
    filter(grepl("Olive", NAME.x))

paradise <- usa_place %>% 
  filter(grepl("Paradise", NAME.x))

township <- usa_place %>% 
  filter(grepl("Town", NAME.x))

township2 <- usa_place %>% 
  filter(grepl("township", NAME.x))

#  Analyze the CCD file for Townships.....
township_ccd <- usa_cousub %>% 
  filter(grepl("township", NAME))

cousub_tally <- usa_cousub %>% 
  sf::st_drop_geometry()  %>%   # drop the geometry 
  group_by(state) %>% 
  count() %>% 
  rename(STATEFP=state,n_cousub=n) # tally of cousubs by state!

place_tally <- usa_place %>% 
  sf::st_drop_geometry()  %>%   # drop the geometry 
  group_by(STATEFP) %>% 
  count() %>% 
  rename(n_places=n) # tally of PLACES by state!

township_tally <- township_ccd %>% 
  mutate(STATEFP=state) %>% 
  group_by(STATEFP) %>% 
  count()  %>% 
  rename(n_townships=n)   # Tally of CCDs with "township" in their names, by state!
 
states <- usa_county %>% 
  sf::st_drop_geometry()  %>%   # drop the geometry 
  group_by(STATEFP,STATE_NAME) %>% 
  count()   %>% 
  rename(n_counties=n)    # Tally of counties by state

# Join together the state-level tally of counts of county subdivisions,
#     townships and places......export to CSV file!!
states2 <- full_join(states,cousub_tally,by="STATEFP") %>% 
           full_join(.,township_tally,by="STATEFP") %>%
           full_join(.,place_tally,by="STATEFP") %>%
  replace(is.na(.),0)

setwd("~/Desktop/tidycensus_work/output")
write.csv(states2,  "census2020_county_township_cousub_place_tally_by_state.csv")

#################################################################### 
prairie <- usa_place %>% 
  filter(grepl("Prairie", NAME.x))

cupcake <- usa_place %>% 
  filter(grepl("Cupcake", NAME.x))

#
eden4 <- usa_county %>% 
  filter(grepl("Eden", NAME.x))

###
# Most common county names!

county1 <- usa_county %>% 
  group_by(NAME.x) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  sf::st_drop_geometry()   # drop the geometry so we can write a nice csv file!
  
county2 <- left_join(usa_county,county1,by="NAME.x") %>% 
  arrange(desc(n),NAME.x,GEOID) 
  

# Most common place names!

places1 <- usa_place %>% 
  group_by(NAME.x) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  sf::st_drop_geometry()   # drop the geometry so we can write a nice csv file!

places2 <- left_join(usa_place,places1,by="NAME.x") %>% 
  arrange(desc(n),NAME.x,GEOID)