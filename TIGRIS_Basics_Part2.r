#######################################################
#  TIGRIS_Basics_Part2.r
#  Scripts for testing various attributes of
#  the R package TIGRIS.... TIGRIS is used to 
#  directly download US Census Bureau TIGER/Line
#  shapefiles, into R's "simple features objects" (SF)
#
# Examples:
#  1   -- Roads for Counties, Multiple Counties within State
#  2   -- Primary & Secondary Roads within a State
#  3   -- Rails for the USA
#  4.1 -- Water Areas for Multiple Counties within a State
#  4.2 -- Water Lines for Multiple Counties within a State
#  5   -- Point & Area Landmarks within a State
#
# Updated August 7, 2021
#######################################################

# Install the TIGRIS package.
# Install the GGPLOT2 package.
# Install the MAPVIEW package.

install.packages('tigris')
install.packages('ggplot2')
install.packages('mapview')

# Load the libraries into the current session.

library(tigris)
library(ggplot2)
library(mapview)

# Cache management. Default is TRUE, to caches to SHP files in an R session.

options(tigris_use_cache = TRUE)

# options(tigris_refresh = TRUE)

# Current (March 2021) version of TIGRIS defaults to 2019.
# Version 1.4.1 works with 2020!!!

options(tigris_year=2020)

# In case you haven't memorized the FIPS codes for all states
# and counties in the United States, this command creates a dataframe
# that can be viewed and referenced:
data(fips_codes)

# or use variations on this command:
lookup_code("Ca","contra")
lookup_code("California","S")
lookup_code("NV")
lookup_code("NV","Cl")
lookup_code(state="NY",county="Queens")

list_counties("CA") # returns a list of counties within a state in the R console

###############################################################
# Example #1 - Download Roads for a Given State, County

# Either way of describing counties will work OK.....
mycounties <- c('01','13','41','55','75','81','85','95','97')

mycounties2 <- c('Ala','Contra','Marin','Napa','San Fran','San Mat',
                 'Santa Cl','Sol','Sonoma County')

BayRoads <- roads(state="CA",county=mycounties2, year = 2019) 

# A mapview of All Bay Area Roads crashes R-Studio on my iMac. Be careful!
#  mapview(BayRoads)

SFRoads <- roads("06","San Francisco County", year = 2020)
mapview(SFRoads)

# bare plot using ggplot....
ggplot(SFRoads) + 
  geom_sf() + 
  theme_void()

# You can use the first few letters of the county name....
# But "San Be" won't work since we have San Benito and San Bernardino Counties.
SBRoads <- roads("CA","San Ben")
mapview(SBRoads)

###############################################################
# Example #2 - Download Primary+Secondary Roads in a State
# These are essentially federal, interstate and state highways.
#  Can't select below state level.

Calif_MainRoads <- primary_secondary_roads(state="CA",year = 2020) 

mapview(Calif_MainRoads)

###############################################################
# Example #3 - Download Railroads for the Nation
#  Can't select on state or county for railroads.....

USARail <- rails(year = 2019) # n=140,338 observations

mapview(USARail)

USARail2 <- rails(year = 2020) # n=130,086 observations

mapview(USARail2)

###############################################################
# Example #4.1 - Download Water Areas for a Given State, County

mycounties <- c('01','13','41','55','75','81','85','95','97')

BayWater <- area_water(state="CA",county=mycounties, year = 2020)

mapview(BayWater)

###############################################################
# Example #4.2 - Download Water Lines for a Given State, County
#  Streams, creeks, rivers, sloughs, canals, etc.

mycounties <- c('01','13','41','55','75','81','85','95','97')

BayWater2 <- linear_water(state="CA",county=mycounties, year = 2020)

mapview(BayWater2)

###############################################################
# Example #5  - Download Point & Area Landmarks in a State

Calif_Landmarks_Point <- landmarks(state="CA", type="point", year = 2020) 
Calif_Landmarks_Area  <- landmarks(state="CA", type="area",  year = 2020) 

mapview(Calif_Landmarks_Point)
mapview(Calif_Landmarks_Area)

###############################################################
