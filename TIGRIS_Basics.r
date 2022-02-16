#######################################################
#  TIGRIS_Basics.r
#  Scripts for testing various attributes of
#  the R package TIGRIS.... TIGRIS is used to 
#  directly download US Census Bureau TIGER/Line
#  shapefiles, into R's "simple features objects" (SF)
#
#  updated analysis on 8/7/21, using newest TIGRIS 1.4.1
#   Kyle Walker updated TIGRIS in July 2021...good idea to re-install 
#      R packages on an ever-so-often basis.
#
#   The cartographic boundaries option now works AOK for Year 2020 geos.
#
# Examples:
#  1.1  -- County Boundaries in California (Detailed TIGER/Highest Resolution)
#  1.2  -- Exporting SHP files to a local computer drive
#    2  -- County Boundaries in California (Medium Resolution)
#    3  -- County Boundaries in California (Lowest Resolution)
#  4.1  -- Tract Boundaries for One County
#  4.2  -- Tract Boundaries for One County, Multiple Census Years (1990-2020)
#    5  -- Tract Boundaries for Multiple Counties in a State
#    6  -- Block Group Boundaries for Multiple Counties in a State
#    7  -- Block Boundaries for Multiple Counties in a State
#    8  -- Place Boundaries within a State
#    9  -- PUMA Boundaries within a State
#   10  -- Consolidated Statistical Areas in the USA
#   11  -- Core-Based Statistical Areas in the USA
#   12  -- Congressional Districts in the USA, single state, and filtering states
#   13  -- Urbanized Areas in the USA
#   14  -- State Legislative Districts (Upper/Lower House) for a State
#   15  -- Zip Code Tabulation Areas, Selected for a State and within state
#   16  -- State Boundaries in the USA
#
#  March 12, 2021 
#  August 7, 2021 (updated for 2020 Census TIGER)
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
# Well, I think the new default is 2020, but it's probably a good habit
#  to be explicit in your TIGRIS call to use year=2020 for Census 2020
#  and probably year=2019 for Census 2010 and ACS data through 2019!!!

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

# Example #1.1 - County Boundaries in California
#  These are the most detailed boundaries, but appear odd since
#  the county boundaries are not "clipped" to the shoreline!

ca_counties <- counties(state = "CA")

ca_counties

plot(ca_counties$geometry)

###################################################################

# Example #1.2
# Export SHP files to a local computer drive!!

# Set directory for exported data files, MacOS directory style
setwd("~/Desktop/tidycensus_work/gis")
library(sf)

st_write(ca_counties, "ca_counties_2019.shp", append=FALSE)

###################################################################

# Example #2 - County Boundaries in California, with 
#   "cartographic boundaries" (cb=TRUE), which are generalized
#   boundaries, and clipped to shorelines.

ca_counties2 <- counties(state = "CA", cb=TRUE)

ca_counties2

plot(ca_counties2$geometry)

###################################################################

# Example #3 - County Boundaries in California, with 
#   "cartographic boundaries" (CB=TRUE), which are generalized
#   boundaries, and clipped to shorelines.
#   Default resolution in most CB files is 5m (1:5,000,000). A coarser
#   resolution 1:20,000,000 is shown here: 

ca_counties3 <- counties(state = "California", cb=TRUE, resolution='20m')

ca_counties3

plot(ca_counties3$geometry)

#  View the previous examples using the MAPVIEW package in R!

mapview(ca_counties)
mapview(ca_counties2)
mapview(ca_counties3)

# Examples of basic mapping using ggplot2

temp1 <- ggplot()
temp1 <- temp1 + geom_sf(data=ca_counties2, color='black', 
                         fill='white', size=0.25)
temp1

###################################################################

# Example #4.1  Downloading tract level SHP files for
#  one county.

ala_tracts1 <- tracts(state = "CA", county = "Alameda")

plot(ala_tracts1$geometry)

ala_tracts2 <- tracts(state = "CA", county = "Alameda", cb = TRUE)

plot(ala_tracts2$geometry)

ala_tracts3 <- tracts(state = "CA", county = "Alameda", cb = TRUE, year=2010)

plot(ala_tracts3$geometry)

ala_tracts4 <- tracts(state = "CA", county = "Alameda", year=2020)

plot(ala_tracts4$geometry)

# The cartographic boundaries are not yet available for 2020, as of March 2021!
# The following few lines won't work, right now!

# The CB are working for 2020 now, tested 8/6/2021.

ala_tracts5 <- tracts(state = "CA", county = "Alameda", cb = TRUE, year=2020)

plot(ala_tracts5$geometry)

# Example #4.2  Downloading tract level SHP files for
#  one county for multiple census years
# (taken from Kyle Walker's code for Tarrant County, Texas)

sanfrancisco90 <- suppressMessages(tracts("CA", "75", cb = TRUE, year = 1990))
sanfrancisco00 <- suppressMessages(tracts("CA", "75", cb = TRUE, year = 2000))
sanfrancisco10 <- tracts("CA", "75", cb = TRUE, year = 2010)
# Cartographic boundary files not yet released for 2020 -- OK as of 8/6/21.
sanfrancisco20 <- tracts("CA", "75", cb=TRUE, year = 2020)

# Plot the four census years census tracts for San Francisco.

par(mfrow = c(2, 2))

plot(sanfrancisco90$geometry, main = "1990")
plot(sanfrancisco00$geometry, main = "2000")
plot(sanfrancisco10$geometry, main = "2010")
plot(sanfrancisco20$geometry, main = "2020")

###################################################################

# Example #5 -  Downloading SHP files for tracts within multiple counties
#  within a state, e.g., nine-county San Francisco Bay Area

bay_tracts1 <- tracts(state = "CA", 
                      county = c('01','13','41','55','75','81','85','95','97'),
                      cb = TRUE,
                      year=2020)

plot(bay_tracts1$geometry)
mapview(bay_tracts1)

# Perhaps a cleaner alternative for pulling multiple counties?

baycounties <- c('01','13','41','55','75','81','85','95','97')
bay_tracts2 <- tracts(state = "CA", county = baycounties, cb = TRUE, year=2020)

plot(bay_tracts2$geometry)
mapview(bay_tracts2)

# Same as before, but for Census 2020 tracts, no cb files, though!
#  cartographic boundaries (CB) are now (Aug2021) working with TIGRIS for 2020
#  I sometimes use the terms "cartographic boundaries" with "clipped boundaries"
#    interchangeably.

bay_tracts2020 <-tracts(state = "CA", year=2020,
                        county = baycounties)
mapview(bay_tracts2020)

###################################################################

# Example #6 -  Downloading SHP files for block groups within multiple counties
#  within a state, e.g., nine-county San Francisco Bay Area

baycounties <- c('01','13','41','55','75','81','85','95','97')

bay_BlkGrps1 <- block_groups(state = "CA", county = baycounties, cb = TRUE, year=2020)

mapview(bay_BlkGrps1)

###################################################################

# Example #7 -  Downloading SHP files for blocks within multiple counties
#  within a state, e.g., nine-county San Francisco Bay Area

baycounties <- c('01','13','41','55','75','81','85','95','97')

bay_Blocks1 <- blocks(state = "CA", county = baycounties, year=2020)

# the option cb=TRUE is not available at the block level! hmmm...

# trying to view all 108,000 blocks in MAPVIEW crashed R-Studio.....
# tested viewing all 109,228 blocks in MAPVIEW... and R-Studio is still crashing.
# mapview(bay_Blocks1)

# 109,228 blocks in the 9-County SF Bay Area, 2020 Census.

###################################################################

# Example # 8 -  Downloading SHP files for places within a state
#  Note that "places" may straddle one or more counties, so you can't
#  (directly) acquire a state-county-place set of SHP files!

USplaces <- places(year=2020, cb=TRUE)
mapview(USplaces)

CalPlaces1 <- places(state="CA", year=2019)

mapview(CalPlaces1)

CalPlaces2 <- places(state="06", year=2019, cb = TRUE)

mapview(CalPlaces2)

CalPlaces2020 <- places(state="CA", year=2020, cb=TRUE)

mapview(CalPlaces2020)

###################################################################

# Example #9 -  Downloading SHP files for PUMAs within a state
#  Note that PUMAs may straddle one or more counties, so you can't
#  (directly) acquire a state-county-PUMA set of SHP files!

# Note that year=2020 is an INVALID command for PUMAs!! (8/7/21)
CalPUMA1 <- pumas(state="CA", year=2019, cb = TRUE)

mapview(CalPUMA1)

# The following doesn't work: needs to pull a specific state.
uspuma <- pumas(year=2019, cb= TRUE)

# I tried to get multiple states in one run, but TIGRIS doesn't accept 
# multiple states in this function. It's probably quicker to get PUMAs
# for the entire nation and then subset/filter out the states of interest.

# This DOESN'T WORK. Can't download All USA PUMAs in one call.
# USAPUMA1 <- pumas(year=2019, cb=TRUE)

# mystates <- c("AZ","CA","NV")
# mystates <- c('04','06','32')
# myPUMAs <- pumas(state=mystates, year=2019, cb = TRUE)

# mapview(myPUMAs)

###################################################################

# Example #10 - Download Consolidated Statistical Areas (CSA) for Nation
# Resolution options include 5m and 20m. Resolution defaults to 500k.

CSA1 <- combined_statistical_areas(cb = TRUE, resolution = "500k", year = 2019) 

mapview(CSA1)

# Same number of CSAs in 2020 (175)... 
CSA2 <- combined_statistical_areas(cb = TRUE, resolution = "500k", year = 2020) 

mapview(CSA2)

###################################################################

# Example #11 - Download Core-Based Statistical Area (CBSA) for Nation
# Resolution options include 5m and 20m. Resolution defaults to 500k.

CBSA <- core_based_statistical_areas(cb = TRUE, resolution = "500k", year = 2019) 

mapview(CBSA)

###################################################################

# Example #12 - Download Congressional Districts for USA.
# Resolution options include 5m and 20m. Resolution defaults to 500k.

congdist <- congressional_districts(cb = TRUE, resolution = "500k", year = 2020)

mapview(congdist)

# Grr.... Can't get congressional Districts WITHIN a state.
# Probably have to download all of the US and then filter for states-of-interest!

# OK, now this works!! Yeah!

congdist06 <- congressional_districts(state="CA",cb = TRUE, 
                                      resolution = "500k", year = 2020)
mapview(congdist06)

# The following works!!

library(dplyr) # need to call the package dplyr to use the filter function

congdist06a <- filter(congdist,STATEFP=="06")
mapview(congdist06a)

# Filter/subset Cong. Districts in Arizona, California, and Nevada....

congdist_west <- filter(congdist,STATEFP %in% c("04","06","32"))
mapview(congdist_west)

###################################################################

# Example #13 - Download Urbanized Areas for Nation
# Apparently there are no options for resolution. They appear to be 500k.

urban <- urban_areas(cb = TRUE, year = 2020) 

mapview(urban)

###################################################################

# Example #14 - Download State Legislative Districts (Upper/Lower) for a State
# Resolution options 500k. Resolution defaults to detailed TIGER/Line.

upperhouse <- state_legislative_districts(state="CA", house='upper',       
                  cb = TRUE, year = 2019) 

mapview(upperhouse)

lowerhouse <- state_legislative_districts(state="CA", house='lower',       
                  cb = TRUE, year = 2019) 

mapview(lowerhouse)

###################################################################

# Example #15 - Download Zip Code Tabulation Areas (ZCTA) for a State
# If CB=TRUE, then Resolution is 500k. Resolution defaults to detailed TIGER.
# Year= is aonly available for 2000 and 2010, right now.

# This takes forever for a state as big as California!!

sel_zcta <- zctas(state="CA", cb = TRUE, starts_with="945", year = 2010) 

mapview(sel_zcta)

###################################################################

# Example #16 - Download State Boundaries for the USA
# Options include 500k or Detailed TIGER resoluation, and year= option

states <- states(cb = TRUE, year=2020)
mapview(states)

###################################################################

