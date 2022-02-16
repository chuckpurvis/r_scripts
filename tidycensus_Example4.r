Example #4. Explore the Geographies that Tidycensus can pull out.

This is an example of using tidycensus to extract 2014/18 ACS data for all available geographies for the entire USA. It’s just a test of capabilities.

Some geographic levels (county subdivision, state upper and lower houses) don’t appear to be working. 

There is some extra code in the “PUMA step” to tally the number of PUMAs per US state, and the minimum and maximum total population levels for each PUMA in the US.

# Step 0: Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)
library(janitor)
library(plyr) # This is needed for a function to concatenate a lot of files in one statement!
library(dplyr)

# Add the variable Geography_Name to each data frame. Maybe concatenate/pancake these dataframes?

selvars  <- c(TotalPop_   = "B06001_001", # Total Population
              SamplePop_  = "B00001_001",  # Unweighted Sample Count of Population
              HHUnits_    = "B25002_001", # Total Housing Units 
              Househlds_  = "B25002_002", # Total Households 
              SampleDU_   = "B00002_001") # Unweighted Sample Count of Dwelling Units
#------------------------------------------------------------------------------------
us  <- get_acs(survey="acs5", year=2018, geography = "us",
               show_call = TRUE,output="wide", variables = selvars)
us$Geography_Name <- "us"
#------------------------------------------------------------------------------------
region  <- get_acs(survey="acs5", year=2018, geography = "region",
                   show_call = TRUE,output="wide", variables = selvars)
region$Geography_Name <- "region"
#------------------------------------------------------------------------------------
division  <- get_acs(survey="acs5", year=2018, geography = "division",
                     show_call = TRUE,output="wide", variables = selvars)
division$Geography_Name <- "division"
#------------------------------------------------------------------------------------
state  <- get_acs(survey="acs5", year=2018, geography = "state",
                  show_call = TRUE,output="wide", variables = selvars)
state$Geography_Name <- "state"

setwd("~/Desktop/tidycensus_work/output")

write.csv(state,"ACS1418_USA_State_1.csv")
#------------------------------------------------------------------------------------
county  <- get_acs(survey="acs5", year=2018, geography = "county",
                   show_call = TRUE,output="wide", variables = selvars)
county$Geography_Name <- "county"
#------------------------------------------------------------------------------------
# County Subdivision isn't working ...returns an API error (unknown/unsupported geography)
countysubdiv  <- get_acs(survey="acs5", year=2018, geography = "county subdivision",
                         show_call = TRUE,output="wide", variables = selvars)
countysubdiv$Geography_Name <- "countysubdiv"
#------------------------------------------------------------------------------------
# Pull just the tracts in Alameda County, California
tract  <- get_acs(survey="acs5", year=2018, geography = "tract",  state="CA",  
            county="Alameda",show_call = TRUE,output="wide", variables = selvars)
tract$Geography_Name <- "tract"
#------------------------------------------------------------------------------------
# Pull just the block groups in Alameda County, California
blockgroup<- get_acs(survey="acs5", year=2018, geography = "block group", state="CA",       county="Alameda",show_call = TRUE,output="wide", variables = selvars)
blockgroup$Geography_Name <- "blockgroup"
#------------------------------------------------------------------------------------
place  <- get_acs(survey="acs5", year=2018, geography = "place",
                  show_call = TRUE,output="wide", variables = selvars)
place$Geography_Name <- "place"
#------------------------------------------------------------------------------------
urban  <- get_acs(survey="acs5", year=2018, geography = "urban area",
                  show_call = TRUE,output="wide", variables = selvars)
urban$Geography_Name <- "urban"
#------------------------------------------------------------------------------------
congdist  <- get_acs(survey="acs5", year=2018, geography = "congressional district",
                  show_call = TRUE,output="wide", variables = selvars)
congdist$Geography_Name <- "congdist"
#------------------------------------------------------------------------------------
puma  <- get_acs(survey="acs5", year=2018, geography = "public use microdata area",
                  show_call = TRUE,output="wide", variables = selvars)

puma$TotalPop2      <- puma$TotalPop_E * 1.0
puma$Tally          <- 1.0
puma$State          <- substr(puma$GEOID,1,2)
puma$Geography_Name <- "puma"
pumas <- puma[order(puma$State,puma$GEOID),] 
summary(pumas)

sum1 <- aggregate(pumas[,3:12],
          by = list(pumas$State),
          FUN = sum, na.rm=TRUE)

min1 <- aggregate(pumas[,3:12],
                  by = list(pumas$State),
                  FUN = min, na.rm=TRUE)

max1 <- aggregate(pumas[,3:12],
                  by = list(pumas$State),
                  FUN = max, na.rm=TRUE)

setwd("~/Desktop/tidycensus_work/output")

write.csv(sum1,"ACS1418_USA_PUMA_sum_by_State_1.csv")
write.csv(min1,"ACS1418_USA_PUMA_min_by_State_1.csv")
write.csv(max1,"ACS1418_USA_PUMA_max_by_State_1.csv")

write.csv(pumas,"ACS1418_USA_PUMA_All_1.csv")

sum2 <- pumas %>% 
  group_by(State) %>% 
  summarize_at("TotalPop_E",
               list(name=sum))
#------------------------------------------------------------------------------------
csa  <- get_acs(survey="acs5", year=2018, geography = "combined statistical area",
                 show_call = TRUE,output="wide", variables = selvars)
csa$Geography_Name <- "csa"
#------------------------------------------------------------------------------------
msamisa  <- get_acs(survey="acs5", year=2018, geography = "metropolitan statistical area/micropolitan statistical area",
                 show_call = TRUE,output="wide", variables = selvars)
msamisa$Geography_Name <- "msamisa"
#------------------------------------------------------------------------------------
zcta  <- get_acs(survey="acs5", year=2018, geography = "zcta",
                 show_call = TRUE,output="wide", variables = selvars)
zcta$Geography_Name <- "zcta"
#------------------------------------------------------------------------------------
# State Senate and House aren't working ...returns an API error (unknown/unsupported geography)
statesenate  <- get_acs(survey="acs5", year=2018, geography = "state legislative district (upper chamber)",
                show_call = TRUE,output="wide", variables = selvars)
statesenate$Geography_Name <- "statesenate"
#------------------------------------------------------------------------------------
statehouse   <- get_acs(survey="acs5", year=2018, geography = "state legislative district (lower chamber)",
                show_call = TRUE,output="wide", variables = selvars)
statehouse$Geography_Name <- "statehouse"
#------------------------------------------------------------------------------------

This concludes Example #4: “exploring tidycensus geography.”



