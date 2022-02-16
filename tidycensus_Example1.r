######################################################
#  tidycensus_Example1.r
# Example #1 
# Simple Examples of TIDYCENSUS
# One Year, Multiple Geographies, Multiple Variables
#  Prepared by Chuck Purvis, Hayward, California
######################################################

# Step 0: Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)
library(janitor)

# Simple Example #1.1: Population by Race/Ethnicity, 2018, SF Bay Area Counties, Table C03002
#  Note that tidycensus can use either the County Name or the County FIPS Code number.
#  Experiment with output="wide" versus output="tidy" ("tidy" is the default.)
#############################################################################################
county1   <- get_acs(survey="acs1", year=2018, geography = "county", state = "CA", 
                   # county=c(1,13,41,55,75,81,85,95,97),
                    county=c("Alameda","Contra Costa","Marin","Napa","San Francisco",
                              "San Mateo","Santa Clara","Solano","Sonoma"),
                   show_call = TRUE,  output="wide",
                   table="C03002")
view(county1)

# Simple Example #1.2: Population by Race/Ethnicity, 2014-2018, All California Counties, Table B03002
#    If the list of counties is excluded, then data is pulled for all counties in the State
#############################################################################################
AllCalCounties   <- get_acs(survey="acs5", year=2018, geography = "county", state = "CA", 
                     show_call = TRUE,  output="wide", table="B03002")
view(AllCalCounties)

# Simple Example #1.3: Population by Race/Ethnicity, 2018, California Congress Dists, Table C03002
#   This example pulls the congressional districts from California. Eliminate state="CA" to get
#   congressional districts from the entire United States
#############################################################################################
congdist1 <- get_acs(survey="acs1", year=2018, geography = "congressional district", state = "CA", 
                   show_call = TRUE, output="wide",
                   table="C03002")
view(congdist1)

# Simple Example #1.4.1: Population by Race/Ethnicity: Bay Counties: Naming Variables.
#  User-defined mnemonic variable names, since "C03002_001_E" doesn't fall trippingly on the tongue!
#  the underscore is useful since tidycensus will append "E" to estimates and "M" to margin of error
#  variables, e.g., "Total_E" and "Total_M"
#############################################################################################
county2   <- get_acs(survey="acs1", year=2018, geography = "county", state = "CA", 
                     county=c(1,13,41,55,75,81,85,95,97),
                     show_call = TRUE, output="wide",
                   variables = c(Total_    = "C03002_001",  # Universe is Total Population
                                 White_NH_ = "C03002_003",  # Non-Hispanic White
                                 Black_NH_ = "C03002_004",  # Non-Hispanic Black
                                 AIAN_NH_  = "C03002_005",  # NH, American Indian & Alaskan Native
                                 Asian_NH_ = "C03002_006",  # Non-Hispanic Asian
                                 NHOPI_NH_ = "C03002_007",  # NH, Native Hawaiian & Other Pacific Isl.
                                 Other_NH_ = "C03002_008",  # Non-Hispanic Other
                                 Multi_NH_ = "C03002_009",  # Two-or-More Races, Non-Hispanic
                                 Hispanic_ = "C03002_012")) # Hispanic/Latino

# Sometimes the results of TIDYCENSUS aren't sorted, so: 
county2 <- county2[order(county2$GEOID),] 
view(county2)
#############################################################################################
# Simple Example #1.4.2: Add a new record: San Francisco Bay Area, as sum of records 1-9
# adorn_totals is a function from the package janitor.
# The name="06888" is arbitrary, just a filler for the GEOID column.

tempxxx <- adorn_totals(county2,name="06888")
tempxxx[10,2]="San Francisco Bay Area"

county3 <- tempxxx

# Set a working directory, and write out CSV files as wanted.
# This is an example for a Mac, with the folder tidycensus_work on the desktop, and the
#   folder output within tidycensus_work
setwd("~/Desktop/tidycensus_work/output")

write.csv(county3,"ACS18_BayAreaCounties.csv")
#############################################################################################