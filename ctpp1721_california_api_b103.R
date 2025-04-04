############################################################
#  ctpp1721_california_api_b103.r
#    Use the AASHTO CTPP Program's API to obtain data
#
#  Convert JSON files into data frames
#   JSON = JavaScript Object Notation.....
#  Clean up variable names
#
#       -- March 23, 2025 --
###########################################################
library(jsonlite)
library(tidyverse)
library(tidycensus)

# Total Population by California's 52 Congressional Districts
#   from the Census Bureau's CD118 file, 2020 Decennial Census

temp1 <- fromJSON("https://api.census.gov/data/2020/dec/cd118?get=NAME,P1_001N&for=congressional%20district:*&in=state:06")

cd_calif <- as.data.frame(temp1)

temp2 <- fromJSON("https://ctppdata.transportation.org/api/groups/b301100/geographies?year=2021")

df2 <- as.data.frame(temp2)