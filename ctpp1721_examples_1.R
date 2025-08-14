########################################################################################
# ctpp1721_examples_1.r
#   Several examples to extract 2017/2021 CTPP data 
#     from Parts 1, 2 and 3, at different geographic levels
#   (grateful to assistance from Stacey Bricka (MacroSys) and Shichen Fang (FresnoCOG))
#  Examples developed by Chuck Purvis, Hayward, California
#    -- August 13, 2025 --
#######################################################################################

# 1. Load relevant libraries
library(jsonlite)
library(httr)

# 2. Read in my CTPP API key from my R environment
# Edit your R profile
#  usethis::edit_r_profile()

# Add the Sys.setenv() line to your R profile or create a .Renviron file
# and add the environment variable setting there.

# Sys.setenv(CTPP_KEY = "mytopsecretapikeyxxxxxxx")
# api_key <- Sys.getenv("CTPP_KEY")

api_key <- Sys.getenv("CTPP_KEY")

######################################################################################
#3. Define API Endpoint: Specify the URL of the API endpoint you want to access
#   In this set of examples, use the 2017-2021 CTPP data
url <- "https://ctppdata.transportation.org/api/data/2021"

#4. Set Headers: Create a list of headers required for the API request, including the API key.
headers <- c(Accept = "application/json", "x-api-key" = api_key)
##############################################################################################
#5. Prepare Parameters: Define the parameters needed for your API call. 
#     This may include specifying the type of data you want (like geographic IDs)
#6. Make the Request: Use the httr package function GET to request data from  the CTPP API.
##################################################################################################
# Example 1. Part 1. Table B101109. Population by School Enrollment (7) by selected census tracts
query01 <- list('get'='group(B101109)',
           'in'='state:06',                                   # California
           'in'='county:001,013,041,055,075,081,085,095,097', # 9 Bay Area Counties
           'for'='tract:*',                                   # All Census Tracts
           'size'=2000,'page'=1)                              # limit to the first 2000 tracts
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query01) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe01 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 2. Part 1. Table B102106. Workers by Means of Transport (7) by all Counties in California
query02 <- list('get'='group(B102106)',
                'in'='state:06',                                   # California
                'for'='county:*', # all 58 counties in California, note use of "for" and not "in"
                'size'=100,'page'=1)                              # limit to the first 100 counties
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query02) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe02 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 3. Part 1. Table B112211. Households by Household Size (5) by Vehicles Avaialble (6)
#            by all PUMAs in California
query03 <- list('get'='group(B112211)',
                'in'='state:06',                                   # California
                'for'='public use microdata area:*',               # all 265 PUMAs in California
                'size'=500,'page'=1)                               # limit to the first 500 PUMAs
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query03) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe03 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 4. Part 1. Table B112100. Households (1), 
#                          B112106. Households by Household Size (5)
#                          B112107. Household Population  (1)
#                          B112108. Mean Household Size (1)
#                    by all places in California
query04 <- list('get'='group(B112100),group(B112106),group(B112107),group(B112108)',
                'in'='state:06',                                   # California
                'for'='place:*',                                   # all 1,611 Places in California
                'size'=2000,'page'=1)                              # limit to the first 2000 places
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query04) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe04 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 5. Part 2. Table B202104. Workers at workplace by Industry (15) 
#                    by all counties in California
query05 <- list('get'='group(B202104)',
                'in'='state:06',                                   # California
                'for'='county:*',                                   # all 58 counties in California
                'size'=100,'page'=1)                              # limit to the first 100 counties
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query05) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe05 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 6. Part 2. Table B202104. Workers at workplace by Industry (15) 
#                    by selected counties in AZ + CA using "geo" instead of in/for combos.
#    This approach would be more suitable for multi-state metropolitan planning organizations.
query06 <- list('get'='group(B202104)',
                "geo" ="C2300US06001,C2300US06005,C2300US06019,C2300US04013",  # Selected counties
                'size'=100,'page'=1)                              # limit to the first 100 counties
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query06) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe06 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 7. Part 2. Table B204200. Workers at workplace by Vehicles Available (6) by
#                                   Poverty Status (5) for
#                    Metropolitan Statistical Areas in USA (CAN'T SELECT BY STATE!!!)
query07 <- list('get'='group(B204200)',
              #  'in'='state:06',                                   # California
                'for'='metropolitan statistical area:*',          # all 392 MSAs in USA
                'size'=5000,'page'=1)                              # limit to the first 5000 MSAs
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query07) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe07 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 8. Part 3. Table B302100. Flow of Total Workers (1) by
#     Selected Tracts of Residence to Selected Tracts of Work, in California (Bay Area intraregional)
#   upwards of 1772 * 1772 = 3,139,000 records!!!!

# This partially works: All tract-of-residence in Alameda to all tract-of-work in Califorina
#   n=26,507 tract-to-tract flow
#  the size=50000 appears to be capped by the software at something less than 500,000????
#   n=44,930 tract-to-tract flows: Alameda, Contra Costa plus Marin to ALL of California.
query08 <- list('get'='group(B302100)',
                'in'='state:06',                                   # California
             #   'in'='county:001', # 1 county (Alameda)
              'in'='county:001,013,041', # 3 counties: Ala, CC, Marin
               # 'in'='county:001,013,041,055,075,081,085,095,097', # 9 Bay Area Counties
                'for'='tract:*',                                   # All Census Tracts of RESIDENCE
                'd-in'='state:06',                                   # California
            #   'd-in'='county:001', # 9 Bay Area Counties
            #    'd-in'='county:001,013,041,055,075,081,085,095,097', # 9 Bay Area Counties
                'd-for'='tract:*',                           # All Census Tracts of WORKPLACE
                'size'=50000,'page'=1)             # limit to the first 50,000 tract-2-tract records
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query08) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe08 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 9. Part 3. Table B302103. Flow of Workers by Means of Transportation (18) by
#     county-to-county flows within California (n=1,499 of 58*58 possible records)
query09 <- list('get'='group(B302103)',
                'in'='state:06',                          # California
                'for'='county:*',                         # All California Counties of RESIDENCE
                'd-in'='state:06',                        # California workplace
                'd-for'='county:*',                       # All California Counties of WORKPLACE
                'size'=5000,'page'=1)    # limit to the first 5,000 county-to-county records
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query09) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe09 <-r$data      # convert from list to data frame.
#############################################################################################
# Example 10. Part 3. Table B303100. Flow of Workers by Household Income Level (9) by
#     place-to-county flows within California (n=9,307 records)
query10 <- list('get'='group(B303100)',
                'in'='state:06',                          # California
                'for'='place:*',                         # All California Places of RESIDENCE
                'd-in'='state:06',                        # California workplace
                'd-for'='county:*',                       # All California Counties of WORKPLACE
                'size'=50000,'page'=1)    # limit to the first 50,000 place-to-county records
x <- httr::GET(url, add_headers('x-api-key' = api_key),query=query10) # httr function to call CTPP API
c <- rawToChar(x$content)  # base R function to convert object to character....
r <- jsonlite::fromJSON(c) # jsonlite function to convert from JSON format to large list
dataframe10 <-r$data      # convert from list to data frame.
#############################################################################################
### get the variable labels
url_group <- "https://ctppdata.transportation.org/api/groups/b303100/variables"
params <- list(year="2021")
response <- GET(url_group, add_headers(.headers = headers), query = params)
zzz <- fromJSON(rawToChar(response$content))
print(zzz$data)
label_b303100 <- zzz$data
############################################################################################
#  Not too sure how to proceed. The dataframe label_b303100 can be used somehow, manually,
#   to create mnemonic variable names, probably by laboriously renaming all 18 variables
#   in this dataframe.
# I also have code to change "+/-" to "" but this would be necessary if computing
#  standard errors from these 90 percent MOEs.
###########################################################################################
