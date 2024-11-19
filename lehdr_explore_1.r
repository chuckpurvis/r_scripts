####################################################################
##  lehdr_explore_1.r
##  Explore the New (2/4/22) lehdr package.
##  This package grabs and aggregates LEHD/LODES datasets
##
##  Use purrr package to iterate on multiple years!
##  Use dplyr package to sort dataframe and re-order columns
##  
##  Now I know my ABCs....
##  LED   = Local Employment Dynamics
##  LEHD  = Longitudinal Employer Household Dynamics Program
##  LODES = LEHD Origin-Destination Employment Statistics
##  UI    = Unemployment Insurance
##  QCEW  = Quarterly Census of Employment and Wages
##  RAC   = Residence Area Characteristics
##  WAC   = Workplace Area Characteristics
##  CES   = Center for Economic Studies, US Bureau of the Census
##
##  job_type: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs, 
##    "JT03" for Private Primary jobs, "JT04" for All Federal jobs, 
##    "JT05" for Federal Primary jobs
##  JT01 is most similar to ACS/CTPP "main job"...perhaps??
##
##  Segment of the workforce: variable is "segment"
##     "S000" total number of jobs for workers, 
##     "SA01" number of jobs for workers aged 29 or younger, 
##     "SA02" number of jobs for workers aged 30-54,
##     "SA03" number of jobs for workers 55 and older, 
##     "SE01" number of jobs with earnings $1,250/month or less, 
##     "SE02" number of jobs with earnings $1,251 to $3,333/month,
##     "SE03" number of jobs with earnings greater than $3,333/month, 
##     "SI01" number of jobs in Goods Producing industry sectors, 
##     "SI02" number of jobs in Trade, Transportation, and Utilities industry sectors, 
##     "SI03" number of jobs in All Other Services industry sectors
##  
##   agg_geo: Aggregate to a geography other than Census Block (default). 
##      Values can be "bg" for block group, "tract", "county", or "state".
##
##        -- February, 24, 2022 --
##        -- March 21, 2022 -- Add iterations by jobtype!!!
####################################################################
install.packages("lehdr")

library(lehdr)
library(tidyverse)
library(purrr) # purrr is actually loaded in the library(tidyverse) command
library(dplyr)

# Provided Examples in lehdr documentation....

# download and load 2014 block level O-D data for Oregon
blk_df_or_od <- grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01",                               segment = "SA01", state_part = "main")

# download and load 2014 O-D data for Oregon and aggregate
# to the tract level
trt_df_or_od <- grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01",                               segment = "SA01", state_part = "main", agg_geo = "tract")

# download and load 2014 RAC data for Oregon and aggregate
# to the tract level
trt_df_or_rac <- grab_lodes(state = 'or', year = 2014, lodes_type = "rac", job_type = "JT01",                              segment = "SA01", agg_geo = "tract")

# download and load 2014 WAC data for Oregon and aggregate
# to the tract level
trt_df_or_wac <- grab_lodes(state = 'or', year = 2014, lodes_type = "wac", job_type = "JT01",                              segment = "SA01", agg_geo = "tract")
### END OF EXAMPLES FROM DOCUMENTATION ###########################################
#################################################################################
# This pulls in block-to-block data for CALIFORNIA and then summarizes the LODES
#   data at county-to-county level. 
# returns 3,165 records... Potential of 58 * 58 = 3,364 intrastate records.

# This takes minutes... add a stopwatch to calculate elapsed time.
# This took 3.95 minutes to run... Probably use a smaller state for "testing"

start_timer <- Sys.time() # start stopwatch on this function
temp1 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT00',   # All Jobs
                           segment  = 'S000',   # All Workers, no segmentation
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

####################################################################################
#  County-to-County Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
temp2 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT00',   # All Jobs
                           segment  = 'S000',   # All Workers, no segmentation
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County Main/Primary Jobs, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
temp3 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           segment  = 'S000',   # All Workers, no segmentation
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County Primary Jobs, Older Workers Age 55+, Nevada State

#  OK, this is weird. Even though I asked for "SA03" the function returned all
#  10 segmentation categories.....

start_timer <- Sys.time() # start stopwatch on this function
temp4 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           segment  = 'SA03',   # Older Workers, Age 55+
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County Primary Jobs, Nevada State

#  I commented out the segment=, but still got all 10 employment segment categories
# 
start_timer <- Sys.time() # start stopwatch on this function
temp5 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                       #    segment  = 'SA03',   # Older Workers, Age 55+
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County Primary Jobs, Nevada State INTERSTATE commuters.....
#   Interstate Commuters with workplace in the State of Interest (Nevada)
#   Does *NOT* have outbound commuters, eg, Nevadans working in California.

# This indeed works.. biggest flow is from Mohave County, AZ to Clark County, NV

start_timer <- Sys.time() # start stopwatch on this function
temp6 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           #    segment  = 'SA03',   # Older Workers, Age 55+
                           state_part = 'aux', # INTERSTATE, only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County Primary Jobs, Nevada State -- 2002....

start_timer <- Sys.time() # start stopwatch on this function
temp7 <- lehdr::grab_lodes(state = 'nv', year = 2002, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-to-County TOTAL PRIVATE Jobs, Nevada State -- 2002....

start_timer <- Sys.time() # start stopwatch on this function
temp8 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT02',   # Primary Jobs
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'county')  # County-to-County
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################

# County-to-County Primary Jobs, Nevada State 2002-2005

#  WOW, this works!!! purrr is incredible....
#  full test will be all years: 2002-2019.... not sure what'll happen!!!!!!

years <- 2002:2005
names(years) <- years

start_timer <- Sys.time() # start stopwatch on this function
multiyr1 <- purrr::map_dfr(years, ~{ lehdr::grab_lodes(state = 'nv', year = .x, 
                                      lodes_type = "od", use_cache=TRUE,
                                      job_type = 'JT01',   # Primary Jobs
                                      state_part = 'main', # Intra-State, Only
                                      agg_geo = 'county')}, 
                                  .id = "year")
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

asort <- multiyr1 %>% 
  dplyr::arrange(h_county,w_county,year) %>% 
  dplyr::relocate(h_county,.before=w_county)  %>% # rearrange columns
  dplyr::relocate(year,.after=w_county)
  
###  End of Day #1 in exploring lehdr ###############################################
#####################################################################################

# County-to-County Jobs by Job Type, Nevada State 2019
# Iterate on each of the six job types:
##  job_type: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs, 
##    "JT03" for Private Primary jobs, "JT04" for All Federal jobs, 
##    "JT05" for Federal Primary jobs

jobtypes <- c("JT00","JT01","JT02","JT03","JT04","JT05")
names(jobtypes) <- jobtypes

start_timer <- Sys.time() # start stopwatch on this function
multiyr2 <- purrr::map_dfr(jobtypes, ~{ lehdr::grab_lodes(state = 'nv', year = 2019, 
                                                       lodes_type = "od", use_cache=TRUE,
                                                       job_type = .x,   # Iterate by Job Type
                                                       state_part = 'main', # Intra-State, Only
                                                       agg_geo = 'county')}, # county-to-county
                                                       .id = "jobtype6")
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

asort2 <- multiyr2 %>% 
  dplyr::arrange(h_county,w_county,jobtype6) %>% 
  dplyr::relocate(h_county,.before=w_county)  %>% # rearrange columns
  dplyr::relocate(jobtype6,.after=w_county)
#####################################################################################
#  Tract-to-Tract Primary Jobs, Nevada State -- 2019....
#  N=155,896 tract pairs for Nevada!

start_timer <- Sys.time() # start stopwatch on this function
temp9 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'tract')  # tract-to-tract
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Block Group to BLock Group Primary Jobs, Nevada State -- 2019....
#  N=348,878  block group pairs for Nevada!

start_timer <- Sys.time() # start stopwatch on this function
temp10 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                           job_type = 'JT01',   # Primary Jobs
                           state_part = 'main', # Intra-State, Only
                           agg_geo = 'bg')  # block group-to-block group
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  STATE-to-STATE Total Primary Jobs, Nevada State -- 2019....
#   N=1 ..... just the one intrastate record!

start_timer <- Sys.time() # start stopwatch on this function
temp11 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "od", use_cache=TRUE,
                            job_type = 'JT01',   # Primary Jobs
                            state_part = 'main', # Intra-State, Only
                            agg_geo = 'state')  # just the state grand total
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################