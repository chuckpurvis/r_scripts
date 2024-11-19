####################################################################
##  lehdr_explore_2.r
##
##  Stage 2: Explore the Residence Area Characteristics (RAC) and
##             Work Area Characteristics (WAC) files
## 
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
##  Job segmentation in the RAC and WAC files:
##    C000 = Total number of jobs
##    CA01 - CA03  = Jobs by Age of Worker
##    CE01 - CE03  = Jobs by Earnings of Worker
##    CNS01 - CNS20 = Jobs by NAICS industrial sector
##    CR01 - CR07  = Jobs by Race of Worker
##    CT01 - CT02  - Jobs by Hispanic/Latinx status of Worker
##    CD01 - CD04  - Jobs by Educational Attainment of Worker
##    CS01 - CS02  - Jobs by Sex of Worker
##    CFA01 - CFA05 - Jobs by Firm Age
##    CFS01 - CFS05 - Jobs by Firm Size
##  
##   agg_geo: Aggregate to a geography other than Census Block (default). 
##      Values can be "bg" for block group, "tract", "county", or "state".
##
##        -- March 21, 2022 --
####################################################################
install.packages("lehdr")

library(lehdr)
library(tidyverse)
library(purrr) # purrr is actually loaded in the library(tidyverse) command
library(dplyr)

####################################################################################
#  County-of-Residence Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
rac1 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "rac", use_cache=TRUE,
                           job_type = 'JT00',   # All Jobs
                           agg_geo = 'county')  # County-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-of-Work Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
wac1 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "wac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'county')  # County-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Tract-of-Residence Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
rac2 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "rac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'tract')  # Tract-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Tract-of-Work Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
wac2 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "wac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'tract')  # Tract-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Block Group-of-Residence Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
rac3 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "rac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'bg')  # Block Group-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Block Group-of-Work Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
wac3 <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "wac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'bg')  # Block Group-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  State-of-Residence Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
rac_state <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "rac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'state')  # State-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  State-of-Work Total Workers, Nevada State
start_timer <- Sys.time() # start stopwatch on this function
wac_state <- lehdr::grab_lodes(state = 'nv', year = 2019, lodes_type = "wac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'state')  # State-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
##  Test a pull for multiple years, County-of-Residence
##   This works great... took only 30 seconds

allyears <- c(2002:2019)

start_timer <- Sys.time() # start stopwatch on this function
rac1_allyears <- lehdr::grab_lodes(state = 'nv', year = allyears, lodes_type = "rac", use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'county')  # County-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

sorted1 <- rac1_allyears  %>% 
           arrange(h_county,year)
####################################################################################
##  Test a pull for multiple years, County-of-Work
##   This works great... took only 15 seconds

allyears <- c(2002:2019)

start_timer <- Sys.time() # start stopwatch on this function
wac1_allyears <- lehdr::grab_lodes(state = 'nv', year = allyears, lodes_type = "wac", use_cache=TRUE,
                                   job_type = 'JT00',   # All Jobs
                                   agg_geo = 'county')  # County-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

sorted2 <- wac1_allyears  %>% 
  arrange(w_county,year)
####################################################################################
##  Test a pull for multiple years and multiple states (New England), County-of-Work
##   This works great...

allyears <- c(2012:2019) # LODES wasn't available for some earlier year/state pair.....
newengland <- c("ma","me","nh","ri","vt")

start_timer <- Sys.time() # start stopwatch on this function
wac2_allyears <- lehdr::grab_lodes(state = newengland, year = allyears, lodes_type = "wac", use_cache=TRUE,
                                   job_type = 'JT00',   # All Jobs
                                   agg_geo = 'county')  # County-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)

sorted3 <- wac2_allyears  %>% 
  arrange(w_county,year)
####################################################################################

