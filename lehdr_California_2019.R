####################################################################
##  lehdr_California_2019.r
##
##  Explore the Residence Area Characteristics (RAC) and
##             Work Area Characteristics (WAC) files
##
##  California, statewide, various geographic levels, 2019
##   (geographies would be from the 2010 Census!)
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
##        -- January 10, 2023 --
####################################################################
install.packages("lehdr")
install.packages("mapview")
install.packages("tmap")
install.packages("shinyjs")

library(lehdr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(mapview)
library(dplyr)
library(ggplot2)
library(tmap)
library(leaflet)
####################################################################################
#  County-of-Residence Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
rac1 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "rac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'county')  # County-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  County-of-Work Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
wac1 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "wac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'county')  # County-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Tract-of-Residence Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
rac2 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "rac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'tract')  # Tract-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Tract-of-Work Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
wac2 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "wac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'tract')  # Tract-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Block Group-of-Residence Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
rac3 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "rac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'bg')  # Block Group-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  Block Group-of-Work Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
wac3 <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "wac", 
                          use_cache=TRUE,
                          job_type = 'JT00',   # All Jobs
                          agg_geo = 'bg')  # Block Group-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  State-of-Residence Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
rac_state <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "rac", 
                               use_cache=TRUE,
                               job_type = 'JT00',   # All Jobs
                               agg_geo = 'state')  # State-of-Residence
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
####################################################################################
#  State-of-Work Total Workers, California State
start_timer <- Sys.time() # start stopwatch on this function
wac_state <- lehdr::grab_lodes(state = 'ca', year = 2019, lodes_type = "wac", 
                               use_cache=TRUE,
                               job_type = 'JT00',   # All Jobs
                               agg_geo = 'state')  # State-of-Work
end_timer <- Sys.time() # stop stopwatch on this function
end_timer - start_timer # report in the console the "elapsed time" (Time difference)
################################################################################
#### Bring in TIGER files using TIGRIS!

# Cache management. Default is TRUE, to caches to SHP files in an R session.

options(tigris_use_cache = TRUE)

# options(tigris_refresh = TRUE)

# Current (March 2021) version of TIGRIS defaults to 2019.
# Well, I think the new default is 2020, but it's probably a good habit
#  to be explicit in your TIGRIS call to use year=2020 for Census 2020
#  and probably year=2019 for Census 2010 and ACS data through 2019!!!

options(tigris_year=2020)

######

geo_BlkGrps1 <- tigris::block_groups(state = "CA", cb = TRUE, year=2019) %>% 
                dplyr::mutate(w_bg = GEOID,
                              landarea_sqmi = ALAND / 2589988.1) 

mapview(geo_BlkGrps1)

CA_blkgrp_work_2019 <- dplyr::left_join(geo_BlkGrps1,wac3,by="w_bg") %>%
                       replace(is.na(.),0) %>%  # replace all NA with 0
                       dplyr::mutate(jobs_per_sqmi = C000/landarea_sqmi) %>% 
                       replace(is.na(.),0) %>%  # replace all NA with 0
                       dplyr::mutate(density_grp = case_when(
                              jobs_per_sqmi < 500 ~    "1.rural",
                              jobs_per_sqmi < 1000 ~   "2.rural-suburb",
                              jobs_per_sqmi < 6000 ~   "3.suburb-disperse",
                              jobs_per_sqmi < 10000 ~  "4.suburb-dense",
                              jobs_per_sqmi < 20000 ~  "5.urban",
                              jobs_per_sqmi >= 20000 ~ "6.urban core",
                              TRUE ~ "0.missing data")) %>% 
                    dplyr::relocate(jobs_per_sqmi:density_grp, .after=landarea_sqmi) 

# Calculate Statewide Summaries of Workers-at-Work, Total Land Area, by Density Group
summary1 <- CA_blkgrp_work_2019 %>% 
            sf::st_drop_geometry() %>% 
            dplyr::group_by(density_grp) %>% 
            dplyr::summarize(n_blkgrp = n(),
                             total_jobs = sum(C000),
                             landarea_sqmi = sum(landarea_sqmi)) %>% 
            dplyr::bind_rows(summarize(., across(where(is.numeric), sum),
                           across(where(is.character), ~'Total'))) %>% 
            dplyr::mutate(jobs_per_sqmi = total_jobs / landarea_sqmi) 

# Calculate Summaries by County by Density Group

summary2 <- CA_blkgrp_work_2019 %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(COUNTYFP,density_grp) %>% 
  dplyr::summarize(n_blkgrp = n(),
                   total_jobs = sum(C000),
                   landarea_sqmi = sum(landarea_sqmi)) %>% 
  dplyr::bind_rows(summarize(., across(where(is.numeric), sum),
                             across(where(is.character), ~'Total'))) %>% 
  dplyr::mutate(jobs_per_sqmi = total_jobs / landarea_sqmi) 

# Pivot wider... one row per county!
summary2a <- summary2 %>% 
  tidyr::pivot_wider(id_cols="COUNTYFP",
                     names_from  = "density_grp",
                     values_from = "n_blkgrp") %>% 
         replace(is.na(.),0)  # replace all NA with 0
  
summary2b <- summary2 %>% 
  tidyr::pivot_wider(id_cols="COUNTYFP",
                     names_from  = "density_grp",
                     values_from = "total_jobs") %>% 
  replace(is.na(.),0)  # replace all NA with 0

summary2c <- summary2 %>% 
  tidyr::pivot_wider(id_cols="COUNTYFP",
                     names_from  = "density_grp",
                     values_from = "landarea_sqmi") %>% 
  replace(is.na(.),0)  # replace all NA with 0

# Write out these summary files into CSV format for further analysis....

setwd("~/Desktop/tidycensus_work/output")

write.csv(summary1,   "lehd_California_BlkGrp_JobDensity_Groups_2019.csv")
write.csv(summary2a,  "lehd_California_BlkGrp_JobDensity_nblkgrps_county_2019.csv")
write.csv(summary2b,  "lehd_California_BlkGrp_JobDensity_totjobs_county_2019.csv")
write.csv(summary2c,  "lehd_California_BlkGrp_JobDensity_landarea_county_2019.csv")

# The following use case_when might be appropriate for integer data, say, microdata        
## dplyr::mutate(density_grp = case_when(
## jobs_per_sqmi 0:499.999       ~ "1.rural",
## jobs_per_sqmi 500:999.999     ~ "2.rural-suburb",
## jobs_per_sqmi 1000:5999.999   ~ "3.suburb-disperse",
## jobs_per_sqmi 6000:9999.999   ~ "4.suburb-dense",
## jobs_per_sqmi 10000:19999.999 ~ "5.urban",
## jobs_per_sqmi >= 20000        ~ "6.urban core",
##  TRUE ~ "missing value")) %>%

###########################################################################
### Let's try mapping!!! :) 

plot(CA_blkgrp_work_2019["density_grp"])

mapview(CA_blkgrp_work_2019, zcol="density_grp")

ggplot(data=CA_blkgrp_work_2019,
       aes(fill=density_grp)) + 
       geom_sf()

library(shiny)
library(shinyjs)
tmaptools::palette_explorer()

tmap_mode("view")

tm_shape(CA_blkgrp_work_2019) +
  tm_fill(col="density_grp", palette="YlOrRd")

#############################################################
### What are the Block Groups with the Highest Job Density in California?
### slice off top 25 block groups, and select (save) selected variables

blockgroupx <- CA_blkgrp_work_2019 %>% 
    sf::st_drop_geometry() %>% # Drop this command if you want to map the BGs!
    dplyr::arrange(desc(jobs_per_sqmi)) %>% 
    dplyr::slice(1:25) %>% 
    dplyr::select(c(COUNTYFP,TRACTCE,BLKGRPCE,GEOID,
                    landarea_sqmi,jobs_per_sqmi,C000))

# mapview(blockgroupx, zcol="jobs_per_sqmi")

write.csv(blockgroupx,   "lehd_California_BlkGrp_Top25_JobDensity_2019.csv")
