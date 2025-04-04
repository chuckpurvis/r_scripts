############################################################
#  CTPP_California_Tracts_1.r
#  Continuing with the Westat R Package "CTPPr"
#    CTPPr is authored by: Anthony Fucci, Alexander Cates 
#      and Marcelo Simas of Westat
#  Note that there isn't an option to select only certain
#    tracts, places, TADs, TAZs, WITHIN a state... only the
#    ENTIRE state!  User may prefer to use the Beyond2020
#    software for extracting only some parts within a state!
# 
#   Developed by Chuck Purvis, Hayward, California
#    -- August 3, 2021 -- 
###########################################################

# Install CTPPr onto local computer.. Just need to do once!
# I'm just not sure about etiquette for updating packages!
install.packages('devtools')



# Activate/load the CTPPr library for this script.
library("CTPPr")

# optional libraries to load, depending on how I expand the examples!
library('dplyr')
library("tidyverse")
library("magrittr")

# Produce a VIEWER table of ALL CTPP Tables
#  It defaults to the CTPP 2012-16 data. Not sure about 2006-10....
ctpp_tables()


# Table A101100: Total Population
# Tracts within California, Split FIPS Code

temp1a  <- download_ctpp(A101100,dataset='2016',
                        geography='Tract',
                        state="California",
                        output = 'Split FIPS Code')
temp1 <- temp1 %>% # Rename the Estimate and Standard Error variables
  rename(totalpop=Estimate,
         totalpop_se=SE)

# Create Variables for State, County, Tract from the Split FIPS Code
temp1$state  <- substr(temp1$RESIDENCE,1,2)
temp1$county <- substr(temp1$RESIDENCE,6,8)
temp1$tract <- substr(temp1$RESIDENCE,12,17)
temp1$GEOID <- paste(temp1$state,temp1$county,temp1$tract,sep="")

# Table A102101: Total Workers (residence)
# Tracts within California, Split FIPS Code

temp2  <- download_ctpp(A102101,dataset='2016',
                        geography='Tract',
                        state="California",
                        output = 'Split FIPS Code')

temp2 <- temp2 %>% # Rename the Estimate and Standard Error variables
  rename(totalworker=Estimate,
         totalworker_se=SE)

# Create Variables for State, County, Tract from the Split FIPS Code
temp2$state  <- substr(temp2$RESIDENCE,1,2)
temp2$county <- substr(temp2$RESIDENCE,6,8)
temp2$tract <- substr(temp2$RESIDENCE,12,17)
temp2$GEOID <- paste(temp2$state,temp2$county,temp2$tract,sep="")

# Table A202100: Total Workers (workplace)
# Tracts within California, Split FIPS Code

temp3  <- download_ctpp(A202100,dataset='2016',
                        geography='Tract',
                        state="California",
                        output = 'Split FIPS Code')

temp3 <- temp3 %>% # Rename the Estimate and Standard Error variables
  rename(totalworker_atwork=Estimate,
         totalworker_atwork_se=SE)

# Create Variables for State, County, Tract from the Split FIPS Code
temp3$state  <- substr(temp3$WORKPLACE,1,2)
temp3$county <- substr(temp3$WORKPLACE,6,8)
temp3$tract <- substr(temp3$WORKPLACE,12,17)
temp3$GEOID <- paste(temp3$state,temp3$county,temp3$tract,sep="")

#######################################################################
# Compare the "cbind" vs "merge" function. 
#  The merge function creates a more streamline dataframe. 

temp4 <- cbind(temp1,temp2)
temp5 <- cbind(temp4,temp3)

# cbind can work on two or more dataframes!
#  The problem here is the duplicated variables such as state, county, etc.
tempx <- cbind(temp1,temp2,temp3)

# merge these dataframes by GEOID, etc.
tempy <- merge(temp1,temp2,  by = c('GEOID','state','county','tract','RESIDENCE'))
tempz <- merge(tempy,temp3,  by = c('GEOID','state','county','tract'))

setwd("~/Desktop/tidycensus_work/output")

# Export the data frames to CSV files, for importing to Excel, and applying finishing touches
write.csv(tempz,"CTPP1216_California_tracts_set1.csv")
#######################################################################

# Table A102106: Workers by Detailed Means of Transportation (18)
# Tracts within California, Split FIPS Code

temp6  <- download_ctpp(A102106,dataset='2016',
                         geography='Tract',
                         state="California",
                         output = 'Split FIPS Code')

temp6 <- temp6 %>%
  rename(means18="Means of Transportation 18")

temp601 <- subset(temp6,means18=="Total, means of transportation")
temp601 <- temp601 %>%
  rename(total=Estimate,
                total_se=SE)

temp602 <- subset(temp6,means18=="Car, truck, or van -- Drove alone")
temp602 <- temp602 %>%
  rename(dralone=Estimate,
         dralone_se=SE)

temp603 <- subset(temp6,means18=="Car, truck, or van -- In a 2-person carpool")
temp603 <- temp603 %>%
  rename(carpool2=Estimate,
         carpool2_se=SE)

# Repeat for other Means of Transportation?????

temp01 = subset(temp601, select = -c(means18))
temp02 = subset(temp602, select = -c(means18))
temp03 = subset(temp603, select = -c(means18))

temp6xxx <- merge(temp01,temp02, by= c('RESIDENCE'))
temp6yyy <- merge(temp6xxx,temp03, by= c('RESIDENCE'))


# Create Variables for State, County, Tract from the Split FIPS Code
temp6$state  <- substr(temp6$RESIDENCE,1,2)
temp6$county <- substr(temp6$RESIDENCE,6,8)
temp6$tract <- substr(temp6$RESIDENCE,12,17)
temp6$GEOID <- paste(temp6$state,temp6$county,temp6$tract,sep="")

