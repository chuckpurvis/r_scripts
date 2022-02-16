######################################################
#  tidycensus_Example0.r
# Example #0 
# Installing and Loading Relevant R Packages, 
# Checking the Variable Lists in Various Datasets
#  Prepared by Chuck Purvis, Hayward, California
######################################################

# Step 1 Install R packages. If installed in previous sessions, there is no need to re-install.
# You may need to install the packages "tidyr" and "sp" for "tidycensus" to be properly installed.

install.packages("tidyverse")
install.packages("tidycensus")
install.packages("janitor")

# Step 2: Load relevant libraries into each R-session.

library(tidyverse)
library(tidycensus)
library(janitor)

# Step 3: Load the User's Census API Key.
# Census API Key was installed in previous sessions, so no need to re-install
# un-comment out the following statement with the user's API key.
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)

# Step 4: Explore the Data Variables using the load_variables() function
# Use the function load_variables() to view all of the possible variables for analysis
# load_variables works for both decennial census and American Community Survey databases

acs18_variable_list <- load_variables(year = 2018, dataset = "acs5", cache = TRUE)
acs18p_variable_list <- load_variables(year = 2018, dataset = "acs5/profile", cache = TRUE)

# 2010 Decennial Census, SF1 is available.
dec10sf1_variable_list <- load_variables(year = 2010, dataset = "sf1", cache = TRUE)
# There is no SF3 for the 2010 Decennial Census.
# dec10sf3_variable_list <- load_variables(year = 2010, dataset = "sf3", cache = TRUE)

# 2000 Census SF1 and SF3 apparently are pullable? 
dec00sf1_variable_list <- load_variables(year = 2000, dataset = "sf1", cache = TRUE)
dec00sf3_variable_list <- load_variables(year = 2000, dataset = "sf3", cache = TRUE)

# 1990 Census SF1 and SF3 apparently are pullable? 
dec90sf1_variable_list <- load_variables(year = 1990, dataset = "sf1", cache = TRUE)
dec90sf3_variable_list <- load_variables(year = 1990, dataset = "sf3", cache = TRUE)

# Maybe write out the data frame to the desktop, for easier in use in Excel?
write.csv(acs18_variable_list,'acs18_variable_list.csv', row.names=FALSE)

View(acs18_variable_list)

B09 <- filter(acs18_variable_list, str_detect(name, "B09"))
View(B09)
B06 <- filter(acs18_variable_list, str_detect(name, "B06"))
View(B06)

#---------------------------------------------------------------------------------------