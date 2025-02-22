######################################################################
#  calif_assembly_senate_2025_2027_scraping_wikipedia_1.r
#  Scrape relevant characteristics of California State Assembly, Senate Districts
#    2025-2027 term
#  https://www.r-bloggers.com/2021/07/politely-scraping-wikipedia-tables-2/
#       -- August 30, 2023 -- 
#       -- February 6, 2025 --
###################################################################################
#  install.packages("polite")
#  install.packages("toOrdinal")
library(tidyverse)
library(lubridate)
library(janitor)

library(rvest)
library(httr)
library(polite)

library(toOrdinal) # This R package has a function to convert cardinal to ordinal numbers

############################################################################

url <- "https://en.wikipedia.org/wiki/California_State_Assembly"
url_bow <- polite::bow(url)
url_bow

assembly_html <-
  polite::scrape(url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table(fill = TRUE) 

assembly_df <- 
  assembly_html[[3]] %>%  #inspection of assembly_html suggests the 3rd wikitable!
  clean_names()

assembly_df <- assembly_df %>% 
  select(-district) %>% 
  rename(district = district_2) %>% 
  mutate(partyx = case_when(party=="Democratic" ~ "(D)",
                            party=="Republican" ~ "(R)")) %>% 
  unite(nameparty,c("name","partyx"),sep=" ", remove=FALSE) %>%
  relocate(partyx, .after=party)

assembly_df$district_ordinal <- toOrdinal(assembly_df$district)
assembly_df$district_type <- "AD"
assembly_df$statefips <- "06"
assembly_df$dist_padded <- str_pad(assembly_df$district, 3, pad="0")
assembly_df <- assembly_df %>% 
   unite(dist_type,c("district_ordinal","district_type"),sep=" ", remove=FALSE) %>% 
   unite(namepartydist,c("nameparty","dist_type"), sep=", ", remove=FALSE) %>% 
   unite(namepartydisthome,c("namepartydist","residence"), sep=", ", remove=FALSE) %>% 
   unite(GEOID,c("statefips","dist_padded"), sep="", remove=FALSE) %>% 
   relocate(GEOID, .before=district)

##################################################################################

url2 <- "https://en.wikipedia.org/wiki/California_State_Senate"

url_bow <- polite::bow(url2)
url_bow

senate_html <-
  polite::scrape(url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table(fill = TRUE) 

senate_df <- 
  senate_html[[3]] %>%  #inspection of senate_html suggests the 3rd wikitable!
  clean_names()

senate_df <- senate_df %>% 
  select(-district) %>% 
  rename(district = district_2) %>% 
  mutate(partyx = case_when(party=="Democratic" ~ "(D)",
                            party=="Republican" ~ "(R)")) %>% 
  unite(nameparty,c("name","partyx"),sep=" ", remove=FALSE) %>%
  relocate(partyx, .after=party)

senate_df$district_ordinal <- toOrdinal(senate_df$district)
senate_df$district_type <- "SD"
senate_df$statefips <- "06"
senate_df$dist_padded <- str_pad(senate_df$district, 3, pad="0")
senate_df <- senate_df %>% 
  unite(dist_type,c("district_ordinal","district_type"),sep=" ", remove=FALSE) %>% 
  unite(namepartydist,c("nameparty","dist_type"), sep=", ", remove=FALSE) %>% 
  unite(namepartydisthome,c("namepartydist","residence"), sep=", ", remove=FALSE) %>% 
  unite(GEOID,c("statefips","dist_padded"), sep="", remove=FALSE) %>% 
  relocate(GEOID, .before=district)
#############################################################################
#  Let's output the final files to RDA format!!!
setwd("~/Desktop/Politics_Elections")

saveRDS(senate_df,  "wikipedia_calif_state_senate_feb2025.rda")
saveRDS(assembly_df,"wikipedia_calif_state_assembly_feb2025.rda")

senate_wiki <- readRDS("wikipedia_calif_state_senate_feb2025.rda")
assembly_wiki <-readRDS("wikipedia_calif_state_assembly_feb2025.rda")
##############################################################################
