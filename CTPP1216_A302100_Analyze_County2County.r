####################################################################
# CTPP1216_A302100_Analyze_County2County.r
#   Compare county-to-county total worker flows, using the
#    county-level summary level and the tract-to-tract summary level
#  CTPP 2012-16, Table #302100 (Total Workers at Work, Age 16+)
#
#       -- February 14, 2022 --
####################################################################
library(readr)
library(dplyr)

county1 <- read_csv("CTPP1216_A302100_Calif_County2County.csv")
View(county1)

tract1 <- read_csv("CTPP1216_A302100_BayArea_Tract2Tract.csv")
View(tract1)

# Use DPLYR to sort the California county-to-county commuter file
#   and then to filter rows with Bay Area counties of residence and work.
#   Then calculate statistics: regional sum, county-of-residence and 
#   county-of-work totals.

# Create a list of Bay area county FIPS codes....
bayco <- c(1,13,41,55,75,81,85,95,97)

baycounty1 <- dplyr::arrange(county1,County_Resid,County_Work) %>% # sort the DF
              dplyr::filter(County_Resid %in% bayco) %>%
              dplyr::filter(County_Work  %in% bayco)

# Summarize the total workers age 16+

sumstat1 <- baycounty1 %>% 
              dplyr::summarize(totalworker=sum(Workers_16p))

sumstat1a <- dplyr::summarize(baycounty1,totalworker=sum(Workers_16p))

resid_sum <- baycounty1 %>% 
             dplyr::group_by(County_Resid) %>% 
             dplyr::summarize(totalworker=sum(Workers_16p))

work_sum <- baycounty1 %>% 
            dplyr::group_by(County_Work) %>% 
            dplyr::summarize(totalworker=sum(Workers_16p))

###################################################################
# Summarize the Bay Area tract-to-tract file to county-to-county

summary(tract1,Workers_16p)

tract2 <- arrange(tract1,County_Resid,County_Work) %>% 
          filter(!is.na(Workers_16p)) # this is needed... a few NA records....

sumstat2 <- tract2 %>% 
            dplyr::summarize(totalworker=sum(Workers_16p))

baycounty2 <- tract2 %>% 
              dplyr::group_by(County_Resid,County_Work) %>% 
              dplyr::summarize(totalwork_tractsum = sum(Workers_16p))

#####################################################################
## Combine the county-to-county file: county sumlev and tract sumlev

baycounty3 <- dplyr::bind_cols(baycounty1,baycounty2) %>% 
             mutate(share_covered=totalwork_tractsum/Workers_16p,
                    corr_factor=Workers_16p/totalwork_tractsum)

baycounty3 <- arrange(baycounty3,RESIDENCE,WORKPLACE)

write_csv(baycounty3, "CTPP1216_A302100_BayArea_Analysis_County2County.csv")
