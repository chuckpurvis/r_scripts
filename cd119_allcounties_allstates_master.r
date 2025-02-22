##########################################################################
#   cd119_allcounties_allstates_master.r
# Concatenated Counties-within_States API call from Census 2020, File cd119
##########################################################################
#  Create and Save an RDS file for further analysis
#   setwd("~/Desktop/cd119_file")
#   aaa_master <- readRDS("cd119_allstates_allcounties.rda")
#    - December 14, 2024 --
# New data for the 119th Congress(2025-2027) was released by the Census
#   Bureau on December 5, 2024.
# This includes adjusted congressional district boundaries for five states:
#    Alabama, Georgia, Louisiana, New York and North Carolina.
##########################################################################
library(jsonlite)
library(tidyverse)
library(tidycensus)

setwd("~/Desktop/cd118_file") # keep the old directory .....

state <- get_decennial(year=2020, sumfile="dhc",
                       geography="state", show_call=TRUE,output="wide",
                       variables = "P1_001N")
##############################################################################
# Alabama counties within congressional districts
temp1 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:01%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state01 <- as.data.frame(temp1)

# Alaska counties within congressional districts
temp2 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:02%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state02 <- as.data.frame(temp2)

# Arizona counties within congressional districts
temp4 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:04%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state04 <- as.data.frame(temp4)

# Arkansas counties within congressional districts
temp5 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:05%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state05 <- as.data.frame(temp5)

# California counties within congressional districts
temp6 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:06%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state06 <- as.data.frame(temp6)

# California counties within congressional districts
temp6 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:06%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state06 <- as.data.frame(temp6)

# Colorado counties within congressional districts
temp8 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:08%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state08 <- as.data.frame(temp8)

# Connecticut counties within congressional districts
temp9 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:09%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state09 <- as.data.frame(temp9)

# Delaware counties within congressional districts
temp10 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:10%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state10 <- as.data.frame(temp10)

# Florida counties within congressional districts
temp12 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:12%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state12 <- as.data.frame(temp12)

# Georgia counties within congressional districts
temp13 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:13%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state13 <- as.data.frame(temp13)

# Hawaii counties within congressional districts
temp15 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:15%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state15 <- as.data.frame(temp15)

# Idaho counties within congressional districts
temp16 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:16%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state16 <- as.data.frame(temp16)

# Illinois counties within congressional districts
temp17 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:17%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state17 <- as.data.frame(temp17)

# Indiana counties within congressional districts
temp18 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:18%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state18 <- as.data.frame(temp18)

# Iowa counties within congressional districts
temp19 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:19%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state19 <- as.data.frame(temp19)

# Kansas counties within congressional districts
temp20 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:20%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state20 <- as.data.frame(temp20)

# Kentucky counties within congressional districts
temp21 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:21%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state21 <- as.data.frame(temp21)

# Lousiana counties within congressional districts
temp22 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:22%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state22 <- as.data.frame(temp22)

# Maine counties within congressional districts
temp23 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:23%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state23 <- as.data.frame(temp23)

# Maryland counties within congressional districts
temp24 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:24%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state24 <- as.data.frame(temp24)

# Massachusetts counties within congressional districts
temp25 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:25%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state25 <- as.data.frame(temp25)

# Michigan counties within congressional districts
temp26 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:26%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state26 <- as.data.frame(temp26)

# Minnesota counties within congressional districts
temp27 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:27%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state27 <- as.data.frame(temp27)

# Mississippi counties within congressional districts
temp28 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:28%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state28 <- as.data.frame(temp28)

# Missouri counties within congressional districts
temp29 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:29%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state29 <- as.data.frame(temp29)

# Montana counties within congressional districts
temp30 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:30%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state30 <- as.data.frame(temp30)

# Nebraska counties within congressional districts
temp31 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:31%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state31 <- as.data.frame(temp31)

# Nevada counties within congressional districts
temp32 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:32%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state32 <- as.data.frame(temp32)

# New Hampshire counties within congressional districts
temp33 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:33%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state33 <- as.data.frame(temp33)

# New Jersey counties within congressional districts
temp34 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:34%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state34 <- as.data.frame(temp34)

# New Mexico counties within congressional districts
temp35 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:35%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state35 <- as.data.frame(temp35)

# New York counties within congressional districts
temp36 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:36%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state36 <- as.data.frame(temp36)

# North Carolina counties within congressional districts
temp37 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:37%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state37 <- as.data.frame(temp37)

# North Dakota counties within congressional districts
temp38 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:38%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state38 <- as.data.frame(temp38)

# Ohio counties within congressional districts
temp39 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:39%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state39 <- as.data.frame(temp39)

# Oklahoma counties within congressional districts
temp40 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:40%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state40 <- as.data.frame(temp40)

# Oregon counties within congressional districts
temp41 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:41%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state41 <- as.data.frame(temp41)

# Pennsylvania counties within congressional districts
temp42 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:42%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state42 <- as.data.frame(temp42)

# Rhode Island counties within congressional districts
temp44 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:44%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state44 <- as.data.frame(temp44)

# South Carolina counties within congressional districts
temp45 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:45%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state45 <- as.data.frame(temp45)

# South Dakota counties within congressional districts
temp46 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:46%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state46 <- as.data.frame(temp46)

# Tennessee counties within congressional districts
temp47 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:47%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state47 <- as.data.frame(temp47)

# Texas counties within congressional districts
temp48 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:48%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state48 <- as.data.frame(temp48)

# Utah counties within congressional districts
temp49 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:49%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state49 <- as.data.frame(temp49)

# Vermont counties within congressional districts
temp50 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:50%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state50 <- as.data.frame(temp50)

# Virginia counties within congressional districts
temp51 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:51%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state51 <- as.data.frame(temp51)

# Washington counties within congressional districts
temp53 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:53%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state53 <- as.data.frame(temp53)

# West Virginia counties within congressional districts
temp54 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:54%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state54 <- as.data.frame(temp54)

# Wisconsin counties within congressional districts
temp55 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:55%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state55 <- as.data.frame(temp55)

# Wyoming counties within congressional districts
temp56 <- fromJSON("https://api.census.gov/data/2020/dec/cd119?get=NAME,P1_001N,P10_001N,P15_001N,P16_001N,H1_001N,H12_002N,H12_010N,P5_010N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N&for=county%20(or%20part):*&in=state:56%20congressional%20district:00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52")
state56 <- as.data.frame(temp56)

######################################################################################
##  Stack the state-place files into a master file, and rename variables
######################################################################################

aaastack1 <- bind_rows(state01,state02,state04,state05,state06,state08,
                         state09,state10,state12,state13,state15,state16,
                         state17,state18,state19,state20,state21,state22,
                         state23,state24,state25,state26,state27,state28,
                         state29,state30,state31,state32,state33,state34,
                         state35,state36,state37,state38,state39,state40,
                         state41,state42,state44,state45,state46,state47,
                         state48,state49,state50,state51,state53,state54,
                         state55,state56)

aaastack2 <- aaastack1 %>% 
  filter(!V1=="NAME") %>% 
  rename(county_cd_name = V1,
         totpop_2020 = V2,
         votingage   = V3,
         hhpop       = V4,
         tothh       = V5,
         totdu       = V6,
         owner_hh    = V7,
         renter_hh   = V8,
         hispanic    = V9,
         nh_white    = V10,
         nh_black    = V11,
         nh_aian     = V12,
         nh_asian    = V13,
         nh_nhopi    = V14,
         nh_other    = V15,
         nh_multi    = V16,
         state_fips  = V17,
         cong_dist   = V18,
         county_fips = V19) %>% 
  mutate_at(c("totpop_2020","votingage","hhpop","tothh","totdu","owner_hh",
              "renter_hh","hispanic","nh_white","nh_black","nh_aian","nh_asian",
              "nh_nhopi","nh_other","nh_multi"), as.numeric) %>% 
  unite(GEOID,c("state_fips","cong_dist"),sep="", remove=FALSE) %>%
  unite(stateco_fips,c("state_fips","county_fips"),sep="", remove=FALSE) %>%
  separate_wider_delim(county_cd_name,delim=",",
                       names=c("countyname","congdistname","statename"), too_many="drop",
                       cols_remove=FALSE) %>%
  mutate(countyname2 = countyname) %>% 
  mutate(across("countyname2",str_replace," County","")) %>% 
  arrange(GEOID,stateco_fips) %>% 
  relocate(GEOID:county_fips, .before=county_cd_name) %>% 
  relocate(countyname2, .after=countyname)

##################################################################################
#  Save this master file in RDS format
##################################################################################

setwd("~/Desktop/cd118_file")

saveRDS(aaastack2,"cd119_allstates_allcounties.rda")
aaa_master <- readRDS("cd119_allstates_allcounties.rda")

#### End of Script ##############################################################
