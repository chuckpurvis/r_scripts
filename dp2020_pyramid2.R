##########################################################
#  dp2020_pyramid2.r
#  Create an Age-Sex Pyramid using Census 2020, Data Profile (DP)
#  Example #2 : California Statewide
#   - July 17, 2023 -
#   - July 25, 2023 - 
#  Adapted from Kyle Walker's example in the book "Analyzing US Census Data"
##########################################################
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(scales) # this is needed for formatting the x-axis in the ggplot2

selvars <- c(
  agepop_m_0004  = "DP1_0026C",   # Male Population, Age 00-04
  agepop_m_0509  = "DP1_0027C",   # Male Population, Age 05-09
  agepop_m_1014  = "DP1_0028C",   # Male Population, Age 10-14
  agepop_m_1519  = "DP1_0029C",   # Male Population, Age 15-19
  agepop_m_2024  = "DP1_0030C",   # Male Population, Age 20-24
  agepop_m_2529  = "DP1_0031C",   # Male Population, Age 25-29
  agepop_m_3034  = "DP1_0032C",   # Male Population, Age 30-34
  agepop_m_3539  = "DP1_0033C",   # Male Population, Age 35-39
  agepop_m_4044  = "DP1_0034C",   # Male Population, Age 40-44
  agepop_m_4549  = "DP1_0035C",   # Male Population, Age 45-49
  agepop_m_5054  = "DP1_0036C",   # Male Population, Age 50-54
  agepop_m_5559  = "DP1_0037C",   # Male Population, Age 55-59
  agepop_m_6064  = "DP1_0038C",   # Male Population, Age 60-64
  agepop_m_6569  = "DP1_0039C",   # Male Population, Age 65-69
  agepop_m_7074  = "DP1_0040C",   # Male Population, Age 70-74
  agepop_m_7579  = "DP1_0041C",   # Male Population, Age 75-59
  agepop_m_8084  = "DP1_0042C",   # Male Population, Age 80-84
  agepop_m_8599  = "DP1_0043C",   # Male Population, Age 85-and-over
  agepop_f_0004  = "DP1_0050C",   # Female Population, Age 00-04
  agepop_f_0509  = "DP1_0051C",   # Female Population, Age 05-09
  agepop_f_1014  = "DP1_0052C",   # Female Population, Age 10-14
  agepop_f_1519  = "DP1_0053C",   # Female Population, Age 15-19
  agepop_f_2024  = "DP1_0054C",   # Female Population, Age 20-24
  agepop_f_2529  = "DP1_0055C",   # Female Population, Age 25-29
  agepop_f_3034  = "DP1_0056C",   # Female Population, Age 30-34
  agepop_f_3539  = "DP1_0057C",   # Female Population, Age 35-39
  agepop_f_4044  = "DP1_0058C",   # Female Population, Age 40-44
  agepop_f_4549  = "DP1_0059C",   # Female Population, Age 45-49
  agepop_f_5054  = "DP1_0060C",   # Female Population, Age 50-54
  agepop_f_5559  = "DP1_0061C",   # Female Population, Age 55-59
  agepop_f_6064  = "DP1_0062C",   # Female Population, Age 60-64
  agepop_f_6569  = "DP1_0063C",   # Female Population, Age 65-69
  agepop_f_7074  = "DP1_0064C",   # Female Population, Age 70-74
  agepop_f_7579  = "DP1_0065C",   # Female Population, Age 75-59
  agepop_f_8084  = "DP1_0066C",   # Female Population, Age 80-84
  agepop_f_8599  = "DP1_0067C")   # Female Population, Age 85-and-over

# Pull Census 2020 data using the tidycensus function get_decennial  
state1 <- get_decennial(year=2020,  sumfile="dp", 
                         geography = "state", state="CA", # county="Alameda",
                         show_call = TRUE,output="tidy", 
                         variables = selvars)

# clean up variables using the tidyverse (dplyr, tidyr)
state1a <- state1 %>% 
            separate(variable,c("agepop","sex","range"),"_") %>% 
            separate(range,c("from","to"),2) %>% 
            unite(agerange,from:to,sep=" to ") %>% 
            mutate(value = ifelse(sex == "m", -value, value)) %>%
            mutate(Sex = ifelse(sex=="m"," Male",  # the space before Male helps...
                         ifelse(sex=="f","Female","missing"))) %>% 
 #           mutate(agerange = ifelse(agerange=="00 to 04","less than 5",agerange)) %>% 
            mutate(agerange = ifelse(agerange=="85 to 99","85 and over",agerange)) 

# Create an age-sex pyramid using ggplot2  
state1_pyramid <- ggplot(state1a, aes(x = value, y = agerange, fill = Sex)) + 
       geom_col(width = 0.90, alpha = 0.8) +
       theme_minimal(base_family = "Verdana", base_size = 12) +
  
# note that the function number_format is from the R package "scales"....  
scale_x_continuous(
  labels = ~ scales::number_format(scale = .001, suffix = "k")(abs(.x))) +
                     
 #      scale_x_continuous(
 #         labels = ~ scales::number_format(scale = .001, suffix = "k")(abs(.x)),
 #                    limits = c(-75000,75000)) +
                #     limits = 75000 * c(-1,1)) + 

  scale_fill_manual(values = c("darkred", "navy")) + 
 theme(plot.caption = element_text(size = 10),
    legend.position = "bottom", legend.direction = "horizontal") +
  theme(plot.subtitle = element_text(size = 14,
    face = "bold"), plot.caption = element_text(face = "bold.italic"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(size = 15,
        face = "bold"), panel.background = element_rect(fill = "gray82")) +
  labs(x="Total Population, Census 2020",y="Age Cohorts", fill=NULL, 
    title    = "California Statewide",
    subtitle = "Age-Sex Population Pyramid based on Census 2020",
    caption  = "Data source: US Census Bureau Data Profile (DP) & tidycensus R package")

state1_pyramid

# Export the finished age-sex pyramid graphic object to a png file.
setwd("~/Desktop/tidycensus_work/output")

ggsave("census2020_agesex_pyramid_California_statewide.png", plot=last_plot(),
       dpi=300,width=8,height=8) 
#######################################################################################