# File Info ---------------------------------------------------------------
#### File Name:   2_gen_data_dictionary.R
#### Purpose:     Merge All Files
#### Created by:  CK
#### Created on:  1/26/2018
#### Modified by: CK
#### Modified on: 2/24/2020
#- - - - - - - - - - - - - - - - - - - -#

# 1. Load library ---------------------------------------------------------

rm(list=ls())
library("pacman")
p_load(dataMeta,knitr,kableExtra)
finaldf <- readRDS(paste0(here(),"/data/process/final_hh_env_df.RDS")) # in the process folder

# 2. Create data dictionary -----------------------------------------------

# Description of variables
var_desc <- c(
  "Household ID","Survey Time",
  "Total Number of People in Household Tested Using RDT", 
  "Total Number of People in Household Tested Positive Using RDT", 
  "Total Number of People in Household Tested Using PCR", 
  "Total Number of People in Household Tested Positive Using PCR", 
  "Total Number of People in Household Tested Using Microscopy", 
  "Total Number of People in Household Tested Positive Using Microscopy", 
  "Altitude in Meters", "Longitude","Latitude", 
  "Total number of Nets in the Household", "Total number of people in the Household sleeping under a net",
  "Total number of people according to Household Survey", 
  "Total number of people according to Household Survey, Net usage survey, and Total number of People in Household Tested Using RDT", 
  "Indicator: At least one net in Houeshold; 1=Household has at least one net, 0=Household doesn't have a net",
  "Indicator: At least 50% Net Coverage in Houeshold; 1=Household covers at least 50% people with net, 0=Household doesn't cover 50% people with net",
  "Proportion of People using Net in Household",
  "Proportion of People Tested Positive in Houeshold using RDT",
  "Proportion of People Tested Positive in Houeshold using Microscopy",
  "Proportion of People Sleeping Under a Net in Houeshold",
  "County Indicator:1=Kapujan, 2=Magoro, 3=Toroma",
  "Interview Date",
  "Average Time of Survey for Each Survey Time",
  "Date of MOD13Q1 Product Used to Extract EVI; MOD13Q1 is collected every 16 days",
  "Date of MOD11A2 Product Used to Extract Daytime Temperature; MOD11A2 is collected every 8 days",
  "EVI value closest to the date of interview (corresponding to evi_date_fin)",
  "Daytime temperature value (Kelvin) closest to the date of interview (corresponding to evi_date_fin)"
  )

var_type <- c(0,0,0,0,0,
              0,0,0,0,0,
              0,0,0,0,0,
              1,1,0,0,0,
              0,1,0,0,0,
              0,0,0)
              
linker <- build_linker(finaldf, variable_description = var_desc, variable_type = var_type)

dict <- build_dict(my.data = finaldf %>% data.frame, linker = linker, 
                   option_description = NULL, 
                   prompt_varopts = FALSE)

kable(dict, format = "html", caption = "Data dictionary for original dataset") %>%
  kable_styling() %>%
  save_kable(file = paste0(here(),"/data/process/uganda_datadict.html"), self_contained = T)

# Exporting dictionary only:
write.csv(dict, paste0(here(),"/data/process/uganda_datadict.csv"), row.names = FALSE)
