# File Info ---------------------------------------------------------------
#### File Name:   3_data_check.R
#### Purpose:     Check final data
#### Created by:  CK
#### Created on:  1/26/2018
#### Modified by: CK
#### Modified on: 2/24/2020
#- - - - - - - - - - - - - - - - - - - -#

# 1. Load library ---------------------------------------------------------

rm(list=ls())
library("pacman")
p_load(dataMeta,knitr,kableExtra,skimr)
finaldf <- readRDS(paste0(here(),"/data/process/final_hh_env_df.RDS")) # in the process folder

# 2. Create data dictionary -----------------------------------------------

finaldf %>%
  mutate(
    county = as.factor(county),
    oneNet = as.factor(oneNet),
    Netptwo = as.factor(Netptwo)
  ) %>%
  skim 

# Print html
knitr::knit(paste0(here(),"/report/data_summary.Rmd"),
            paste0(here(),"/report/data_summary.html"))
