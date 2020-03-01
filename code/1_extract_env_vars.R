# File Info ---------------------------------------------------------------
#### File Name:   1_extract_env_vars.R
#### Purpose:     Merge All Files
#### Created by:  CK
#### Created on:  1/26/2018
#### Modified by: CK
#### Modified on: 2/24/2020
#- - - - - - - - - - - - - - - - - - - -#

# Check landsat products online: https://ladsweb.modaps.eosdis.nasa.gov/search/order/1/MOD11A1--6/2020-02-15..2020-02-29/DB/Tile:H21V8

# 1. Load Libs & Data -----------------------------------------------------

rm(list=ls())
library("pacman")
p_load(here,sp,rgdal,raster,gdalUtils,dplyr,tidyr,data.table,MODISTools,
       fuzzyjoin,furrr)
final_hh_df <- readRDS(paste0(here(),"/data/process/final_hh_df.RDS"))
source(paste0(here(),"/code/functions.R"))

# 2. Identify Dates from Final Dataframe -------------------------------------

unique(final_hh_df$time1) %>%
  summary


# 3. Extract EVI ----------------------------------------------------------

# So the problem is that the EVI dataset only has 16 days intervals of data... so we won't actually get the exact dates that we want... In this case we need to identify dates closest to the dataset of interest
seq_dates1 <- seq(as.Date("2015/1/1"), as.Date("2015/12/30"), by = "16 day")
seq_dates2 <- seq(as.Date("2016/1/1"), as.Date("2016/12/30"), by = "16 day")
seq_dates3 <- seq(as.Date("2017/1/1"), as.Date("2017/12/30"), by = "16 day")
seq_dates4 <- seq(as.Date("2018/1/1"), as.Date("2018/12/30"), by = "16 day")
seq_dates5 <- seq(as.Date("2019/1/1"), as.Date("2019/12/30"), by = "16 day")
evidatedf <- rbind(
  data.table(dates = seq_dates1),
  data.table(dates = seq_dates2),
  data.table(dates = seq_dates3),
  data.table(dates = seq_dates4),
  data.table(dates = seq_dates5)
) %>%
  rename(modis_date1 = 1) %>%
  mutate(modis_date2 = modis_date1+16)

final_hh_evidate_df <- final_hh_df %>%
  fuzzy_left_join(
    evidatedf,
    by = c(
      "time1" = "modis_date1",
      "time1" = "modis_date2"
    ),
    match_fun = list(`>=`,`<`)
  ) 

# Select final date to extract from NOAA... need date that is closest to the interview date
final_hh_evi_df <- final_hh_evidate_df %>%
  mutate(
    modis_date_fin = case_when(
      abs(as.numeric(time1 - modis_date1)) > abs(as.numeric(time1 - modis_date2)) ~ modis_date2,
      abs(as.numeric(time1 - modis_date1)) < abs(as.numeric(time1 - modis_date2)) ~ modis_date1,
      TRUE ~ modis_date1
    ))


# Generate distinct dates, lat & lon dataframe to download using {MODISTools}
distevidatedf <- final_hh_evi_df %>% 
  distinct(modis_date_fin,latitude,longitude) %>%
  # There are some rows where latitude & longitude doesn't exist...
  filter(!is.na(latitude) & !is.na(longitude))

evi_band <- mt_bands("MOD13Q1")

eviextractdf <- distevidatedf %>%
  mutate(
    product = "MOD13Q1",
    bandinfo = evi_band[11,"band"]
    ) 

# Extract all EVI values
plan(multiprocess)
evdf <- eviextractdf %>%
  future_pmap_dfr(extract_evi, .progress = TRUE)


# 4. Extract Temperature Data ---------------------------------------------
seq_dates1 <- seq(as.Date("2015/1/1"), as.Date("2015/12/30"), by = "8 day")
seq_dates2 <- seq(as.Date("2016/1/1"), as.Date("2016/12/30"), by = "8 day")
seq_dates3 <- seq(as.Date("2017/1/1"), as.Date("2017/12/30"), by = "8 day")
seq_dates4 <- seq(as.Date("2018/1/1"), as.Date("2018/12/30"), by = "8 day")
seq_dates5 <- seq(as.Date("2019/1/1"), as.Date("2019/12/30"), by = "8 day")
tempdatedf <- rbind(
  data.table(dates = seq_dates1),
  data.table(dates = seq_dates2),
  data.table(dates = seq_dates3),
  data.table(dates = seq_dates4),
  data.table(dates = seq_dates5)
) %>%
  rename(modis_date1 = 1) %>%
  mutate(modis_date2 = modis_date1+8)

final_hh_tempdate_df <- final_hh_df %>%
  fuzzy_left_join(
    tempdatedf,
    by = c(
      "time1" = "modis_date1",
      "time1" = "modis_date2"
    ),
    match_fun = list(`>=`,`<`)
  ) 

final_hh_temp_df <- final_hh_tempdate_df %>%
  mutate(
    modis_date_fin = case_when(
      abs(as.numeric(time1 - modis_date1)) > abs(as.numeric(time1 - modis_date2)) ~ modis_date2,
      abs(as.numeric(time1 - modis_date1)) < abs(as.numeric(time1 - modis_date2)) ~ modis_date1,
      TRUE ~ modis_date1
    ))


# Generate distinct dates, lat & lon dataframe to download using {MODISTools}
disttempdatedf <- final_hh_temp_df %>% 
  distinct(modis_date_fin,latitude,longitude) %>%
  # There are some rows where latitude & longitude doesn't exist...
  filter(!is.na(latitude) & !is.na(longitude))

temp_band <- mt_bands("MOD11A2")
tempextractdf <- disttempdatedf %>%
  mutate(
    product = "MOD11A2",
    bandinfo = temp_band[6,"band"]
    ) 

# Extract all EVI & TEMPERATURE values
plan(multiprocess)
tempdf <- tempextractdf %>%
  future_pmap_dfr(extract_evi, .progress = TRUE)

# 5. Merge extracted modis info & final hh data ---------------------------

finaldf <- final_hh_evi_df %>%
  dplyr::select(-modis_date1,-modis_date2) %>%
  rename(evi_date_fin = modis_date_fin) %>%
  left_join(
    final_hh_temp_df %>%
      dplyr::select(household,stime,modis_date_fin) %>%
      rename(temp_date_fin = modis_date_fin) 
  ) %>%
  left_join(
    evdf %>%
      rename(
        evi_date_fin = 1,
        EVI16=value
        )
  ) %>%
  left_join(
    tempdf %>%
      rename(
        temp_date_fin = 1,
        TEMP8=value
        )
  )

saveRDS(finaldf, paste0(here(),"/data/process/final_hh_env_df.RDS")) # in the process folder
rm(list=ls())

