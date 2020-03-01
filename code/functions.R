# File Info ---------------------------------------------------------------
#### File Name:   functions.R
#### Purpose:     Function file
#### Created by:  CK
#### Created on:  1/26/2018
#### Modified by: CK
#### Modified on: 2/24/2020
#- - - - - - - - - - - - - - - - - - - -#

extract_evi <- function(product="MOD13Q1",
                        bandinfo,
                        modis_date_fin,
                        longitude,
                        latitude){
  
  subset <- mt_subset(product=product,
                      lat=latitude,
                      lon=longitude,
                      band=bandinfo, # this is band for 16 day avg EVI
                      start=modis_date_fin,
                      end=modis_date_fin + 1,
                      km_lr = 0,
                      km_ab = 0,
                      internal = TRUE,
                      progress = FALSE
  )
  
  return(subset %>%
           mutate(
             latitude=latitude,
             longitude=longitude,
             calendar_date = modis_date_fin
           ) %>%
           dplyr::select(calendar_date, latitude, longitude, value)
  )
}
