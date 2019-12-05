#- - - - - - - - - - - - - - - - - - - - - -#
# Environmental Data Management
### Created by: CK
### Created on: 1/11/2017
### Modified by: CK
### Modified on: 10/18/2018
#- - - - - - - - - - - - - - - - - - - - - -#

# This script is to manage files associated with:
# 1. Enhanced Vegetation Index 
# 2. Temperature, Humidity, Rain

# The steps are outlined below
# 1. First identify the unique dates that we need for the downloadable data
# 2. Download the data based on the unique dates
# 3. Subset the region of interest that we want to crop from the files
# 4. Merge the subsetted data (as Spatial Polygon or Points) onto the dataframe we have

#- - - - - - - - - - - - - - - - - - - - - -#
# Identify the dates!                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# so here let's start with matching the temperature data since it has more number of unique dates and will be harder to do

# load library to read in the hdf files
rm(list=ls())
library(sp)
library(rgdal)
library(raster)
library(gdalUtils)
library(dplyr)
library(tidyr)
library(data.table)

# directly incorporate using raster function
setwd("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/")
# import our original dataframe without EVI info
final_hh_df <- readRDS("../process/final_hh_df.RDS")


# Previously in the data_management.R script we've found the dates of the survey for each household so we just need to find the dates to download the environmental data

# check summary to grasp the min and max
summary(final_hh_df$time1)

# now we need to select the distinct dates
# how many distinct rows?

final_hh_df %>%
  ungroup() %>%
  filter(county == 2) %>%
  dplyr::select(time1) %>%
  distinct(., time1)

# 14 distinct dates to download the EVI data... how about temperature?

#- - - - - - - - - - - - - - - - - - - - - -#
# EVI Download                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# So the problem is that the EVI dataset only has 16 days intervals of data... so we won't actually get the exact dates that we want... In this case we need to identify dates closest to the dataset of interest

# The structure of the data set is that it starts from 01/01/2016 to 12/18/2016... so adding 16. we just need to perhaps fuzzy match on the dates closest... let's simulate a dataseet/list that is from 1/1/2016 to 12/18/2017

seq_dates1 <- seq(as.Date("2015/1/1"), as.Date("2015/12/30"), by = "16 day")
seq_dates2 <- seq(as.Date("2016/1/1"), as.Date("2016/12/30"), by = "16 day")
seq_dates3 <- seq(as.Date("2017/1/1"), as.Date("2017/12/30"), by = "16 day")

# Let's put that into a dataframe where we add a column along with that
library(data.table)
temp <- rbind(
  data.table(dates = seq_dates1, data = paste0("MOD13Q1.A2015",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006")),
  data.table(dates = seq_dates2, data = paste0("MOD13Q1.A2016",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006")),
  data.table(dates = seq_dates3, data = paste0("MOD13Q1.A2017",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006"))
)

# courtesy of s.o. for sprint:https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros

# Let's do a fuzzy match on the dates that we have with the df

#final_hh_df[,c("household","longitude","latitude","time1")]
# we need up to 15 wks of temperature which would be of the 16 days increment which is approximately 2 wks then we need to do 8 16 days increment or 8 * 2 wks increment to get approximately 16 wks work of temperature data

FH <- data.table(final_hh_df[,c("household","longitude","latitude","time1")])

# calculate the 16day lag for 8 wks each
for(i in 0:8){
    FH[, paste0("time_",(i*16)) := time1-(i*16)]
}
FH <- FH %>%
    gather(., datelag, time1, time1:time_128) %>%
    #group_by(household, longitude, latitude, dates) %>%
    distinct(., household, longitude, latitude, time1, .keep_all = TRUE) %>%
    dplyr::select(household, longitude, latitude, time1) %>%
    arrange(household)
temp <- data.table(temp)
FH <- data.table(FH)
setkey(FH,time1)
temp[,mergerdate:=dates]
setkey(temp,mergerdate)
# this is where we essentially fuzzy match the temp to FH time keys
merge<-temp[FH,roll=TRUE]
setnames(merge,"mergerdate","date")



# courtesy to :https://stackoverflow.com/questions/9269179/r-speeding-up-approximate-date-match-idata-frame

# Now we need to get the unique MODIS file names and just need to download it from here :)

unique(merge$data) # these are the unique files to download with regards to the MODIS files
unique(merge$dates) # for pymodis module in python we just need these dates actually... 
# So we can invoke the pymodis module from here and let it run in the background

# Download MODIS hdf file based on the unique dates
#sapply(unique(merge$dates)[-1], function(x) {
sapply(unique(merge$dates), function(x) {
           system(
                  paste0("modis_download.py -U 'kimchon' -P 'CkMj1527' -r -t h21v08 -p MOD13Q1.006 -f ",x," ~/Documents/Projects/DrColborn/UgandaNets/data/raw/MODIS -O")
                  )
})

#- - - - - - - - - - - - - - - - - - - - - -#
# Match the EVI data now!              ----
#- - - - - - - - - - - - - - - - - - - - - -#

ex <- extent(c(3732852, 3843852, 170000,205800))
# create a sequence data frame that will take the mered dates 
tmp99 <- sapply(unique(merge$dates), function(x) {
                    # first load the MODIS data
                    mod_hdf <- system(
                                      paste0("ls ./MODIS/",
                                             substr(paste0(merge %>% distinct(dates, data) %>% filter(dates == x) %>% dplyr::select(data)),0, 28),
                                             "*.hdf"), intern = TRUE
                                      )
                    # Subset MODIS data set
                    sds_hdf <- get_subdatasets(mod_hdf)

                    # save the files into a tif file
                    gdal_translate(sds_hdf[2], dst_dataset = paste0(mod_hdf,".tif"),
                                   projwin = c(xmin(ex),ymax(ex),xmax(ex),ymin(ex)))
                    # rasterize the geotiff file
                    rasts <- raster(paste0(mod_hdf,".tif"))

                    # change the CRS from sinusoidal to wgs84
                    llcrs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
                    rll <- projectRaster(rasts, crs=llcrs)
                    # it's possible that all of the values are NA due to faulty measurement
                    spoly <- raster::rasterToPolygons(rll)
                    # match that of the polygon with that of the spatial points
                    spdf <- SpatialPointsDataFrame(coords = merge %>%
                                                   filter(dates == x) %>%
                                                   dplyr::select(longitude, latitude), 
                                               data = merge %>%
                                                   filter(dates == x),
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

                    # now find the overlapping regions between the spdf and evi_sp
                    try(EVI <- sp::over( spdf , spoly ))

                    # bind the tempK data to the spdf
                    try(spdf@data <- cbind( spdf@data, EVI ))
                    # change name of temp column
                    try(colnames(spdf@data)[ncol(spdf@data)] <- "EVI")
                    spdf
})

# rbind all of the dataset in the list
tmp_df1 <- do.call("rbind", tmp99)
#saveRDS(tmp_df1, "../../process/EVI_lag.RDS")
tmp_df1 <- readRDS("../process/EVI_lag.RDS")
#tmp_df2 <- readRDS("../../process/temp_lag.RDS")

#table(is.na(tmp_df2@data$tempK))
table(is.na(tmp_df1@data$EVI))


tmp23 <- final_hh_df %>% 
    select(-contains("EVI"),-contains("tempK")) %>%
    left_join(
              tmp_df1@data %>%
                  select(-data, -dates) %>%
                  rename(time_0 = date) 
                  ) 
table(is.na(tmp23$EVI))
rm(tmp23)




#saveRDS(tmp_df2, "../../process/temp_lag.RDS")


#- - - - - - - - - - - - - - - - - - - - - -#
# Temperature Download                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# For temperature data we can download daily data... so
length(unique(merge$data)) # all 37 should be downloaded... kinda long wouldn't you say? 

# create a sequential data again just to make it into format that pymodis_download_list will be happy with 

seq_dates1 <- seq(as.Date("2015/1/1"), as.Date("2015/12/30"), by = "16 day")
seq_dates2 <- seq(as.Date("2016/1/1"), as.Date("2016/12/30"), by = "16 day")
seq_dates3 <- seq(as.Date("2017/1/1"), as.Date("2017/12/30"), by = "16 day")

temp <- rbind(
  # save the name in this form: MOD11A1.A2012278.h19v11.005.*.hdf*
  data.table(dates = seq_dates1, data = paste0("MOD11A1.A2015",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006.*.hdf*")),
  data.table(dates = seq_dates2, data = paste0("MOD11A1.A2016",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006.*.hdf*")),
  data.table(dates = seq_dates3, data = paste0("MOD11A1.A2017",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006.*.hdf*"))
)
    
# match again
FH <- data.table(final_hh_df[,c("household","longitude","latitude","time1")])

# calculate the 16day lag for 8 wks each
for(i in 0:8){
    FH[, paste0("time_",(i*16)) := time1-(i*16)]
}
FH <- FH %>%
    gather(., datelag, timelag, time_0:time_128) %>%
    #group_by(household, longitude, latitude, dates) %>%
    distinct(., household, longitude, latitude, timelag, .keep_all = TRUE) %>%
    dplyr::select(household, longitude, latitude, time1, timelag) %>%
    arrange(household)
temp <- data.table(temp)
FH <- data.table(FH)
setkey(FH,timelag)
temp[,mergerdate:=dates]
setkey(temp,mergerdate)
# this is where we essentially fuzzy match the temp to FH time keys
merge<-temp[FH,roll=TRUE]
setnames(merge,"mergerdate","date")
write(unique(merge$data), "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/MODIS/MODTiles.txt") # the unique data file we need to downnload
head(unique(merge$data))
unique(merge$data)

# to download from system
# try downloading the temperature file using modis_download_list
system(
  "modis_download_from_list.py -U 'kimchon' -P 'CkMj1527' -f /home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/MODIS/MODTiles.txt /home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/MODIS"
)

# Now we have all the weather and EVI information! Just need to merge it into the data that we have :) 

# Note: The pymodis download sequence didn't seem to be in a parallel fashion... (even though I tried the sapply) it seemed that the program was connecting one at a time to the NASA server, use doParallel with foreach() to spread it across the clusters...

# information on the correct tiles, format, dates, and etc are here: https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/


#- - - - - - - - - - - - - - - - - - - - - -#
# Match the data!                 ----
#- - - - - - - - - - - - - - - - - - - - - -#
merge[.(dates = unique(merge$dates)), .(data)] %>% tail(10)
merge[.(date = unique(merge$dates)), .(dates, household, longitude, latitude)] # this is the data frame that we want to merge the temperature file from the unique date to be merged.. we may need to create a function for this to parallelize the merging but let's first go through one example
setwd("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/MODIS")
tmp1 <- system(
  paste0("ls ",substr(paste0(unique(merge[.(date = unique(merge$date)[4]), .(data)])),0, 28),"*.hdf"), intern = TRUE
) # this is the file name that matches the unique date that we want to download
unique(merge$date)[-1]
# we can get the gdalinfo
gdalinfo(tmp1)
sds <- get_subdatasets(tmp1)
sds[1] # daytime land surface temperature is the 1st index
# create the extnet to which we need information from the tif file
ex <- extent(c(3732852, 3843852, 170000,205800))
ex <- extent(c(3432852, 4043852, 170000,255800))
# get the EVI dataset which will be created as a geotiff file in the /data/ folder
gdal_translate(sds[1], dst_dataset = paste0(tmp1,".tif"),
               projwin = c(xmin(ex),ymax(ex),xmax(ex),ymin(ex)))
# load the .tif file 
rasts <- raster(paste0(tmp1,".tif"))
# let's check the plot
plot(rasts)
# check the projection for the raster file...
proj4string(rasts)
# we see that it's in sinusoidal ... let's convert it into wgs84 since our original data is in that format
llcrs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
rll <- projectRaster(rasts, crs=llcrs)
all(is.na(rll@data@values)) # check if all of them are NA... if then then we would need to make sure they aren't all NA's if they are we 
summary(rll@data@values)
temp_sp <- rasterToPolygons(rll)
proj4string(temp_sp)
summary(coordinates(temp_sp))
temp_sp@bbox

# match that of the polygon with that of the spatial points
df <- data.table(final_hh_df)

# lagged data frame (in wide format)
df <- data.table(final_hh_df) %>%
    left_join(
              merge %>%
                  arrange(desc(dates)) %>%
                  group_by(household) %>%
                  mutate(rank = row_number()-1) %>%
                  mutate(rank = paste0("time_",rank)) %>%
                  select(household, time1, dates, rank) %>%
                  spread(rank, dates) 
              )
    

df <- as.data.frame(df) # need to convert the tibble data that is grouped into just a simple data frame


tmp2 <- sapply(0:8, function(x){
           over(SpatialPointsDataFrame(coords = df %>%
                                  filter_(paste0("time_",x,"== unique(.$time_",x,")")) %>%
                                  dplyr::select(longitude, latitude),
                              data = df %>%
                                  filter_(paste0("time_",x,"== unique(.$time_",x,")")),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")), temp_sp)
              })

rbindlist(lapply(tmp2, function(x) { data.frame(x)}), id='id')

df <- data.frame(merge)
              
spdf <- SpatialPointsDataFrame(coords = df %>%
                                 filter(time1 == unique(merge$time1)[10]) %>%
                                 dplyr::select(longitude, latitude), 
                             data = df %>%
                                 filter(time1 == unique(merge$time1)[10]),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
summary(coordinates(spdf))
# now find the overlapping regions between the spdf and evi_sp
tempK <- over( spdf , temp_sp )
# we have the temperature for those households matched with long and lat 
spdf@data <- cbind( data.frame(spdf@data), tempK )
head(spdf)
# we now need to go through the entire list of files and rowbind to this dataframe

#- - - - - - - - - - - - - - - - - - - - - -#
# Vectorize the Process!                 ----
#- - - - - - - - - - - - - - - - - - - - - -#
tmp99 <- sapply(unique(merge$dates), function(x) {
                    # first load the MODIS data
                    mod_hdf <- system(
                                      paste0("ls ",
                                             substr(paste0(merge %>% distinct(dates, data) %>% filter(dates == x) %>% dplyr::select(data)),0, 28),
                                             "*.hdf"), intern = TRUE
                                      )
                    # Subset MODIS data set
                    sds_hdf <- get_subdatasets(mod_hdf)

                    # save the files into a tif file
                    ex <- extent(c(3432852, 4043852, 170000,255800))
                    gdal_translate(sds_hdf[1], dst_dataset = paste0(mod_hdf,".tif"),
                                   projwin = c(xmin(ex),ymax(ex),xmax(ex),ymin(ex)))
                    # rasterize the geotiff file
                    rasts <- raster(paste0(mod_hdf,".tif"))

                    # change the CRS from sinusoidal to wgs84
                    llcrs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
                    rll <- projectRaster(rasts, crs=llcrs)
                    # it's possible that all of the values are NA due to faulty measurement
                    # check if all of them are NA... if then then we would need to make sure they aren't all NA's if they are we 
                    if(all(is.na(rll@data@values))){
                        # match that of the polygon with that of the spatial points
                        spdf <- SpatialPointsDataFrame(coords = merge %>%
                                                       filter(dates == x) %>%
                                                       dplyr::select(longitude, latitude), 
                                                   data = merge %>%
                                                       filter(dates == x),
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                        spdf@data$tempK <- NA
                    } else {

                        spoly <- raster::rasterToPolygons(rll)
                        # match that of the polygon with that of the spatial points
                        spdf <- SpatialPointsDataFrame(coords = merge %>%
                                                       filter(dates == x) %>%
                                                       dplyr::select(longitude, latitude), 
                                                   data = merge %>%
                                                       filter(dates == x),
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

                        # now find the overlapping regions between the spdf and evi_sp
                        try(tempK <- sp::over( spdf , spoly ))

                        # bind the tempK data to the spdf
                        try(spdf@data <- cbind( spdf@data, tempK ))
                    
                    }
                    try(colnames(spdf@data)[ncol(spdf@data)] <- "tempK")
                    spdf
})

# rbind all of the dataset in the list
tmp_df2 <- do.call("rbind", tmp99)
#saveRDS(tmp_df2, "../../process/temp_laga.RDS")
#saveRDS(tmp_df2, "../process/temp_lag.RDS")
tmp_df2 <- readRDS("../process/temp_lag.RDS")

# Here we have missing data so let's impute based on date, long, lat
library(mice)
            select(longitude, latitude, dates, tempK), m=10, method="norm.predict",
        seed=1234)

tmp_df2@data <- tmp_df2@data %>%
    select(-contains("tempK")) %>%
    left_join(complete(imp))

tmp_df2@data %>% head


# - - - - - - - - - - - - - - - - - - - - - - - - - -
# Merge and match EVI and Temperature data into 
# - - - - - - - - - - - - - - - - - - - - - - - - - -

FH <- data.table(final_hh_df[,c("household","longitude","latitude","time1")])
FH <- FH %>% distinct(household, .keep_all=TRUE) 

#-- dates for the actual survey is different from that of the date of EVI and temperature extraction so we need to add a column to the final dataset that is of the date of extraction for EVI and temperature
FH <- FH %>%
    right_join(
               tmp_df1@data %>%
                   select(household, dates, EVI)) %>%
    arrange(household, dates) %>%
    left_join(
              tmp_df2@data %>%
                  select(household, dates, tempK)) %>%
    arrange(household, dates) 
FH <- FH %>%
    distinct(household, dates, .keep_all=TRUE) 


tmp23 <- FH %>% 
    distinct(household, dates, .keep_all=TRUE) %>%
    group_by(household) %>%
    arrange(household, desc(dates)) %>%
    mutate(rrank = (row_number()-1)*16) %>%
    mutate(rrank = paste0("time_",rrank)) 
    #spread(rank, EVI) %>%

#-- EVI columns
tempEVIwide <- tmp23 %>% 
    select(household, rrank, tempK) %>%
    spread(rrank, tempK) %>%
    rename_(.dots=setNames(names(.), gsub("time_", "tempK_", names(.))))  %>%
left_join(
          tmp23 %>% 
              select(household, rrank, EVI) %>%
              spread(rrank, EVI) %>%
              rename_(.dots=setNames(names(.), gsub("time_", "EVI_", names(.)))) 
          )

#-- merge everything back
final_hh_df <- final_hh_df %>%
    distinct(household, .keep_all=TRUE) %>%
    select(-contains("tempK"), -contains("EVI")) %>%
    left_join( 
              tempEVIwide) 

final_hh_df %>%
    colnames


# save laggedevi and temp
saveRDS(final_hh_df, "../process/final_hh_df.RDS")
saveRDS(tmp_df1, "../../process/EVI_lag.RDS")
saveRDS(tmp_df2, "../../process/temp_lag.RDS")


## The below code is something that I would like to use as a backbone for if-then switch algorithm
#tmp1 <- system(
  #paste0("ls ",substr(paste0(unique(merge[.(date = unique(merge$date)[11]), .(data)])),0, 28),"*.hdf"), intern = TRUE
#) # this is the file name that matches the unique date that we want to download

## we can get the gdalinfo
#gdalinfo(tmp1)
#sds <- get_subdatasets(tmp1)
#sds[1] # daytime land surface temperature is the 1st index
## create the extnet to which we need information from the tif file
#ex <- extent(c(3732852, 3843852, 170000,205800))
## get the EVI dataset which will be created as a geotiff file in the /data/ folder
#gdal_translate(sds[1], dst_dataset = paste0(tmp1,".tif"),
               #projwin = c(xmin(ex),ymax(ex),xmax(ex),ymin(ex)))
## load the .tif file 
#rasts <- raster(paste0(tmp1,".tif"))
## let's check the plot
#plot(rasts)
#summary(rll@data@values)
#temp_sp <- rasterToPolygons(rll)
#proj4string(temp_sp)
#summary(coordinates(temp_sp))

# match that of the polygon with that of the spatial points
df <- as.data.frame(df) # need to convert the tibble data that is grouped into just a simple data frame
spdf <- SpatialPointsDataFrame(coords = df %>%
                                 filter(time1 == unique(merge$date)[9]) %>%
                                 dplyr::select(longitude, latitude), data = df %>%
                                 filter(time1 == unique(merge$date)[9]),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
summary(coordinates(spdf))
# now find the overlapping regions between the spdf and evi_sp
# tempK <- over( spdf , temp_sp )
# colnames(tmp99[[9]]@data)[ncol(tmp99[[9]])] <- "time1"
# # we have the temperature for those households matched with long and lat 
# tmp99[[9]]@data <- cbind( tmp99[[9]]@data, tempK )
# colnames(tmp99[[9]]@data)[ncol(tmp99[[9]])] <- "TempK"
# colnames(tmp99[[9]]@data)
# # rbind all of the dataset in the list
tmp_df <- do.call("rbind", tmp99)

#- - - - - - - - - - - - - - - - - - - - - -#
# Altitude Info                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# Although the original data has had altitude information there are too many missing. Let's take a look.
table(is.na(final_hh_df$alt_srtm)) # 1137 households with missing altitude information... It's most likely that the altitude of the area probably doesn't change as much with time so this won't affect the RDT rate over time...but it's still possible that lower altitude areas have a higher rate vs lower altitude areas.. let's downlaod the data

# download using raster packae
library(raster)
UGA0 <- getData('GADM', country='UGA', level=0)
UGA1 <- getData('GADM', country='UGA', level=1)
UGA2 <- getData('GADM', country='UGA', level=2)

##Plot
#par(mfrow=c(2,1))
#plot(UGA0, main="Adm. Boundaries Uganda Level 0")
#plot(UGA1, main="Adm. Boundaries Uganda Level 1")
#plot(UGA2, main="Adm. Boundaries Uganda Level 2")

# get elevation
# first check the longlat 
summary(tmp_df@data$latitude); summary(tmp_df@data$longitude)
try(
    
    # although we get an error the geotiff is downloaded and extracted in the working directory
    srtm <- getData('SRTM', lon=32.5, lat=2.5, download = TRUE)
    )
srtm <- raster(paste0("srtm_43_12.tif"))
#plot(srtm)
#plot(UGA1, add = TRUE)

proj4string(srtm) # already in long lat now we just need to convert to polygon
cells <- cellsFromExtent(srtm, extent = extent(33.82, 34.24, 1.64, 1.81))
r <- crop(srtm, extent(33.82, 34.24, 1.64, 1.81))
# need to reproject even though they are in WGS84, the overlaying won't work if the CRS isn't exact
llcrs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
rll <- projectRaster(r, crs=llcrs)
sp_alt <- rasterToPolygons(rll)


ug_spdf <- tmp_df
colnames(tmp_df@data)[20] <- "date" # change the stime column to date
ug_spdf@data <- cbind(ug_spdf@data, tmp_df1@data$EVI)

alt_srtm <- sp::over( ug_spdf , sp_alt ) # over laying the areas and we get the 1741 altitude values
table(is.na(alt_srtm)) # none missing 
# rename previous alt_srtm column
ug_spdf@data <- ug_spdf@data %>%
    rename(alt_srtma = alt_srtm)

ug_spdf@data <- cbind(ug_spdf@data, alt_srtm)

# all data has been merged :) 
??? from here until ???END lines may have been inserted/deleted

# rename
ug_spdf@data <- ug_spdf@data %>%
  rename(EVI = "tmp_df1@data$EVI",
         alt_srtm = "srtm_43_12")
??? from here until ???END lines may have been inserted/deleted

# rename
ug_spdf@data <- ug_spdf@data %>%
  rename(EVI = "tmp_df1@data$EVI",
         alt_srtm = "srtm_43_12")

#- - - - - - - - - - - - - - - - - - - - - -#
# Rainfall Info                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# Rainfall courtesy to: https://matinbrandt.wordpress.com/2013/09/04/automatically-downloading-and-processing-trmm-rainfall-data/
library("devtools")
#devtools::install_github("environmentalinformatics-marburg/heavyRain")
#library(Rsenal)

#-- File downloaded using the ftp script in data/raw/TRMM/ftp*.py
#-- ned the rhdf5 package from bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
h5ls("./TRMM/3B-DAY.GPM.DPRGMI.CORRAGD.20181018-S000000-E235959.291.V06A.HDF5")

help(h5read)

tmp23 <- H5Fopen("./TRMM/3B-DAY.GPM.DPRGMI.CORRAGD.20181018-S000000-E235959.291.V06A.HDF5")

mydata <- h5read("./TRMM/3B-DAY.GPM.DPRGMI.CORRAGD.20181018-S000000-E235959.291.V06A.HDF5", "/mygroup/mydata")
ex <- extent(c(3432852, 4043852, 170000,255800))

temp <- rbind(
  data.table(dates = seq_dates1, data = paste0("MOD13Q1.A2015",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006")),
  data.table(dates = seq_dates2, data = paste0("MOD13Q1.A2016",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006")),
  data.table(dates = seq_dates3, data = paste0("MOD13Q1.A2017",sprintf("%03d",seq(from = 1, to = 353, by = 16)),".h21v08.006"))
)

unique(merge$dates)


#- - - - - - - - - - - - - - - - - - - - - -#
# Data Check                 ----
#- - - - - - - - - - - - - - - - - - - - - -#

# check the missing data

table(is.na(ug_spdf@data$EVI))# non are missing :)
table(is.na(ug_spdf@data$TempK))# There are 607 that are missing...
table(is.na(ug_spdf@data$alt_srtm))# There are 607 that are missing...

saveRDS(ug_spdf@data, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS")
???END
