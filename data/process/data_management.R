#- - - - - - - - - - - - - - - - - - - -#
#### Data Management
#### Created by: CK
#### Created on: 1/26/2018
#### Modified by: CK
#### Modified on: 9/11/2018
#- - - - - - - - - - - - - - - - - - - -#

# WARNING: Run the data_management.R file to get the necessary files for visualization and exploratory analysis here 

rm(list=ls())



#- - - - - - - - - - - - - - - - - - - -#
### 1. RDT and Microscopy Data  ----
#- - - - - - - - - - - - - - - - - - - -#

### Read the most recent CleanS1S2S3rdtMicroscopyGPS_20180223.csv file

library(data.table)

# merged <- fread("/home/ck1/Documents/Projects/DrColborn/Uganda/data/CleanS1S2S3rdtMicroscopyGPS_20180223.csv")
merged<- fread("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/raw/Microscopy/CleanS1S2S3S4rdtMicroscopyGPS_20180608.csv")
#### Check irregularities 
# table(merged$cleanMicroResult) #1172 positive 5625 negative no NA's
# table(is.na(merged$longitude)) #229 missing long/lat coordinate

table(merged$cleanMicroResult) #1365 positive 7556 negative no NA's
table(is.na(merged$longitude)) #253 missing long/lat coordinate

# Check duplicates of household
library(DescTools)
table(AllDuplicated(merged$household))


table(is.na(merged$longitude))# 253 missing

### The allmerged dataset has households that don't have GPS information... can't do much with them
library(dplyr)
merged_wgps <- merged %>%
  filter(!is.na(longitude))


#### Load the net usage data now


#- - - - - - - - - - - - - - - - - - - -#
### 3. Net Usage Data  ----
#- - - - - - - - - - - - - - - - - - - -#

library(readxl)
s1net <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey1/tbl_HouseholdNets.xlsx", sheet =  1)
s2net <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey2/tbHouseholdNetsSurvey2.xlsx", sheet =  1)
s3net <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey3/HouseholdNets.xlsx", sheet =  1)
s4net <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey4/tbHouseholdNets.xlsx", sheet =  1)

#### People need to standardize across each of the datasets in terms of the variable name and the information that is being gathered... surveys 1, 2, and 3 all have slightly different variable names...

table(AllDuplicated(paste0(s1net$autohhid2,'-',s1net$Qn131_netno))) # 46 duplicates
table(AllDuplicated(paste0(s2net$household,'-',s2net$tbl_HouseholdNets))) # no duplicates
table(AllDuplicated(paste0(s3net$household,'-',s3net$tbl_HouseholdNets))) # no duplicates
table(AllDuplicated(paste0(s4net$household,'-',s4net$tbl_HouseholdNets))) # no duplicates

### Since there are 46 duplicate net info we will only use the unique hh 

s1net_f <- s1net %>%
  mutate(studyid = paste0(autohhid2, Qn131_netno),
         household = as.numeric(sub("HH","",s1net$autohhid2)),
         stime = 1)  %>%
  dplyr::select(household, Qn131_netno, Qn137_sleptundernet, stime)

s2net_f <- s2net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         stime = 2)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet, stime)

s3net_f <- s3net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         stime = 3)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet, stime)

s4net_f <- s4net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         household = as.numeric(household),
         stime = 4)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet, stime)

### In each of the net datasets one can set up a filter to remove the duplicated values. Most of them are missing but if you remove them it's possible that some of them were sleeping under the net but didn't record the net number (for some reason...)

temp <- merged_wgps %>%
  rename(household = householdID,
         stime = survey) %>%
  mutate(rdtPos = ifelse(rdtResult == 1, 1, ifelse(is.na(rdtResult) == TRUE, NA, 0)),
         micPos = ifelse(cleanMicroResult == 1, 1, ifelse(is.na(cleanMicroResult) == TRUE, NA, 0)),
         stime = as.numeric(stime)) %>%
  group_by(household, stime) %>%
  summarise_each(funs(Sum = sum(., na.rm = TRUE)), vars = c("rdtPos","micPos"))

### Sum all net use 
temp2 <- s1net_f %>%
  mutate(net = 1) %>%
  group_by(household) %>%
  dplyr::summarise(num_net = sum(net)) %>%
  mutate(stime = 1)


### merge net use data from survey2
net_merged <- bind_rows(temp2, s2net_f %>%
                          mutate(net = 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(num_net = sum(net)) %>%
                          mutate(stime = 2))

### merge net use data from survey3
net_merged <- bind_rows(net_merged, s3net_f %>%
                          mutate(net = 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(num_net = sum(net)) %>%
                          mutate(stime = 3))

### merge net use data from survey4
net_merged <- bind_rows(net_merged, s4net_f %>%
                          mutate(net = 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(num_net = sum(net)) %>%
                          mutate(stime = 4))

### now join with the gps data
temp3 <- left_join(temp, net_merged)
temp4 <- left_join(temp3, merged_wgps %>%
                     rename(household = householdID,
                            stime = survey) %>%
                     mutate(stime = as.numeric(stime)) %>%
                     dplyr::select(household, latitude, longitude, stime) %>%
                     group_by(household, latitude, longitude, stime) %>%
                     distinct(household), by = c("household","stime"))

### Let's check the total number in household data now

#- - - - - - - - - - - - - - - - - - - -#
### 4. Household Survey Data  ----
#- - - - - - - - - - - - - - - - - - - -#


s1hhd <- read_excel("~/Documents/Projects/DrColborn/Uganda/data/Survey1/tbl_Household_Survey.xlsx", sheet =  )
s2hhd <- read_excel("~/Documents/Projects/DrColborn/Uganda/data/Survey2/tbHousehold_Survey2.xlsx", sheet =  1)
s3hhd <- read_excel("~/Documents/Projects/DrColborn/Uganda/data/Survey3/Household_Survey.xlsx", sheet =  1)
s4hhd <- read_excel("~/Documents/Projects/DrColborn/Uganda/data/Survey\ r4/tbHousehold_Survey.xlsx", sheet =  1)

hhtn <- s1hhd %>%
  dplyr::select(hhid, `eligibleNumbers-Qn10_totalpersons`) %>%
  ### There are some households that have -x (where x = [0-9]) there will be a warning... basically we take the maximum of the household
  mutate(household = as.numeric(substr(sub("HH","", hhid), 1, 9))) %>%
  rename(num_hh = `eligibleNumbers-Qn10_totalpersons`) %>%
  dplyr::select(num_hh, household) %>%
  ### There are duplicates.. so take the one with the higher number for now
  filter(!duplicated(household)) %>%
  mutate(num_hh = as.numeric(num_hh),
         stime = 1) %>%
  ### Bind with survey 2 total number in hh
  bind_rows(., s2hhd %>%
              dplyr::select(household, Qn10_TotalPersonsInHH) %>%
              dplyr::rename(num_hh = Qn10_TotalPersonsInHH) %>%
              mutate(num_hh = as.numeric(num_hh),
                     stime = 2) ) %>%
  ### Bind with survey 3 total number in hh
  bind_rows(., s3hhd %>%
              dplyr::select(household, Qn10_TotalPersonsInHH) %>%
              dplyr::rename(num_hh = Qn10_TotalPersonsInHH) %>%
              mutate(num_hh = as.numeric(num_hh),
                     stime = 3) ) %>%
  bind_rows(., s4hhd %>%
              dplyr::select(household, Qn10_TotalPersonsInHH) %>%
              dplyr::rename(num_hh = Qn10_TotalPersonsInHH) %>%
              mutate(num_hh = as.numeric(num_hh),
                     household = as.numeric(household),
                     stime = 4) )



### Join the 2nd houshold survey that has total number of households
temps <- temp4 %>%
  left_join(hhtn) 


### Check number missing
table(is.na(temps$num_hh)) # 4 households with missing

### Average number in HH by county
s2hhd %>%
  group_by(Scounty) %>%
  dplyr::summarise(num_hh = mean(as.numeric(Qn10_TotalPersonsInHH)))

### For those that are missing the total number in household... we'll see that each household will have less number in household due to the way that the survey was collected... so we'll use the max by each household as the number in the household. Also make sure that if there are any NA's then just choose the non-NA value.

temp5 <- temps %>%
  mutate(num_hh_final = ifelse(is.na(num_hh) == TRUE, 
                               # so if there is `NA` then we need to use the num_net or rdtPos_Sum 
                               ifelse(is.na(num_net) == TRUE & is.na(rdtPos_Sum) == TRUE, NA, 
                                      ifelse(is.na(num_net) == TRUE, rdtPos_Sum, 
                                             ifelse(is.na(rdtPos_Sum) == TRUE, num_net, 
                                                    ifelse(rdtPos_Sum > num_net, rdtPos_Sum, num_net)))
                               ), num_hh)) %>%
  # Since there is one household that indicated 0 in household... wtf?
  mutate(num_hh_final = ifelse(num_hh_final == 0, 1, num_hh_final))


### Now also need to merge in the at least once usage data

### To merge smoothly have the aggregated net use data have the same column names
colnames(s1net_f) <- colnames(s2net_f); colnames(s3net_f) <- colnames(s2net_f)

### the net number column is of character data type for survey 2-4 so convert to numeric
s2net_f <- s2net_f  %>%
  mutate(Qn131_NetNumber = as.numeric(Qn131_NetNumber))
s3net_f <- s3net_f  %>%
  mutate(Qn131_NetNumber = as.numeric(Qn131_NetNumber))
s4net_f <- s4net_f  %>%
  mutate(Qn131_NetNumber = as.numeric(Qn131_NetNumber))

### now bind the net usage data
temp7a <- bind_rows(s1net_f, s2net_f)
temp7b <- bind_rows(temp7a, s3net_f)
temp8 <- bind_rows(temp7b, s4net_f)

### sum the net use data at each stime points
temp9 <- temp8 %>%
  group_by(household, stime) %>%
  dplyr::summarise(slept_net = sum(Qn137_SleptUnderNet, na.rm = TRUE))

### almost final data at the household level
temp10 <- left_join(temp5, temp9, by = c("household","stime"))

### The Rdt/mic Rate will require that we use only those tested in the denominator not everyone in the household... so let's get that number

temp11 <- left_join(temp10,
                    merged_wgps %>%
                      rename(household = householdID,
                             stime = survey) %>%
                      count(household, stime) %>%
                      rename(hh_tested = n),
                    by = c("household","stime")
                    )

### Add in a couple of other predictor variables specified by CDC

household_lvl_data <- temp11 %>%
  mutate(oneNet = ifelse(!is.na(num_net), 1, 0), # at least one net in household
         num_net = ifelse(is.na(num_net), 0, num_net), # set number of net to 0 if NA
         num_hh_finals = as.numeric(num_hh_final), 
         Netptwo = ifelse(is.na(num_net), 0,  ifelse(num_net/num_hh_final >= 0.5, 1, 0)), # at least net coverage > 0.5
         netCov = ifelse(is.na(num_net),0, num_net/num_hh_final), # net coverage: # net/ total # of ppl in hh
         rdtRate = rdtPos_Sum/hh_tested, # rdt rate: total # rdt + in house / total # ppl tested in hh
         micRate = micPos_Sum/hh_tested, # mic rate: total # mic + in house / total # ppl tested in hh
         slept_net_p = slept_net/num_hh_final # proportion slept under net in household
  ) %>%
  # there is one row that is > 1 rdtrate
  mutate(rdtRate = ifelse(rdtRate > 1, 1, rdtRate),
         micRate = ifelse(micRate > 1, 1, micRate),
         # there is also > 1 netCov rate due to some people reporting more nets but less hh numb
         netCov = ifelse(netCov >= 1, 1, netCov)
  )

### Finally add in county information also

#- - - - - - - - - - - - - - - - - - - -#
### 5. County Information Data  ----
#- - - - - - - - - - - - - - - - - - - -#

### Merge the County information now 
temp <- merged_wgps %>%
  rename(household = householdID) %>%
  dplyr::select(household, county) %>%
  distinct(household, county) %>%
  mutate(household = as.numeric(household))

final_hh_data <- household_lvl_data %>%
  left_join(temp)


### Merge Altitude Information

#- - - - - - - - - - - - - - - - - - - -#
### 6. Altitude Data  ----
#- - - - - - - - - - - - - - - - - - - -#

# use stringr as done by A.F.'s code
library(stringr)

final <- final_hh_data %>%
  inner_join(merged %>%
               dplyr::select(householdID, altitude) %>%
               dplyr::rename(household = householdID) %>%
               dplyr::mutate(household = as.numeric(household)) %>%
               distinct(household, altitude))

final$Alt_type <- NA
final$Alt_type <- ifelse(str_detect(final$altitude, "m"), "Meter", NA)
final$Alt_type <- ifelse(str_detect(final$altitude, "ft"), "Feet", final$Alt_type)
sum(is.na(final$Alt_type))

# remove characters
final$altitude <- gsub("m", "", final$altitude)
final$altitude <- gsub("ft", "", final$altitude)

# convert all feet to meters
final$altitude <- as.numeric(final$altitude)
#hist(final$altitude)
#plot(final$altitude) # some crazy outliers way above
final$altitude <- ifelse(final$Alt_type=="Feet", final$altitude*0.3048, final$altitude)
#hist(final$altitude)
#plot(final$altitude) 
summary(final$altitude)
colMeans(is.na(final))




# Issue 1: We don't have all of the denominator (i.e. total number of people in household). So far the RDT+ dataset (i.e. df) has more people compared to the net survey data in 190-9 households and the netsurvey data (t1net_f) has households that have more people in 9 of the observations. We'll just use the max


#- - - - - - - - - - - - - - - - - - - -#
# 7. Greenness/Vegetation Add ----
#- - - - - - - - - - - - - - - - - - - -#

# Note: Here we want to add in greenness to each of the households...

hh_sdate <- merged %>%
  mutate(household = as.numeric(sub("HH*","",householdID)),
         time1 = as.Date(time, "%m/%d/%Y %H:%M:%S")) %>%
  select(household, time1) %>%
  distinct(household, time1)


# since some dates that are closeby don't quite have all the times but should be in the same time period we use last observation carried forward to fill them in
library(zoo)
hh_sdate_m <- na.locf(hh_sdate)

final <- final %>%
  left_join(hh_sdate_m)

# check if there are any missing date...
table(is.na(final$time1))
final <- final %>%
  rename(alt_srtm = altitude)

# save data
saveRDS(final, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS") # in the process folder
# using these dates we will merge in the greenness index.. let's check what is the max and min dates 

summary(final$time1)
times_dl <- final%>%
  ungroup() %>%
  filter(county == 2) %>%
  select(time1) %>%
  distinct(., time1) %>%
  unlist()

length(unique(final$time1)) # 43 unique dates...
# So we need to download the NOAA website that has the 37 files here: https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/

# Once we download the 37 files we parse them so that we have a dataframe with 3 columns (long, lat, NDVI). The Normalized Difference Vegetation Index (NVDI) is what we want. After we've made these... we can do a giant left join... or something similar in python.

#- - - - - - - - - - - - - - - - - - - -#
### 2. Table 1 Data
#- - - - - - - - - - - - - - - - - - - -#

#-- age info in merged_wgps dataframe
#tab1 <- final %>%
final %>%
    filter(county == 2) %>%
    left_join(merged_wgps %>%  # merge age from CleanS1S2S3S4...xslx file
              dplyr::select(householdID, age) %>% 
              rename(household = householdID)) 

#-- gender info from tbHouseholdMembers.xlsx file
#-- import tables
tbHH1 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey1/tbHouseholdMembers.xlsx", sheet =  1)
tbHH2 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey2/tbHouseholdMembers.xlsx", sheet =  1)
tbHH3 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey3/tbHouseholdMembers.xlsx", sheet =  1)
tbHH4 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey4/tbHouseholdMembers.xlsx", sheet =  1)

#-- merge tables together
tbHH <- rbindlist(
          tbHH4 %>%
              dplyr::select(household, Qn4_Gender),
          tbHH4 %>%
              dplyr::select(household, Qn4_Gender),
          tbHH4 %>%
              dplyr::select(household, Qn4_Gender),
          tbHH4 %>%
              dplyr::select(household, Qn4_Gender),
          idcol = TRUE
          )

#-- find percent of females in household
tbHH <- tbHH %>%
    group_by(household) %>%
    rename(gender = 2) %>%
    summarise(pct = mean(gender, na.rm=TRUE))

#-- merge all gender tabs with all other tabs
tab1 <- final %>%
    filter(county == 2) %>%
    left_join(merged_wgps %>%  # merge age from CleanS1S2S3S4...xslx file
              dplyr::select(householdID, age) %>% 
              rename(household = householdID) %>%
              group_by(household) %>%
              summarise(age = mean(age)))  %>% # take mean age of household
    left_join(tbHH)

library(knitr)
library(kableExtra)
library(moonBook)

tab1 <- final %>%
    filter(county == 2) %>%
    left_join(merged_wgps %>%  # merge age from CleanS1S2S3S4...xslx file
              dplyr::select(householdID, age) %>% 
              rename(household = householdID) %>%
              group_by(household) %>%
              summarise(age = mean(age))) %>%  # take mean age of household
    mutate(stime = ifelse(stime ==1, "Time 1", ifelse(stime ==2, "Time 2", ifelse(stime == 3, "Time 3", "Time 4"))))

#--labeling
library(Hmisc)
label(tab1[["age"]]) <- "Age mean (sd)"
label(tab1[["gender"]]) <- "Female n (%)"
label(tab1[["num_hh"]]) <- "People per HH mean(sd)"
label(tab1[["num_net"]]) <- "Nets per HH mean(sd) "
label(tab1[["netCov"]]) <- "Nets per person mean(sd) "
label(tab1[["Netptwo"]]) <- "Nets per two mean (sd)"
label(tab1[["hh_tested"]]) <- "Total sampled mean (sd)"
label(tab1[["micRate"]]) <- "Prop + by microscopy"
label(tab1[["rdtRate"]]) <- "Prop + by RDT"

saveRDS(tab1, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/table1.RDS") # in the process folder
#tab1 <- readRDS("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/table1.RDS") # in the process folder



#- - - - - - - - - - - - - - - - - - - -#
### 3. Magoro Map Data
#- - - - - - - - - - - - - - - - - - - -#
map <- get_map(location = c(lon = 34.1, lat = 1.75), zoom = 12, maptype = "hybrid")
saveRDS(map, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/map.RDS") # in the process folder


#- - - - - - - - - - - - - - - - - - - -#
### Ap1. Data Check  ----
#- - - - - - - - - - - - - - - - - - - -#



# See if the Long and Lat points correspond to areas in Uganda

### Since we have points data we would create a spatialpointsdataframe
library(rgdal)
library(sp)
df <- data.table(ungroup(final))
spdf <- SpatialPointsDataFrame(coords = df[,c("longitude","latitude")], data = df[,-c("latitude","longitude")], proj4string = CRS("+proj=longlat +ellps=WGS84"))

### check information regarding coordinates and etc
str(spdf) 


### Load the shp file from the [Institue for disease modeling](http://idmod.org/) 
### shapefile located in level above
#setwd("./data/PIL_shp")

### Check to see if the spatial points data are within the areas that are in the shp file.
#library(PBSmapping)
#plotPolys(importShapefile("pilgrim_villages"), col = "bisque2", colHoles = "blue", main = "Intervention in Uganda")
#plot(spdf, add= TRUE, col = "darkolivegreen1")


### Summary: It seems that the data set resides in the regions that we expect them to have the study conducted. CHECK!
