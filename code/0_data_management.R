# File Info ---------------------------------------------------------------
#### File Name:   0_data_management.R
#### Purpose:     Merge All Files
#### Created by:  CK
#### Created on:  1/26/2018
#### Modified by: CK
#### Modified on: 2/24/2020
#- - - - - - - - - - - - - - - - - - - -#

# WARNING: Run the 0_data_management.R file to get the necessary files for visualization and exploratory analysis here 

# 1. Load Lib ----------------------------------------------------------------
rm(list=ls())
library("pacman")
p_load(data.table,here,DescTools,tidyverse,
       readxl,janitor,naniar,skimr,stringr)

# 2. Manage RDT, PCR & Microscopy Data ------------------------------------
rawdiag <- fread(paste0(here(),"/data/raw/Microscopy/AllSurveysCleanPCR_20200211.csv"))
# Check missing data
gg_miss_var(rawdiag) 

# There are missing data for cleanPCR, cleanRDT and cleanMicroscopy. Since we are analyzing the data at the household level, we need to analyze data such that we have information on all household members.

table(rawdiag$dupPCRcode)
table(rawdiag$dupUID)
table(rawdiag$dupBarcode)

# Exclude people that have dupUID (dupUID == 1) and dupBarcode (dupBarcode == 1)
diag_nodup <- rawdiag %>%
  filter(dupUID == 0 & dupBarcode == 0) 

diag_nodup %>%
  tabyl(dupUID)
diag_nodup %>%
  tabyl(dupBarcode)

# Check if the cleanRDT, cleanPCR, & cleanMicroscopy are only within 0,1,NA
diag_nodup %>%
  tabyl(cleanRDT)
## There are 4 people that have cleanRDT = 6...
diag_nodup %>%
  tabyl(cleanPCR)
diag_nodup %>%
  tabyl(cleanMicroscopy)

# Set cleanRDT = NA for those with cleanRDT = 6
diag_nodup_mod1 <- diag_nodup %>%
  mutate(cleanRDT = ifelse(cleanRDT == 6, NA, cleanRDT))

diag_nodup_mod1 %>%
  tabyl(cleanRDT)
diag_nodup_mod1 %>%
  tabyl(cleanPCR)
diag_nodup_mod1 %>%
  tabyl(cleanMicroscopy)

# Generate Dataset with Denominator (N_RDT, N_PCR, N_MIC) as well as Numerator
diag_df <- diag_nodup_mod1 %>%
  dplyr::select(householdID, survey, cleanRDT) %>%
  group_by(householdID, survey) %>%
  filter(!is.na(cleanRDT)) %>%
  summarise(
    N_RDT = n(),
    sumRDT = sum(cleanRDT)
  ) %>%
  left_join(
    diag_nodup_mod1 %>%
      dplyr::select(householdID, survey, cleanPCR) %>%
      group_by(householdID, survey) %>%
      filter(!is.na(cleanPCR)) %>%
      summarise(
        N_PCR = n(),
        sumPCR = sum(cleanPCR)
      ) 
  ) %>%
  left_join(
    diag_nodup_mod1 %>%
      dplyr::select(householdID, survey, cleanMicroscopy) %>%
      group_by(householdID, survey) %>%
      filter(!is.na(cleanMicroscopy)) %>%
      summarise(
        N_MIC = n(),
        sumMIC = sum(cleanMicroscopy)
      ) 
  ) %>%
  ungroup() %>%
  arrange(survey, householdID)
  
diag_df %>%
  group_by(survey) %>%
  dplyr::select(sumRDT,sumPCR,sumMIC) %>%
  skim

rm(rawdiag,diag_nodup,diag_nodup_mod1)

# 3. Load Coordinates --------------------------------------------------------

diag_s5 <- read_csv(paste0(here(),"/data/raw/Microscopy/CleanS1S2S3S4S5rdtMicroscopyGPS_20190305.csv"))
coord_s6_kap <- read_csv(paste0(here(),"/data/raw/Survey6/kapujan.csv"))
coord_s6_mag <- read_csv(paste0(here(),"/data/raw/Survey6/magoro.csv"))
coord_s6_toroma <- read_csv(paste0(here(),"/data/raw/Survey6/toroma.csv"))

# Identify if altitude column as ft or m in it... if meter then just remove the m if ft then convert to meters
coord_s6 <- coord_s6_kap %>%
  bind_rows(coord_s6_mag, coord_s6_toroma) %>%
  mutate(
    householdID = as.numeric(gsub("HH","",name)),
    survey = 6
    ) %>%
  dplyr::select(householdID,survey,altitude,longitude,latitude) %>%
  mutate(Alt_Type = case_when(
    str_detect(altitude,"m") ~ "meter",
    str_detect(altitude,"ft") ~ "feet",
    TRUE ~ "meter"
  )) %>%
  mutate(
    altitude = as.numeric(case_when(
      Alt_Type == "meter" ~ gsub("m","",altitude),
      Alt_Type == "feet" ~ gsub("ft","",altitude),
      TRUE ~ altitude
    )
    )) %>%
  mutate(
    altitude = ifelse(
      Alt_Type == "feet", altitude*0.3048, altitude
    )
  ) %>%
  dplyr::select(-Alt_Type)


# Combine s12345 coordinates with s6
coord_df <- diag_s5 %>%
  dplyr::select(householdID,survey,altitude,longitude,latitude) %>%
  bind_rows(
    coord_s6
  ) 
rm(diag_s5,coord_s6,coord_s6_kap,coord_s6_mag,coord_s6_toroma)

diag_coord_df <- diag_df %>% 
  left_join(
    coord_df 
  ) %>%
  distinct(householdID,survey, .keep_all = TRUE) %>%
  arrange(survey, householdID)
rm(coord_df,diag_df)
# 4. Manage Net Usage Data ------------------------------------------------

# We need to still capture all people within the household in terms of the Net Usage

s1net <- read_excel(paste0(here(),"/data/raw/Survey1/tbl_HouseholdNets.xlsx"), sheet =  1)
s2net <- read_excel(paste0(here(),"/data/raw/Survey2/tbHouseholdNetsSurvey2.xlsx"), sheet =  1)
s3net <- read_excel(paste0(here(),"/data/raw/Survey3/HouseholdNets.xlsx"), sheet =  1)
s4net <- read_excel(paste0(here(),"/data/raw/Survey4/tbHouseholdNets.xlsx"), sheet =  1)
s5net <- read_excel(paste0(here(),"/data/raw/Survey5/tbHouseholdNets.xlsx"), sheet =  1)
s6net <- read_excel(paste0(here(),"/data/raw/Survey6/tbHouseholdNets survey 6.xlsx"), sheet =  1)

#### People need to standardize across each of the datasets in terms of the variable name and the information that is being gathered... surveys 1, 2, and 3 all have slightly different variable names...

table(AllDuplicated(paste0(s1net$autohhid2,'-',s1net$Qn131_netno))) # 46 duplicates
table(AllDuplicated(paste0(s2net$household,'-',s2net$tbl_HouseholdNets))) # no duplicates
table(AllDuplicated(paste0(s3net$household,'-',s3net$tbl_HouseholdNets))) # no duplicates
table(AllDuplicated(paste0(s4net$household,'-',s4net$tbl_HouseholdNets))) # no duplicates
table(AllDuplicated(paste0(s5net$household,'-',s5net$tbl_HouseholdNets))) # 2 duplicates
table(AllDuplicated(paste0(s6net$household,'-',s6net$tbl_HouseholdNets))) # 2 duplicates

### Since there are 46 + 2 duplicate net info we will only use the unique hh 
# In survey 1, the total number sleeping under the net has not been captured... Qn137_numslept. So we assume if Qn137_sleptundernet == 1, then everyone slept under net
s1net_f <- s1net %>%
  mutate(studyid = paste0(autohhid2, Qn131_netno),
         household = as.numeric(sub("HH","",s1net$autohhid2)),
         stime = 1)  %>%
  dplyr::select(household, Qn131_netno, Qn137_sleptundernet, stime)

s2net_f <- s2net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         stime = 2)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet, Qn137_numSlept, stime)

s3net_f <- s3net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         stime = 3)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet,Qn137_numSlept, stime)

s4net_f <- s4net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         household = as.numeric(household),
         stime = 4)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet,Qn137_numSlept, stime)

s5net_f <- s5net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         household = as.numeric(household),
         stime = 5)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet,Qn137_numSlept, stime)

s6net_f <- s6net %>%
  mutate(studyid = paste0(household, Qn131_NetNumber),
         household = as.numeric(household),
         stime = 6)  %>%
  # filter(!AllDuplicated(.[["studyid"]])) %>%
  dplyr::select(household, Qn131_NetNumber, Qn137_SleptUnderNet,Qn137_numSlept, stime)

 ### In each of the net datasets one can set up a filter to remove the duplicated values. Most of them are missing but if you remove them it's possible that some of them were sleeping under the net but didn't record the net number (for some reason...)

### Sum all net use 
temp2 <- s1net_f %>%
  # Select only the nets that were used 
  filter(Qn137_sleptundernet == 1) %>%
  mutate(net = 1) %>%
  group_by(household) %>%
  dplyr::summarise(num_net = sum(net)) %>%
  mutate(
    stime = 1,
    num_slept = NA
         )


### merge net use data from survey2
net_merged <- bind_rows(temp2, s2net_f %>%
                          mutate(net = 1) %>%
                          filter(Qn137_SleptUnderNet == 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(
                            num_net = sum(net),
                            num_slept = sum(Qn137_numSlept)
                            ) %>%
                          mutate(stime = 2))

### merge net use data from survey3
net_merged <- bind_rows(net_merged, s3net_f %>%
                          mutate(net = 1) %>%
                          filter(Qn137_SleptUnderNet == 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(
                            num_net = sum(net),
                            num_slept = sum(Qn137_numSlept)
                            ) %>%
                          mutate(stime = 3))

### merge net use data from survey4
net_merged <- bind_rows(net_merged, s4net_f %>%
                          mutate(net = 1) %>%
                          filter(Qn137_SleptUnderNet == 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(
                            num_net = sum(net),
                            num_slept = sum(Qn137_numSlept)
                            ) %>%
                          mutate(stime = 4))

### merge net use data from survey5
net_merged <- bind_rows(net_merged, s5net_f %>%
                          mutate(net = 1) %>%
                          filter(Qn137_SleptUnderNet == 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(
                            num_net = sum(net),
                            num_slept = sum(Qn137_numSlept)
                            ) %>%
                          mutate(stime = 5))

### merge net use data from survey6
net_merged <- bind_rows(net_merged, s6net_f %>%
                          mutate(net = 1) %>%
                          filter(Qn137_SleptUnderNet == 1) %>%
                          group_by(household) %>%
                          dplyr::summarise(
                            num_net = sum(net),
                            num_slept = sum(Qn137_numSlept)
                            ) %>%
                          mutate(stime = 6))

### now join with the gps data
diag_coord_net_df <- left_join(
  diag_coord_df %>%
    rename(household=1, stime = 2),
  net_merged
)
rm(temp2,s1net,s1net_f,s2net,s2net_f,s3net,s3net_f,s4net,s4net_f,s5net,s5net_f,s6net,s6net_f)


# 5. Household Survey Data ------------------------------------------------

s1hhd <- read_excel(paste0(here(),"/data/raw//Survey1/tbl_Household_Survey.xlsx"), sheet = 1)
s2hhd <- read_excel(paste0(here(),"/data/raw/Survey2/tbHousehold_Survey2.xlsx"), sheet =  1)
s3hhd <- read_excel(paste0(here(),"/data/raw/Survey3/Household_Survey.xlsx"), sheet =  1)
s4hhd <- read_excel(paste0(here(),"/data/raw/Survey4/tbHousehold_Survey.xlsx"), sheet =  1)
s5hhd <- read_excel(paste0(here(),"/data/raw/Survey5/tbHousehold_Survey.xlsx"), sheet =  1)
s6hhd <- read_excel(paste0(here(),"/data/raw/Survey6/tbHousehold_Survey 6.xlsx"), sheet =  1)

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
                     stime = 4) ) %>%
  bind_rows(., s5hhd %>%
              dplyr::select(household, Qn10_TotalPersonsInHH) %>%
              dplyr::rename(num_hh = Qn10_TotalPersonsInHH) %>%
              mutate(num_hh = as.numeric(num_hh),
                     household = as.numeric(household),
                     stime = 5) ) %>%
  bind_rows(., s6hhd %>%
              dplyr::select(household, Qn10_TotalPersonsInHH) %>%
              dplyr::rename(num_hh = Qn10_TotalPersonsInHH) %>%
              mutate(num_hh = as.numeric(num_hh),
                     household = as.numeric(household),
                     stime = 6) )



### Join the 2nd houshold survey that has total number of households
diag_coord_net_thh_df <- diag_coord_net_df %>%
  left_join(hhtn) 


### Check number missing
table(is.na(diag_coord_net_thh_df$num_hh)) # 6 households with missing

### Average number in HH by county
s6hhd %>%
  group_by(Scounty) %>%
  dplyr::summarise(num_hh = mean(as.numeric(Qn10_TotalPersonsInHH), na.rm=TRUE))

### For those that are missing the total number in household... we'll see that each household will have less number in household due to the way that the survey was collected... so we'll use the max by each household as the number in the household. Also make sure that if there are any NA's then just choose the non-NA value.

diag_coord_net_thh_mod_df <- diag_coord_net_thh_df %>%
  mutate(num_hh_final = ifelse(is.na(num_hh), 
                               # so if there is `NA` then we need to use the num_net or rdtPos_Sum 
                               ifelse(is.na(num_slept) & is.na(N_RDT), NA, 
                                      ifelse(is.na(num_slept), N_RDT, 
                                             ifelse(is.na(N_RDT), num_slept, 
                                                    ifelse(N_RDT > num_slept, N_RDT, num_slept)))
                               ), num_hh)) %>%
  # Since there is one household that indicated 0 in household... wtf?
  mutate(num_hh_final = ifelse(num_hh_final == 0, 1, num_hh_final))


rm(s1net,s1net_f,s2net,s2net_f,s3net,s3net_f,s4net,s4net_f,s5net,s5net_f,s6net,s6net_f)

# 6. Other predictor variables --------------------------------------------


### Add in a couple of other predictor variables specified by CDC

household_lvl_data <- diag_coord_net_thh_mod_df %>%
  mutate(
    oneNet = ifelse(!is.na(num_net), 1, 0), # at least one net in household
    # num_net = ifelse(is.na(num_net), 0, num_net), # set number of net to 0 if NA
    Netptwo = ifelse(is.na(num_net), 0,  ifelse(num_net/num_hh_final >= 0.5, 1, 0)), # at least net coverage > 0.5
    netCov = ifelse(is.na(num_net),0, num_net/num_hh_final), # net coverage: # net/ total # of ppl in hh
    rdtRate = sumRDT/N_RDT , # rdt rate: total # rdt + in house / total # ppl tested in hh
    micRate = sumMIC/N_MIC, # mic rate: total # mic + in house / total # ppl tested in hh
    # If num_slept greater than num_hh then we set it to num_hh
    num_slept = ifelse(num_slept > num_hh_final,num_hh_final,num_slept),
    slept_net_p = num_slept/num_hh_final # proportion slept under net in household
  ) %>%
  # there is one row that is > 1 rdtrate
  mutate(rdtRate = ifelse(rdtRate > 1, 1, rdtRate),
         micRate = ifelse(micRate > 1, 1, micRate),
         # there is also > 1 netCov rate due to some people reporting more nets but less hh numb
         netCov = ifelse(netCov >= 1, 1, netCov)
  )
rm(hhtn,diag_coord_net_thh_mod_df,s1hhd,s2hhd,s3hhd,s4hhd,s5hhd,s6hhd)

# 7. County level info ----------------------------------------------------
rawdiag <- fread(paste0(here(),"/data/raw/Microscopy/AllSurveysCleanPCR_20200211.csv"))
### Merge the County information now 
countyinfo <- rawdiag %>%
  dplyr::select(householdID,subcounty,village) %>%
  rename(
    household = householdID,
    county = subcounty
    ) %>%
  dplyr::select(household, county) %>%
  distinct(household, county) %>%
  mutate(household = as.numeric(household))

final_hh_data <- household_lvl_data %>%
  left_join(countyinfo)

rm(rawdiag,countyinfo,household_lvl_data)


# 8. Greenness/Vegetation Value -------------------------------------------
# Note: Here we want to add in greenness to each of the households...
# First need to identify dates interview was conducted for the household 

df_interviewtime <- s1hhd %>%
  dplyr::select(hhid, interview_date) %>%
  mutate(household = as.numeric(substr(sub("HH","", hhid), 1, 9))) %>%
  mutate(
    DateOfInterview = as.Date(interview_date, "%b %d, %Y"),
    stime = 1
  ) %>%
  dplyr::select(household, DateOfInterview, stime) %>%
  bind_rows(
    s2hhd %>%
      dplyr::select(household, DateOfInterview) %>%
      mutate(DateOfInterview= as.Date(DateOfInterview, "%d/%m/%Y"),
             stime = 2) 
  ) %>%
  bind_rows(
    s3hhd %>%
      dplyr::select(household, DateOfInterview) %>%
      mutate(DateOfInterview= as.Date(DateOfInterview, "%d/%m/%Y"),
             stime = 3) 
  ) %>%
  bind_rows(
    s4hhd %>%
      dplyr::select(household, DateOfInterview) %>%
      mutate(
        household = as.numeric(household),
        DateOfInterview= as.Date(DateOfInterview, "%d/%m/%Y"),
        stime = 4) 
  ) %>%
  bind_rows(
    s5hhd %>%
      dplyr::select(household, DateOfInterview) %>%
      mutate(DateOfInterview= as.Date(DateOfInterview, "%d/%m/%Y", tz="MST"),
             stime = 5) 
  ) %>%
  bind_rows(
    s6hhd %>%
      dplyr::select(household, DateOfInterview) %>%
      mutate(
        household = as.numeric(household),
        DateOfInterview= as.Date(DateOfInterview, "%d/%m/%Y", tz="MST"),
        stime = 6)  
  ) 

hh_sdate <- final_hh_data %>%
  left_join(
    df_interviewtime 
  ) %>%
  select(household, stime, DateOfInterview) %>%
  rename(household = 1, time1 = 3)  %>%
  distinct(household, stime, time1) 

# Check anomalous time
summary(hh_sdate$time1)
# There clearly are times that are erroneous 
# Survey times for Survey 1 should be time >= 10-01-2016
# Survey times for Survey 2 should be time >= 06-01-2016
# Survey times for Survey 3 should be time >= 06-01-2016

hh_sdate %>%
  filter(time1 < "2016-10-01") %>%
  group_by(stime) %>%
  summarise(
    mt = mean(time1),
    n = n()
    )

# set the above values to NA
hh_sdate_mod <- hh_sdate %>%
  mutate(time1 = as.Date(ifelse(
    time1 < "2016-10-01",NA,time1
    ), origin="1970-01-01")) 

# 3 dates with missing interview time... take average of the stimes
avgstimedf <- hh_sdate_mod %>%
  group_by(stime) %>%
  summarise(avgstime = mean(time1, na.rm=TRUE))

# merge avgstime with hh_sdate_mod
hh_sdate_fin <- hh_sdate_mod %>%
  left_join(avgstimedf) %>%
  mutate(time1 = as.Date(ifelse(is.na(time1), avgstime, time1), origin="1970-01-01")) %>%
  distinct(household, stime, .keep_all = TRUE)

finalt <- final_hh_data %>%
  left_join(hh_sdate_fin)

# check if there are any missing date...
table(is.na(finalt$time1))
finalta <- finalt %>%
  rename(alt_srtm = altitude)

# save data
saveRDS(finalta, paste0(here(),"/data/process/final_hh_df.RDS")) # in the process folder
# using these dates we will merge in the greenness index.. let's check what is the max and min dates 

summary(finalta$time1)
times_dl <- finalta%>%
  ungroup() %>%
  select(time1) %>%
  distinct(., time1) %>%
  unlist()

length(times_dl) # 116 unique dates...
# So we need to download the NOAA website that has the 116 files here: https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/

# Once we download the environmental data files we parse them so that we have a dataframe with 3 columns (long, lat, NDVI). The Normalized Difference Vegetation Index (NVDI) is what we want. After we've made these... we can do a giant left join... or something similar in python.

# More processing in code/1_extract_env_vars.R file 







































#-- just incase we need to modify the data again
final_hh_df <- readRDS("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS") # in the process folder

final_hh_df <- final_hh_df %>%
    select(-contains("tested"), -contains("Rate")) %>%
    left_join(
              final %>%
                  select(household, stime, contains("tested"), contains("Rate"))
              ) 

saveRDS(final_hh_df, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS") # in the process folder
#- - - - - - - - - - - - - - - - - - - -#
# 8. Scale and Standardize Temp and EVI
#- - - - - - - - - - - - - - - - - - - -#

final <- readRDS("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS") # in the process folder

final <- final %>%
    mutate_at(vars(matches("EVI"), matches("tempK")), scale)
saveRDS(final, "/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process/final_hh_df.RDS") # in the process folder

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
#tbHH1 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey1/tbHouseholdMembers.xlsx", sheet =  1)
#tbHH2 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey2/tbHouseholdMembers.xlsx", sheet =  1)
#tbHH3 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey3/tbHouseholdMembers.xlsx", sheet =  1)
#tbHH4 <- read_excel("~/Documents/Projects/DrColborn/UgandaNets/data/raw/Survey4/tbHouseholdMembers.xlsx", sheet =  1)

#-- merge tables together
#tbHH <- rbindlist(
          #tbHH4 %>%
              #dplyr::select(household, Qn4_Gender),
          #tbHH4 %>%
              #dplyr::select(household, Qn4_Gender),
          #tbHH4 %>%
              #dplyr::select(household, Qn4_Gender),
          #tbHH4 %>%
              #dplyr::select(household, Qn4_Gender),
          #idcol = TRUE
          #)

#-- find percent of females in household
#tbHH <- tbHH %>%
    #group_by(household) %>%
    #rename(gender = 2) %>%
    #summarise(pct = mean(gender, na.rm=TRUE))

#-- merge all gender tabs with all other tabs
#tab1 <- final %>%
    #filter(county == 2) %>%
    #left_join(merged_wgps %>%  # merge age from CleanS1S2S3S4...xslx file
              #dplyr::select(householdID, age) %>% 
              #rename(household = householdID) %>%
              #group_by(household) %>%
              #summarise(age = mean(age)))  %>% # take mean age of household
    #left_join(tbHH)

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
#label(tab1[["gender"]]) <- "Female n (%)"
label(tab1[["num_hh"]]) <- "People per HH mean(sd)"
label(tab1[["num_net"]]) <- "Nets per HH mean(sd) "
label(tab1[["netCov"]]) <- "Nets per person mean(sd) "
label(tab1[["Netptwo"]]) <- "Nets per two mean (sd)"
label(tab1[["hh_tested"]]) <- "Total sampled mean (sd)"
label(tab1[["micRate"]]) <- "Proportion + by microscopy"
label(tab1[["rdtRate"]]) <- "Proportion + by RDT"
label(tab1[["micPos_tested"]]) <- "MIC tested per HH mean(sd)"
label(tab1[["rdtPos_tested"]]) <- "RDT tested per HH mean(sd)"

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
