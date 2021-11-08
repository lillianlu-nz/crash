# LOAD LIBRRIES & RAW DATA ----
library(readr)
library(tidyverse)
library(epiDisplay)

# read file 
raw <- read.csv("./Crash_Analysis_System_(CAS)_data.csv")

# EDA
str(raw)
summary(raw)
head(raw)
nrow(raw)

# function to check all unique values in categorical/factor columns

checkunique <- function(df) {
  for (i in 1:ncol(df)) {
    if (class(df[, i]) == "character" | class(df[, i]) == "factor") {
      print(paste("-----------", colnames(df)[i], "-----------"))
      print(unique(df[, i]))
    }
  }
}

checkunique(raw)

# count crashSeverity 
raw %>% 
  group_by(crashSeverity) %>% 
  tally()

# DATA CLEANING ----

# random sampling to reduce df size
set.seed(42)
rand = sample(1:nrow(raw), size = nrow(raw)*.1, replace = F)

# make sure each year still has a representative sample size
table(raw$crashYear)

# temp = raw %>% group_by(crashYear) %>% 
#   tally() %>% 
#   cbind(crash %>% group_by(crashYear) %>% 
#           tally())

# cleaned dataset
crash <- raw %>%
  # keep sample only 
  slice(rand) %>% 
  
  dplyr::mutate(
    # add 'not holiday' label to holiday column
    holidayNew =
      case_when(nchar(holiday) == 0 ~ 'not holiday',
                TRUE ~ holiday),
    
    # convert categorical column to binary
    severeOrFatal =
      case_when(
        crashSeverity == "Non-Injury Crash" |
          crashSeverity == "Minor Crash" ~ F,
        crashSeverity == "Serious Crash" |
          crashSeverity == "Fatal Crash" ~ T
      ),
    
    # convert binary vehicle columns to categorical
    vehicleType =
      case_when(
        carStationWagon > 0 ~ "car",
        motorcycle > 0 ~ "motorcycle",
        bicycle > 0 ~ "bicycle",
        bus > 0 ~ "bus",
        truck > 0 ~ "truck",
        train > 0 ~ "train"
      ),
    
    # convert region into police districts
    policeDistrict = 
      case_when(
        region == "Waikato Region" ~ "Waikato",
        region == "ManawatÅ«-Whanganui Region" ~ "Central",
        region == "Auckland Region" ~ "Counties Manukau",
        region == "Wellington Region" ~ "Wellington",
        region == "Hawke's Bay Region" ~ "Eastern",
        region == "Northland Region" ~ "Northland",
        region == "Canterbury Region" ~ "Canterbury",
        region == "ManawatÅ«-Whanganui Region" | region == "Taranaki Region" ~ "Central",
        region == "Bay of Plenty Region" | region == "Gisborne Region" ~ "Bay of Plenty",
        region == "Southland Region" | region == "Otago Region" ~ "Southern",
        region == "Marlborough Region" | region == "Nelson Region" |
          region == "West Coast Region" | region == "Tasman Region" ~ "Tasman",
        TRUE ~ region
      ),

    # road type based on speed limit    
    roadType = 
      case_when(
        speedLimit <= 50 ~ "Urban Road",
        speedLimit >50 & speedLimit < 100 ~ "Arterial Road",
        speedLimit >= 100 ~ "Highway",
        TRUE ~ "Unknown")
    
    ) %>%
  
  # remove all NA values in vehicleType
  dplyr::filter(!is.na(vehicleType) & policeDistrict != "") %>% 
  
  # rename X column 
  dplyr::rename(X = 1) %>% 
  
  # select all wanted columns in order
  dplyr::select(X, Y, crashYear, holidayNew, region, policeDistrict, areaUnitID, crashLocation1,
                weatherA, vehicleType, light, roadSurface, roadType, speedLimit, 
                pedestrian, minorInjuryCount, fatalCount, severeOrFatal)

# covert all categorical vars to factors
for (i in 1:ncol(crash)) {
  if (class(crash[, i]) == "character") {
    crash[, i] = factor(crash[, i])
  }
}

# verify
summary(crash)

# verify numbers in severeOrFatal
# crash %>% group_by(severeOrFatal) %>% tally() 

# verify numbers in vehicleType
# crash %>% group_by(vehicleType) %>% tally()
# crash %>% 
#   filter(carStationWagon == 0 &
#                    motorcycle == 0 &
#                    bicycle == 0 &
#                    bus == 0 &
#                    truck == 0) %>% 
#   filter(train == 0 | is.na(train)) %>% nrow() # some columns contained NA such as train

# output data to CSV
write.csv(crash, "./crash_sample.csv", row.names = F)
