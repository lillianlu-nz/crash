# LIBARIES ----
library(tidyverse)
library(knitr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)
library(gridExtra)
library(scales)
library(ggpubr)
library(ggrepel)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(ggsci)
library(sf)
library(png)
library(wesanderson)
library(ggcorrplot)
library(DT)
library(shinycssloaders)
library(simplevis)
library(palmerpenguins)
library(leaflet)

# DATA ----
df = read.csv("./www/crash_sample.csv")
df_severe = read.csv("./www/crash_sample_04nov_severeonly.csv")
df_severe = df_severe %>% head(20)

# covert all categorical vars to factors
for (i in 1:ncol(df)) {
  if (class(df[, i]) == "character") {
    df[, i] = factor(df[, i])
  }
}

# relevel road type
df$roadType = factor(df$roadType, 
                      levels = c("Highway", "Arterial Road", "Urban Road", "Unknown"))

# relevel light
df$light = factor(df$light, 
                     levels = c("Dark", "Twilight", "Overcast", "Bright sun"))

# df$severeOrFatal = factor(df$severeOrFatal, 
#                      levels = c(TRUE, FALSE))

# convert NA to 0 for column pedestrian
df$pedestrian[is.na(df$pedestrian)] = 0

# verify
summary(df)

# list for filter
districtlist = as.character(unique(df$policeDistrict))
holidaylist = as.character(unique(df$holidayNew))

# get mode for speed limit
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(df$speedLimit)

# MAP ----
# load police district shape file
# shp = st_read("./www/map_files/nz-police-districts-clipped-to-coast-100m.shp")
# colnames(shp)[2] = "policeDistrict"
# shp$policeDistrict[shp$policeDistrict == "Counties/Manukau"] = "Counties Manukau"

# dfmap = df %>%
#   group_by(policeDistrict) %>%
#   tally() %>%
#   right_join(shp) %>%
#   dplyr::select(policeDistrict, geometry, n) %>%
#   st_as_sf()

# BAYESIAN ----
# load posterior sample
posterior = read.csv("./www/posterior_sample.csv")
posterior = posterior %>% dplyr::select(-1)

# function to calculate prediction prob in dashbaord
# to be improved
pred_fun <- function(weatherHailorSleet, weatherHeavyRain, weatherLightRain, weatherMistorFog, weatherSnow, 
                     vehicleTypebus, vehicleTypecar, vehicleTypemotorcycle, vehicleTypetrain, vehicleTypetruck,
                     lightDark, lightOvercast, lightTwilight, 
                     roadTypeHighway, roadTypeUrban, pedestrian){

  prob_logit = posterior$beta.1. + posterior$beta.2.* weatherHailorSleet + posterior$beta.3.* weatherHeavyRain +
    posterior$beta.4. * weatherLightRain + posterior$beta.5. * weatherMistorFog + posterior$beta.6. * weatherSnow +
    posterior$beta.7. * vehicleTypebus + posterior$beta.8. * vehicleTypecar + posterior$beta.9. * vehicleTypemotorcycle +
    posterior$beta.10. * vehicleTypetrain + posterior$beta.11. * vehicleTypetruck + posterior$beta.12. * lightDark +
    posterior$beta.13. * lightOvercast + posterior$beta.14. * lightTwilight + posterior$beta.15. * roadTypeHighway +    
    posterior$beta.16. * roadTypeUrban + posterior$beta.17. * pedestrian 

  prob_exp = exp(prob_logit)/(1+exp(prob_logit))
}

# load credible intervals
ci95table = read_csv("./www/ci95_1000.csv")
ci95odds = ci95table
ci95odds$`2.5%` = round(exp(ci95odds$`2.5%`),3)
ci95odds$`50%` = round(exp(ci95odds$`50%`),3)
ci95odds$`97.5%` = round(exp(ci95odds$`97.5%`),3)
colnames(ci95odds)[1] = "exp(coefficient)"

confusion_matrix = data.frame(Severe = c(8, 875), "Not Severe" = c(21, 12858))
rownames(confusion_matrix) = c("Severe pred", "Not Severe pred")

# HOW TO TEXT ----
text_about <- "This app is recreated with sample data downloaded from the Crash Analysis System (CAS) Open Data Portal:
https://opendata-nzta.opendata.arcgis.com/, containing 70010 observations in total. It aims to analyse the relationships between different crash 
characteristics and severe/fatal crashes.
Any filters applied will change the results in the Crash Locations and 
Driving Condition tabs. The Severity Prediction tab demonstrates the possibility of predicting severe or fatal crashes 
by the dataset, under the Bayesian approach. The model performance is to be improved. For error reporting or feedback 
please contact lillianlu.nz@gmail.com"
