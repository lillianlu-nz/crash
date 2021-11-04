library(shiny)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)
library(simplevis)
library(htmlTable)
library(htmltools)
library(leafpop)

severe = read_csv("crash_sample_04nov_severeonly.csv")

severe = severe %>% 
  dplyr::mutate(fatal =
           case_when(fatalCount > 0 ~ T,
                     TRUE ~ F)) %>% 
  dplyr::rename(`holiday` = holidayNew,
                weather = weatherA,
                crashLocation = crashLocation1) %>% 
  dplyr::select(-X, -Y, -severeOrFatal) 
  
# severe_sf = st_as_sf(severe, coords = c("LNG", "LAT"), crs = 2193)

ui <- bootstrapPage(
  
  title = "New Zealand COVID-19 Locations of Interest Map",
  #set theme
  theme = shinythemes::shinytheme('simplex'),
  
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  
  absolutePanel(top = 10, right=10, draggable =F, id = 'controls',
    checkboxInput("checkbox", "Show fatal crashes only", value = F)
  ),
  
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}
    #controls{background-color:white;opacity:0.8;padding:20px;}
  "),
  
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;opacity:0.8;padding:20px;}
  ")
)

server <- function(input, output, session) {
  
fatal_filter <- reactive({
  if (input$checkbox == F) {
    severe
  }
  else {
    severe %>% filter(fatal == T)
  }
})
  
output$map <- leaflet::renderLeaflet({
  
  fatal <- colorFactor(c("deepskyblue4","darkcyan"), domain = c(T, F))
  
  leaflet(fatal_filter()) %>%
    # addTiles() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      ~LNG,
      ~LAT,
      label = ~paste0("Year: ", crashYear, " Fatality: ", fatalCount, " - click to view details"),
      # fillColor = "darkcyan",
      radius = 10,
      color = ~ fatal(fatal),
      stroke = FALSE,
      fillOpacity = 0.5,
      popup = popupTable(severe[,], row.numbers = F)
    )
})
  
session$onSessionEnded(function() {
  stopApp()
})
}

shinyApp(ui, server)

# REFERENCE
# https://rstudio.github.io/leaflet/popups.html
# https://rstudio.github.io/leaflet/markers.html
# https://cran.r-project.org/web/packages/leafpop/leafpop.pdf
# https://github.com/r-spatial/leafpop