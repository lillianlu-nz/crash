shinyServer(function(input, output, session) {

# HOW TO TEXT ----
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = 'About'))
  })
  
# FILTERS ----
# filter YEAR & FATALITY
filter01_Year <- reactive({
    df %>%  filter(crashYear >= input$slider_year[1] &
                   crashYear <= input$slider_year[2] &
                       fatalCount >= input$slider_fatal &
                       pedestrian >= input$slider_pedestrian)
})
    
# filter DISTRICT
filter02_District <- reactive({
    if (input$select_district == 'All') {
        filter01_Year()
    } 
    else {
        filter01_Year() %>% 
            filter(policeDistrict %in% input$select_district)
    }
})

# filter SEVERITY
filter03_Severity <- reactive({
    if (input$select_severe == 'All') {
        filter02_District()
    } 
    else {
        filter02_District() %>% 
            filter(severeOrFatal %in% input$select_severe)
    }
})

# filter HOLIDAYS - FINAL
filter_final <- reactive({
    if (input$select_holidays == 'All') {
        filter03_Severity()
    } 
    else {
        filter03_Severity() %>% 
            filter(holidayNew %in% input$select_holidays)
    }
})

# LOCATION TAB ----

# info box

output$totalbox <- renderInfoBox({
    valueBox(filter_final() %>% nrow(), 
             "Total Crashes", color = "navy")
})    
  
output$severebox <- renderInfoBox({
    valueBox(filter_final() %>% filter(severeOrFatal == T) %>% nrow(), 
             "Severe/Fatal Crashes", color = "red")
})    

output$holidaybox <- renderInfoBox({
    valueBox(filter_final() %>% filter(holidayNew != "not holiday") %>% nrow(), 
    "Holiday Crashes", color = "yellow")
})

output$fatalbox <- renderInfoBox({
    valueBox(sum(filter_final()$fatalCount,na.rm = T), 
             "Fatalities", color = "red")
})

output$pedebox <- renderInfoBox({
    valueBox(sum(filter_final()$pedestrian,na.rm = T), 
             "Pedestrians Involved", color = "blue")
})

output$minorbox <- renderInfoBox({
    valueBox(sum(filter_final()$minorInjuryCount,na.rm = T), 
             "Minor Injuries", color = "blue")
})


# map - embedded solution

output$frame <- renderUI({
  crashmap <- tags$iframe(src="https://g7nd6z-lillian-lu.shinyapps.io/crashmap/", 
                          width = "100%",
                          style="height: 75vh;",
                          scrolling = 'no')
  print(crashmap)
  crashmap
})

#******************************
# map - working code - plotly #
#*****************************#
#
# map <- reactive({
#   filter_final() %>%
#     group_by(policeDistrict) %>%
#     tally() %>%
#     right_join(shp) %>%
#     dplyr::select(policeDistrict, geometry, n) %>%
#     ggplot(aes(geometry = geometry, fill = n, label = policeDistrict)) +
#     geom_sf() +
#     theme_minimal() +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           plot.title = element_text(size = 12)) +
#     labs(title = "Crash Count by Police Districts*") +
#     scale_fill_material("indigo")
#   
# })
# 
# output$mapplot <- renderPlotly({
#   ggplotly(
#   map()) %>% 
#     layout(hovermode = 'x')
# })
#
#**********************************************
# map - failed attempt - leaflet won't render #
#*********************************************#
#
# dfmap <- reactive({
#   dfshp = 
#   filter_final() %>%
#     group_by(policeDistrict) %>%
#     tally() %>%
#     right_join(shp) %>%
#     dplyr::select(policeDistrict, geometry, n) %>%
#     st_as_sf()
# })

# output$mapplot <- leaflet::renderLeaflet({
#   simplevis::leaflet_basemap(bounds = c(166.70047,-34.45676, 178.52966,-47.06345))
#   
#   simplevis::leaflet_sf_col(
#     st_zm(dfmap),
#     col_var = n,
#     label_var = policeDistrict,
#     col_method = "quantile",
#     col_cuts = seq(0, 1, 0.25),
#     title = "Crash Count by Police Districts"
#   )
# })

# dfmap = filter_final() %>%
#   group_by(policeDistrict) %>%
#   tally() %>%
#   right_join(shp) %>%
#   dplyr::select(policeDistrict, geometry, n) %>%
#   st_as_sf()

# simplevis::leaflet_sf_col(st_zm(dfmap),
#                           col_var = n,
#                           label_var = policeDistrict,
#                           col_method = "quantile",
#                           col_cuts = seq(0, 1, 0.25),
#                           title = "Crash Count by Police Districts")
# })

# severe crash trend
output$severetrend <- renderPlotly({
    ggplotly(
        filter_final() %>%
            group_by(crashYear, severeOrFatal) %>%
            tally() %>%
            ggplot(aes(x=crashYear, y=n, color = severeOrFatal)) +
            geom_line(size=1) +
            geom_point(size=2) +
            theme_minimal() +
            theme(axis.title = element_blank(),
                  panel.grid.minor = element_blank()) +
            labs(title = "Crash Trend by Severe/Fatal Crashes*") +
            # scale_colour_npg(name="Severe/Fatal Crashes") +
            scale_color_manual(name="Severe/Fatal Crashes",
                               values = wes_palette("Royal1", n = 2)) +
            scale_y_continuous(trans = "log2")) %>%
        layout(legend = list(orientation = "h", x = 0, y = -.12,
                             font = list(size = 9)))
})

# district prop
output$districtplot <- renderPlotly({
  
  ggplotly(
    filter_final() %>%
      group_by(policeDistrict, severeOrFatal) %>%
      summarise(count = length(policeDistrict)) %>% 
      mutate(percent = count/sum(count)) %>%
      ggplot(aes(x = policeDistrict, y = percent, fill = severeOrFatal)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(y=percent, label= count),colour = "white") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = "Severe/Fatal Crash Count & Proportion by Police Districts") +
      scale_fill_manual(name="Severe/Fatal Crashes",
                        values = wes_palette("Royal1", n = 2)))  %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.1,
                         font = list(size = 9)))
  
})

output$districtchi2 <- renderText({
    paste("Chi-square test showed a p-value of ", 
          chisq.test(table(filter_final()$severeOrFatal, filter_final()$policeDistrict))$p.value)
})

# district table

output$districttable <- DT::renderDataTable({
    filter_final() %>%
        group_by(crashYear, policeDistrict, severeOrFatal) %>% 
        tally() %>% 
        arrange(desc(crashYear, policeDistrict, severeOrFatal)) %>% 
        rename(`Year of Crash` = crashYear,
               `Police District` = policeDistrict,
               `Sever/Fatal Crashes` = severeOrFatal,
               Count = n)
},
options = list(paging = F, searching = F))


# CRASH TAB ----

# vehicle type
output$vehicleplot <- renderPlotly({
    ggplotly(
    filter_final() %>%
        group_by(vehicleType, severeOrFatal) %>%
        summarise(count = length(vehicleType)) %>%
        # mutate(percent = count/sum(count)) %>% 
        ggplot(aes(x = vehicleType, y = count, fill = severeOrFatal)) +
        geom_col(position = "fill") +
        # geom_text(aes(y=percent, label=count),  hjust = 2, colour = "black") +
        coord_flip() +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = "Severe/Fatal Crash Proportion by Vehicle Types") +
        scale_fill_brewer(name="Severe/Fatal Crashes", palette = "Paired")) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.1,
                             font = list(size = 9)))
})

output$vehiclechi2 <- renderText({
    paste("Chi-square test showed a p-value of ", 
          chisq.test(table(filter_final()$severeOrFatal, filter_final()$vehicleType))$p.value)
})

# vehicle crash trend
output$vehicletrend <- renderPlotly({
    ggplotly(
        filter_final() %>%
            group_by(crashYear, vehicleType) %>%
            tally() %>% 
            ggplot(aes(x=crashYear, y=n, color = vehicleType)) +
            geom_line(size=1) +
            geom_point(size=2) +
            theme_minimal() +
            theme(axis.title.x = element_blank(),
                  panel.grid.minor = element_blank()) +
            labs(title = "Crash Trend by Vehicle Types*") +
            ylab("log(scaled)") +
            scale_colour_npg(name="Vehicle Type") +
            scale_y_continuous(trans = "log2")) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.1,
                             font = list(size = 9)))
})

# road type (by speed limit)
output$roadplot <- renderPlotly({
    ggplotly(
    filter_final() %>%    
    group_by(roadType, severeOrFatal) %>% tally() %>%
        filter(roadType != "Unknown") %>% 
        ggplot(aes(x=roadType, y=n, fill=severeOrFatal)) +
        geom_col() +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = "Severe/Fatal Crash Count by Road Types") +
        scale_fill_manual(name="Severe/Fatal Crashes",
                          values = wes_palette("GrandBudapest1", n = 2)) +
        scale_y_continuous(labels = unit_format(unit ="K", scale=1e-3))) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.1,
                             font = list(size = 9)))
})

output$roadchi2 <- renderText({
    paste("Chi-square test showed a p-value of ", 
          chisq.test(table(filter_final()$severeOrFatal, filter_final()$roadType))$p.value)
})

# speed limit - continous
output$speedplot <- renderPlotly({
    mode_speed <- filter_final() %>%
        group_by(severeOrFatal) %>%
        summarize(mode=getmode(speedLimit))
    
    ggplot(filter_final()) +
        geom_density(aes(speedLimit, fill = severeOrFatal)) +
        geom_vline(data = mode_speed, aes(xintercept = mode), 
                   color = "gray35", linetype="dashed", size=.8) +
        facet_wrap(~severeOrFatal) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none") +
        labs(title = "Road Speed Limit by Severe/Fatal Crashes with Mode") +
        scale_fill_brewer(palette = "BrBG")
})

# weather
output$weatherplot <- renderPlotly({
    filter_final() %>% 
        filter(weatherA != "Null") %>% 
        group_by(weatherA, severeOrFatal) %>% tally() %>%
        # mutate(`Severe/Fatal Crashes` = factor(severeOrFatal,
        #                                        levels = c(TRUE, FALSE))) %>% 
        ggplot(aes(x=severeOrFatal, y=n, fill=severeOrFatal)) +
        geom_col() +
        facet_wrap(~weatherA) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing.y = unit(1, "lines"),
              legend.position = "none") +
        labs(title = "Severe/Fatal Crash Count by Weather Conditions") +
        ylab("log(scaled)") +
        # scale_fill_npg() +
        scale_fill_manual(name="Severe/Fatal Crashes",
                          values = wes_palette("Royal1", n = 2)) +
        scale_y_continuous(trans = "log2",
                           labels = unit_format(unit ="K", scale=1e-3))
})

output$weatherchi2 <- renderText({
    paste("Chi-square test showed a p-value of ", 
          chisq.test(table(filter_final()$severeOrFatal, filter_final()$weatherA))$p.value)
})

# weather prop
output$weatherprop <- renderPlotly({

ggplotly(
    filter_final() %>%
        group_by(weatherA, severeOrFatal) %>%
        summarise(count = length(weatherA)) %>% 
        mutate(percent = count/sum(count)) %>%
    ggplot(aes(x = weatherA, y = percent, fill = severeOrFatal)) +
    geom_col(position = "fill") +
    coord_flip() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Severe/Fatal Crash Proportion by Weather") +
    scale_fill_manual(name="Severe/Fatal Crashes",
                      values = wes_palette("Royal1", n = 2)))  %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.1,
                         font = list(size = 9)))

})

# light heatmap
output$lightheatmap <- renderPlotly({
    filter_final() %>% 
        group_by(light, severeOrFatal) %>% 
        tally() %>% 
        ggplot() +
        geom_tile(aes(x=light, y=severeOrFatal, fill = n)) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              axis.text.x=element_text(angle = 35)) +
        labs(title = "Heatmap of Light Conditions & Severe/Fatal Crashes") +
        scale_fill_material("teal")
})

output$lightchi2 <- renderText({
    paste("Chi-square test showed a p-value of ", 
          chisq.test(table(filter_final()$severeOrFatal, filter_final()$light))$p.value)
})

# light count
output$lightplot <- renderPlotly({
    filter_final() %>% 
        group_by(light) %>% tally() %>%
        filter(light != "Unknown") %>% 
        ggplot(aes(x=light, y=n, fill=light)) +
        geom_col() + coord_flip() +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none") +
        labs(title = "Crash Count by Light Conditions") +
        scale_fill_brewer(palette = "BrBG") +
        scale_y_continuous(labels = unit_format(unit ="K", scale=1e-3))
})

# PREDICT TAB ----

# REFLECTIVE ELEMENTS 
# weather
weatherHailorSleet <- reactive({
  if(input$pred_weather == "Fine" | input$pred_weather == "Heavy rain" |
     input$pred_weather == "Light rain" | input$pred_weather == "Mist or Fog" |
     input$pred_weather == "Snow"){
    0
  }
  else{
    1
  }
})

weatherHeavyRain <- reactive({
  if(input$pred_weather == "Fine" | input$pred_weather == "Hail or Sleet" |
     input$pred_weather == "Light rain" | input$pred_weather == "Mist or Fog" |
     input$pred_weather == "Snow"){
    0
  }
  else{
    1
  }
})

weatherLightRain <- reactive({
  if(input$pred_weather == "Fine" | input$pred_weather == "Hail or Sleet" |
     input$pred_weather == "Heavy rain" | input$pred_weather == "Mist or Fog" |
     input$pred_weather == "Snow"){
    0
  }
  else{
    1
  }
})

weatherMistorFog <- reactive({
  if(input$pred_weather == "Fine" | input$pred_weather == "Hail or Sleet" |
     input$pred_weather == "Heavy rain" | input$pred_weather == "Light rain" |
     input$pred_weather == "Snow"){
    0
  }
  else{
    1
  }
})

weatherSnow <- reactive({
  if(input$pred_weather == "Fine" | input$pred_weather == "Hail or Sleet" |
     input$pred_weather == "Heavy rain" | input$pred_weather == "Light rain" |
     input$pred_weather == "Mist or Fog"){
    0
  }
  else{
    1
  }
})

# vehicle

vehicleTypebus <- reactive({
  if(input$pred_vehicle == "bicycle" | input$pred_vehicle == "car" |
     input$pred_vehicle == "motorcycle" | input$pred_vehicle == "train" |
     input$pred_vehicle == "truck"){
    0
  }
  else{
    1
  }
})

vehicleTypecar <- reactive({
  if(input$pred_vehicle == "bicycle" | input$pred_vehicle == "bus" |
     input$pred_vehicle == "motorcycle" | input$pred_vehicle == "train" |
     input$pred_vehicle == "truck"){
    0
  }
  else{
    1
  }
})

vehicleTypemotorcycle <- reactive({
  if(input$pred_vehicle == "bicycle" | input$pred_vehicle == "bus" |
     input$pred_vehicle == "car" | input$pred_vehicle == "train" |
     input$pred_vehicle == "truck"){
    0
  }
  else{
    1
  }
})

vehicleTypetrain <- reactive({
  if(input$pred_vehicle == "bicycle" | input$pred_vehicle == "bus" |
     input$pred_vehicle == "car" | input$pred_vehicle == "motorcycle" |
     input$pred_vehicle == "truck"){
    0
  }
  else{
    1
  }
})

vehicleTypetruck <- reactive({
  if(input$pred_vehicle == "bicycle" | input$pred_vehicle == "bus" |
     input$pred_vehicle == "car" | input$pred_vehicle == "motorcycle" |
     input$pred_vehicle == "train"){
    0
  }
  else{
    1
  }
})

# light

lightDark <- reactive({
  if(input$pred_light == "Bright" | input$pred_light == "Overcast" |
     input$pred_light == "Twilight"){
    0
  }
  else{
    1
  }
})

lightOvercast <- reactive({
  if(input$pred_light == "Bright" | input$pred_light == "Dark" |
     input$pred_light == "Twilight"){
    0
  }
  else{
    1
  }
})

lightTwilight <- reactive({
  if(input$pred_light == "Bright" | input$pred_light == "Dark" |
     input$pred_light == "Overcast"){
    0
  }
  else{
    1
  }
})

# road

roadTypeHighway <- reactive({
  if(input$pred_road == "Arterial Road" | input$pred_road == "Urban Road"){
    0
  }
  else{
    1
  }
})

roadTypeUrban <- reactive({
  if(input$pred_road == "Arterial Road" | input$pred_road == "Highway"){
    0
  }
  else{
    1
  }
})

# model prediction
model_prob <- reactive({
  df = data.frame(pred = pred_fun(
    weatherHailorSleet(), weatherHeavyRain(), weatherLightRain(), weatherMistorFog(), weatherSnow(), 
     vehicleTypebus(), vehicleTypecar(), vehicleTypemotorcycle(), vehicleTypetrain(), vehicleTypetruck(),
     lightDark(), lightOvercast(), lightTwilight(), 
     roadTypeHighway(), roadTypeUrban(), input$pred_pede
  ))
})

# prediction label
model_class <- reactive({
  df = data.frame(class = factor(rbinom(2000, 1, model_prob()$pred),
                                 levels=c(1,0),labels = c(T, F)))
})

rval_model <- eventReactive(input$show_model, {

  percent = model_prob()$pred*100

  gghistogram(percent, y = "..density..",
              fill="slategray3", bins=40, add_density=TRUE) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank()) +
    scale_x_continuous(labels = number_format(accuracy = 1, suffix="%"), n.breaks=10) +
    ggtitle("Posterior Distribution of Severe/Fatal Crashes") +
    geom_vline(xintercept=median(percent), lwd=1, linetype=2, color="firebrick1") +
    geom_vline(xintercept=quantile(percent,0.025), lwd=.5, color="gray60") +
    geom_vline(xintercept=quantile(percent,0.975), lwd=.5, color="gray60")
})

rval_class <- eventReactive(input$show_model, {

  model_class() %>%
    group_by(class) %>%
    tally() %>%
    mutate(percent = n/2000*100) %>%
    ggplot() +
    geom_bar(aes(x=class, y=percent, group=class),
             fill=c("#da9046","#374e54"),stat="identity",width=.7) +
    scale_y_continuous(labels = number_format(accuracy = 1, suffix="%"), n.breaks=10) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank()) +
    ggtitle("Severe/Fatal Crash Prediction Class Distribution (random sampling)")
})

rval_text <- eventReactive(input$show_model, {
  paste(
    'This crash was<b>',
    round(median(model_prob()$pred*100),2),
    "% </b> likely to be severe or fatal.")
})

rval_summary <- eventReactive(input$show_model, {
  round(summary(model_prob()$pred),4)
})

rval_percentile <- eventReactive(input$show_model, {
  round(quantile(model_prob()$pred, c(0.025, 0.1, 0.5, 0.9, 0.975)),4)
})


# OUTPUT
# show model
observeEvent(input$show_info, {
  showModal(modalDialog(modelinfo, title = 'Model info'))
})

# text summary
output$modeltext <- renderText({
  rval_text()
})

output$summarytext <- renderPrint({
  rval_summary()
})

output$percentext <- renderPrint({
  rval_percentile()
})

# prediction plots
output$preddistplot <- renderPlotly({
  rval_model()
})    

output$predclassplot <- renderPlotly({
  rval_class()
})

# confusion matrix
output$confunsion <- DT::renderDataTable({
  confusion_matrix
}, 
  caption = "Confusion Matrix",  
  options = list(dom = 't'))

# ci table
output$ci95table <- DT::renderDataTable({
  ci95odds
},
caption = "Odds Ratio - 95% Credible Interval",  
options = list(paging = T, searching = T, pageLength = 5))


# 
# 
# 
# output$ci95table <- DT::renderDataTable({
#   ci95odds}, 
#   caption = "Odds Ratio - 95% Credible Interval",  
#   options = list(dom = 't', searching = T))

## SESSION ----
session$onSessionEnded(function() {
    stopApp()
})

})
