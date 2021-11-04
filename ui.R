# SIDE BAR ----

sidebar <- dashboardSidebar(
    width = 250,
    sidebarMenu(
        menuItem(
            "Crash Locations",
            tabName = "location",
            icon = icon("map-marked-alt")),
        menuItem("Driving Conditions",
                 tabName = "crash",
                 icon = icon("car")),
        menuItem(
            "Severity Prediction",
            tabName = "predict",
            icon = icon("chart-line")
        )
        ,
        sliderInput(
            "slider_year",
            "Year range",
            min = min(df$crashYear),
            max = max(df$crashYear),
            value = c(max(df$crashYear) - 10, max(df$crashYear)),
            step = 1
        ),
        sliderInput(
            "slider_fatal",
            "Fatalities >=",
            min = min(df$fatalCount, na.rm = T),
            max = max(df$fatalCount, na.rm = T),
            value = 0,
            step = 1
        ),
        sliderInput(
            "slider_pedestrian",
            "Pedestrians involved >=",
            min = min(df$pedestrian, na.rm = T),
            max = max(df$pedestrian, na.rm = T),
            value = 0,
            step = 1
        ),
        selectInput(
            "select_district",
            "Police district",
            choice = c(districtlist,
                       "All"),
            multiple = F,
            selected = "All"
        ),
        selectInput(
            "select_holidays",
            "Holidays",
            choice = c(holidaylist,
                       "All"),
            multiple = F,
            selected = "All"
        ),
        selectInput(
            "select_severe",
            "Severe or fatal crashes",
            choices = list(TRUE, FALSE, "All"),
            selected = "All"
        ),
        actionButton('show_about', 'About this app')
    )
)

# BODY HEAD ----
body <- dashboardBody(tabItems(

# LOCATION TAB ----   
tabItem(tabName = "location",
        fluidRow(column(
            width = 5,
                        valueBoxOutput(width = 4, "totalbox"),
                        valueBoxOutput(width = 4, "holidaybox"),
                        valueBoxOutput(width = 4, "severebox"),
                        valueBoxOutput(width = 4, "pedebox"),
                        valueBoxOutput(width = 4, "minorbox"),
                        valueBoxOutput(width = 4, "fatalbox"),
                        box(
                            height = "850px",
                            width = 12,
                            h3("Map of All Severe Crashes 2000-2021"),
                            p("Sorry, filters on the left-hand side are not applicable here."),
                            htmlOutput("frame") %>% withSpinner()
                        )
                    ),
                 column(
                     width = 7,
                     box(width = 12,
                         plotlyOutput("severetrend", height = 200) %>% withSpinner(),
                         p("*Crash record of 2021 is not complete")),
                     box(width = 12,
                         plotlyOutput("districtplot", height = 350) %>% withSpinner(),
                         htmlOutput("districtchi2")),
                     box(
                         width = 12,
                         h4("Crash data by Year, Police District and Severe/Fatal Crashes"),
                         DT::dataTableOutput("districttable"),
                         style = "height:405px; overflow-y: scroll;"
                     )
                 ))
),
    
# CRASH TAB ----
tabItem(tabName = "crash",
        fluidRow(
            tabBox(
                width = 6,
                id = "vehicle",
                height = "450px",
                tabPanel("Vehicle Type", box(
                    width = 12, plotlyOutput("vehicleplot", height = 320) %>% withSpinner(),
                    htmlOutput("vehiclechi2")
                )),
                tabPanel("Crash Trend", box(width = 12, plotlyOutput(
                    "vehicletrend", height = 320) %>% withSpinner(),
                    p("*Crash record of 2021 is not complete")))
            ), 
            tabBox(
                width = 6,
                id = "road",
                height = "450px",
                tabPanel("Road Type", box(
                    width = 12, plotlyOutput("roadplot", height = 320) %>% withSpinner(),
                    htmlOutput("roadchi2")
                )),
                tabPanel("Speed Limit", box(
                    width = 12, plotlyOutput("speedplot", height = 350) %>% withSpinner()
                ))
            )),
        fluidRow(
            tabBox(
                width = 6,
                id = "weather",
                height = "450px",
                tabPanel("Weather Conditions", box(
                    width = 12, plotlyOutput("weatherplot", height = 320) %>% withSpinner(),
                    htmlOutput("weatherchi2")
                )),
                tabPanel("Proportion", box(
                    width = 12, plotlyOutput("weatherprop", height = 350) %>% withSpinner()
                ))
            ),
            tabBox(
                width = 6,
                id = "light",
                height = "450px",
                tabPanel("Light Conditions", 
                         box(width = 12, plotlyOutput("lightheatmap", height = 320) %>% withSpinner(),
                             htmlOutput("lightchi2"))
                         ),
                tabPanel("Crash Count", box(
                    width = 12, plotlyOutput("lightplot", height = 350) %>% withSpinner()
                ))
            ))
        ),



# PREDICTION TAB ----
tabItem(tabName = "predict",
        fluidRow(
            box(
                width = 3, background = "navy",
                h1("Severe/Fatal Crash Prediction Modelling"),
                p('This section demonstrates the machine learning application of the New Zealand 
                Crash Analysis System (CAS) sample data 2000-2021.
            Select desired information on the right and hit "Show Model Result". 
            The model will provide a probability distribution on how likely the crash was severe or fatal 
            based on the corresponding data.'),
                actionButton("show_model", "Show Model Result"),
                br(), br(),
                fileInput("file", "Upload your own data (not available)")
                ),
            box(
                title = "Weather & Light",width = 3,
                selectInput("pred_weather", "What was the weather?",
                             choices = list("Fine", "Heavy rain", "Light rain" ,
                                            "Hail or Sleet", "Mist or Fog", "Snow" ),selected = "Fine"),
                radioButtons("pred_light", "What was the condition of natural lighting?",
                            choices = list("Bright sun", "Overcast", "Twilight", "Dark" ),selected = "Bright sun")
                ),
            box(
                title = "Road & Vehicle",width = 3,
                selectInput("pred_road", "What was the road type?",
                            choices = list("Highway", "Arterial Road", "Urban Road"),selected = "Highway"),
                selectInput("pred_vehicle", "What was the vehicle type?",
                            choices = list("car", "motorcycle", "train", "truck" ,
                                           "bicycle", "bus"),selected = "car"),
                selectInput("pred_svm", span("Was this a single or multiple vehicle crash? (N.A.)",style = "color:lightgray"),
                            choices = list("Single", "Multiple"), selected = "Multiple")
                ),
            box(
                title = "Other Info",width = 3,
                numericInput("pred_pede", "Were there any pedestrians involved? How many?",
                             value = 0),
                selectInput("pred_hol", span("Did the crash happen on a public holiday? (N.A.)",style = "color:lightgray"),
                            choices = list("Yes", "No"), selected = "No"),
                selectInput("pred_district", span("Where did the crash happen? (N.A.)",style = "color:lightgray"),
                            choices = list("Counties Manukau", "Wellington", "Tasman",
                                           "Canterbury","Southern"), selected = "Counties Manukau")
            )
        ),
        fluidRow(
            br(),
            htmlOutput("modeltext"),
            tags$style(type="text/css", "#modeltext { text-align:center; color: navy; 
                   font-size: 30px; display: block;}"),
            br()
        ),
        fluidRow(
            verbatimTextOutput("summarytext"),
            verbatimTextOutput("percentext"),
            tags$style(type="text/css", "#summarytext { text-align:center; color: black; 
                   font-size: 15px; background-color: transparent;}"),
            tags$style(type="text/css", "#percentext { text-align:center; color: black; 
                   font-size: 15px; background-color: transparent;}"),
            br()
        ),
        fluidRow(
            box(width = 6, plotlyOutput("preddistplot", height = 400) %>% withSpinner()),
            box(width = 6, plotlyOutput("predclassplot", height = 400) %>% withSpinner())
        ),
        fluidRow(
            br(),
            strong(span("About the Bayesian model", style = "text-align:center; color: navy;
                   font-size: 30px; display: block;")),
            
            
            span(p("This section provides the techincal details of the parsimonious Bayesian model."),
                 style = "text-align:center;font-size: 16px; color: navy;"),
            
            box(width=12,
                span(p(strong("Introduction:"), "This Bayesian JAGS parsimonious (reduced) model was derived based on the 95% credible 
        intervals of the coefficients (the log odds range of severe/fatal crashes for each predictor, which was converted to odds ratio in the table below). 
        The significant predictors included weather, light, road type, vehicle type, and number of pedestrians. 
        The MCMC process of the model had 5000 iterations with a thinning factor of 5. Thus, there were a total of 1,000 posterior samples. 
        Not all of the coefficients had over 200 effective samples due to computational power; this is believed to greatly influence the model's current 
        performance. The prediction class in the bar chart above was generated with rbinom() based on the posterior probability distribution.
               "), style = "font-size: 16px;"),
                
                span(p(strong("Model formula:")), style = "font-size: 16px;"),
                
                withMathJax(),
                p('$$Logit(p) = ~\\beta~_1 + ~\\beta~_2 * WeatherHail + ~\\beta~_3 * WeatherHeavyRain +
                ~\\beta~_4 * WeatherLightRain + ~\\beta~_5 * WeatherMist + ~\\beta~_6 * WeatherSnow +
                ~\\beta~_7 * VehicleBus + ~\\beta~_8 * VehicleCar + ~\\beta~_9 * VehicleMotorcycle + $$',style="color:black"),
            
                withMathJax(),
                p('$$ ~\\beta~_{10} * VehicleTrain + ~\\beta~_{11} * VehicleTruck +
                ~\\beta~_{12} * LightDark + ~\\beta~_{13} * LightOvercast + ~\\beta~_{14} * LightTwilight +
                ~\\beta~_{15} * RoadHighway + ~\\beta~_{16} * RoadUrban + ~\\beta~_{17} * Pedestrian $$',style="color:black"),
                
                span(p(strong("Priors (in JAGS code):"), "beta[j] ~ dnorm(0, 1/1000^2)"), style = "font-size: 16px;"),
                
                span(p(strong("MCMC iterations:"), "5,000"), style = "font-size: 16px;"),
                
                span(p(strong("Burn-in period:"), "none"), style = "font-size: 16px;"),
                
                span(p(strong("Thinning factor:"), "5"), style = "font-size: 16px;"),

                span(p("Refer to the tables and charts below for the", strong("confusion matrix, 95% credible intervals,
                                                                      and density and trace plots"), "of the model."), 
                     style = "font-size: 16px;")),
            br()
        ),
        fluidRow(
            column(width = 5,
                   box(width = 12, DT::dataTableOutput("confunsion"),
                       br(),
                       column(width = 6,
                              p(strong("Model accuracy:"), "93.5%"),
                              p(strong("True positive rate:"), "0.9%")),
                              p(strong("True negative rate:"), "99.8%")),
                   box(width = 12, DT::dataTableOutput("ci95table"))),
            
            column(width = 7,
                   tabBox(width = 12,
                          id = "predicttab", height = "580px",
                          tabPanel("Density Plot",tags$img(src = "densityplot.png", height=550)),
                          tabPanel("Trace Plot",tags$img(src = "traceplot.png", height=550))
                   ))
        )
        )

# BODY TAIL
))

# EVERYTHING ----

# Put them together into a dashboardPage
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Severe and Fatal Crash Analysis",
                    titleWidth = 350),
    sidebar,
    body
)
