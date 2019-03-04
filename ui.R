library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)

monthChoices <- list("January 2014" = "2014-01", "February 2014" = "2014-02", "March 2014" = "2014-03",
                     "April 2014" = "2014-04", "May 2014" = "2014-05", "June 2014" = "2014-06",
                     "July 2014" = "2014-07", "August 2014" = "2014-08", "September 2014" = "2014-09",
                     "October 2014" = "2014-10", "November 2014" = "2014-11", "December 2014" = "2014-12",
                     "January 2015" = "2015-01", "February 2015" = "2015-02", "March 2015" = "2015-03",
                     "April 2015" = "2015-04", "May 2015" = "2015-05", "June 2015" = "2015-06",
                     "July 2015" = "2015-07", "August 2015" = "2015-08", "September 2015" = "2015-09",
                     "October 2015" = "2015-10", "November 2015" = "2015-11", "December 2015" = "2015-12",
                     "January 2016" = "2016-01", "February 2016" = "2016-02", "March 2016" = "2016-03",
                     "April 2016" = "2016-04", "May 2016" = "2016-05", "June 2016" = "2016-06",
                     "July 2016" = "2016-07", "August 2016" = "2016-08", "September 2016" = "2016-09",
                     "October 2016" = "2016-10", "November 2016" = "2016-11", "December 2016" = "2016-12",
                     "January 2017" = "2017-01", "February 2017" = "2017-02", "March 2017" = "2017-03",
                     "April 2017" = "2017-04", "May 2017" = "2017-05", "June 2017" = "2017-06",
                     "July 2017" = "2017-07", "August 2017" = "2017-08", "September 2017" = "2017-09",
                     "October 2017" = "2017-10", "November 2017" = "2017-11", "December 2017" = "2017-12",
                     "January 2018" = "2018-01", "February 2018" = "2018-02", "March 2018" = "2018-03",
                     "April 2018" = "2018-04", "May 2018" = "2018-05", "June 2018" = "2018-06",
                     "July 2018" = "2018-07", "August 2018" = "2018-08", "September 2018" = "2018-09",
                     "October 2018" = "2018-10", "November 2018" = "2018-11", "December 2018" = "2018-12")

header <- dashboardHeader(title="London Crime Map",
                          tags$li(class = "dropdown", tags$a("Data source: data.police.uk", href ="https://data.police.uk")),
                          tags$li(tags$style("#month{display:inline}"), class = "dropdown", tags$a(HTML(paste("Month selected - ", textOutput("month"))))),
                          tags$li(tags$style("#crime{display:inline}"), class = "dropdown", tags$a(HTML(paste("Crime type selected - ", textOutput("crime"))))),
                          tags$li(href = "", icon = icon("code"), title = "Get code", class = "dropdown"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Interactive Map", tabName = "map", icon = icon("map")),
    menuItem("Trend", tabName = "trend", icon = icon("line-chart")),
    menuItem("Forecast", tabName = "forecast", icon = icon("forward")),
    menuItem("Definitions", tabName = 'def', icon = icon("book-open")),
    selectInput("month", label = h4("Select month"), 
                choices = monthChoices, 
                selected = "December 2018"),
    radioButtons("crime_group", label = h4("Select crime group"), 
                       choices = list("All crime" = "All crime",
                                      "Anti-social behaviour" = "Anti-social behaviour",
                                      "Burglary" = "Burglary",
                                      "Bicycle theft" = "Bicycle theft",
                                      "Criminal damage and arson" = "Criminal damage and arson",
                                      "Drugs" = "Drugs",
                                      "Other crime" = "Other crime",
                                      "Other theft" = "Other theft",
                                      "Public order" = "Public order",
                                      "Possession of weapons" = "Possession of weapons",
                                      "Robbery" = "Robbery",
                                      "Shoplifting" = "Shoplifting",
                                      "Theft from the person" = "Theft from the person",
                                      "Vehicle crime" = "Vehicle crime",
                                      "Violence and sexual offences" = "Violence and sexual offences"),
                       selected = "All crime")
    )
  )

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard", 
            class = "active",
            h2("Month Dashboard"),
            fluidRow(
              infoBoxOutput("infoBox1"),
              infoBoxOutput("infoBox2"),
              infoBoxOutput("infoBox3")
            ),
            box(width = NULL, solidHeader = TRUE,
                leafletOutput("londonMap", height=400)
            ),
            box(width=NULL,
                dataTableOutput("boroughTable")
            )
            ),
  
  tabItem(tabName = "map",
          h2("Street Level Crime Map"),
          box(p("This map plots the location of each crime that falls under the currently selected criteria. Crimes are clustered by centre of mass, with the cluster showing on hover the number and area. Clicking on clusters will zoom in on the area until you are looking at a small enough geographical grouping for the individual crimes to be displayed. This clustering has been done for ease of naviagtion and presentation and should not be used to infer details crime hotspots."), solidHeader = TRUE, width = NULL, title = "Instructions"),
          box(width = NULL, solidHeader = TRUE,
              leafletOutput("interactiveMap", height=400)
          )
          ),
  
  tabItem(tabName = "trend",
          h2("Crime trend by month"),
          box(width = 12, title = "Total Crime Volume", solidHeader = TRUE,
              plotlyOutput("crimeTrend", height = 250)
          )
          ,
          fluidRow(
          box(title = "South Area", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("crimeSN", height = 250)
          ),
          box(title = "South East", solidHeader = TRUE, collapsible = TRUE,
              plotlyOutput("crimeSE", height = 250)
          )
          )
          ,
          fluidRow(
            box(title = "Central South", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("crimeAS", height = 250)
            ),
            box(title = "South West", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("crimeSW", height = 250)
            )
          )
            ,
            fluidRow(
            box(title = "West Area", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeWA", height = 250)
            ),
            box(title = "North West", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("crimeNW", height = 250)
            )
          )
          ,
          fluidRow(
            box(title = "North Area", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeNA", height = 250)
            ),
            box(title = "North East", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeNE", height = 250)
            )
            )
          ,
          fluidRow(  
            box(title = "Central North", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeCN", height = 250)
            ),
            box(title = "Central East", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeCE", height = 250)
            )
            )
          ,
            fluidRow(
              box(title = "Central West", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeAW", height = 250)
            ),
            box(title = "East Area", solidHeader = TRUE, collapsible = TRUE, 
                plotlyOutput("crimeEA", height = 250)
            )
          )
  ),
  
  tabItem(tabName = "forecast",
          h2("Crime type forecast"),
          h3("Forecast Methods"),
          fluidRow(
            box(collapsible = TRUE, collapsed = TRUE, title = "ARIMA", p("ARIMA stands for Autoregressive Integrated Moving Average models. Univariate (single vector) ARIMA is a forecasting technique that projects the future values of a series based entirely on its own inertia. Its main application is in the area of short term forecasting requiring at least 40 historical data points. It works best when your data exhibits a stable or consistent pattern over time with a minimum amount of outliers. Sometimes called Box-Jenkins (after the original authors), ARIMA is usually superior to exponential smoothing techniques when the data is reasonably long and the correlation between past observations is stable. If the data is short or highly volatile, then some smoothing method may perform better. If you do not have at least 38 data points, you should consider some other method than ARIMA.")),
            box(collapsible = TRUE, collapsed = TRUE, title = "Exponential Smoothing", p("Exponential smoothing is a rule of thumb technique for smoothing time series data using the exponential window function. Whereas in the simple moving average the past observations are weighted equally, exponential functions are used to assign exponentially decreasing weights over time. It is an easily learned and easily applied procedure for making some determination based on prior assumptions by the user, such as seasonality. Exponential smoothing is often used for analysis of time-series data."))
          ),
          fluidRow(
            box(collapsible = TRUE, collapsed = TRUE, title = "Holt Winters'", p("Holt Winter's method, also known as triple exponential smoothing, applies exponential smoothing three times, which is commonly used when there are three high frequency signals to be removed from a time series under study. There are different types of seasonality: 'multiplicative' and 'additive' in nature, much like addition and multiplication are basic operations in mathematics.")),
            box(collapsible = TRUE, collapsed = TRUE, title = "Neural Network", p("A neural network (NN), in the case of artificial neurons called artificial neural network (ANN) or simulated neural network (SNN), is an interconnected group of natural or artificial neurons that uses a mathematical or computational model for information processing based on a connectionistic approach to computation. In most cases an ANN is an adaptive system that changes its structure based on external or internal information that flows through the network."), br(), p("In more practical terms neural networks are non-linear statistical data modeling or decision making tools. They can be used to model complex relationships between inputs and outputs or to find patterns in data."))
          ),
          fluidRow(
            column(width = 8,
                   box(plotOutput("fcplot"),
                       width = NULL),
                   box(dataTableOutput("predtab"),
                       width = NULL),
                   box(plotOutput("fcdiag"),
                       width = NULL)),
            column(width = 4,
                   box(selectInput("scope", "Choose areas to include:",
                                   c("Pan-London" = "Pan-London",
                                     "South Area" = "South Area",
                                     "South East" = "South East",
                                     "Central South" = "Central South",
                                     "South West" = "South West",
                                     "West Area" = "West Area",
                                     "North West" = "North West",
                                     "North Area" = "North Area",
                                     "North East" = "North East",
                                     "Central North" = "Central North",
                                     "Central East" = "Central East",
                                     "Central West" = "Central West",
                                     "East Area" = "East Area")),
                     selectInput("forecast", "Choose Forecast Method:",
                                   c("ARIMA" = "fit_arima",
                                     "Exponential Smoothing" = "fit_ets",
                                     "Holt-Winters'" = "fit_hw",
                                     "Neural Networks" = "fit_net")),
                       sliderInput("periods", "Months to forecast:", 12, 24, 12),
                       width=NULL),
                       box(DTOutput("accuracy_table"),
                       width=NULL),
                   box(verbatimTextOutput("test_print"),
                       width=NULL)
            )
            )
            
          ),
  tabItem(tabName = "def",
          h2("Crime type definitions"),
          fluidRow(
            box(width = 4, 
                title = "Anti Social Behaviour",
                p("Includes personal, environmental and nuisance anti-social behaviour."),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Burglary",
                p("Includes offences where a person enters a house or other building with the intention of stealing."),
                br(),
                h4("Specific offences"),
                DTOutput("burglary"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Bicycle theft",
                p("Includes the taking without consent or theft of a pedal cycle."),
                br(),
                h4("Specific offences"),
                DTOutput("bicycle"),
                collapsible = TRUE,
                collapsed = TRUE)
          ),
          fluidRow(
            box(width = 4,
                title = "Criminal damage and arson",
                p("Includes damage to buildings and vehicles and deliberate damage by fire."),
                br(),
                h4("Specific offences"),
                DTOutput("criminal"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Drugs",
                p("Includes offences related to possession, supply and production."),
                br(),
                h4("Specific offences"),
                DTOutput("drugs"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Other crime",
                p("Includes forgery, perjury and other miscellaneous crime."),
                br(),
                h4("Specific offences"),
                DTOutput("other_c"),
                collapsible = TRUE,
                collapsed = TRUE)
          ),
          fluidRow(
            box(width = 4,
                title = "Other theft",
                p("Includes theft by an employee, blackmail and making off without payment."),
                br(),
                h4("Specific offences"),
                DTOutput("other_t"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Public order",
                p("Includes offences which cause fear, alarm or distress."),
                br(),
                h4("Specific offences"),
                DTOutput("public"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Possession of weapons",
                p("Includes possession of a weapon, such as a firearm or knife."),
                br(),
                h4("Specific offences"),
                DTOutput("weapon"),
                collapsible = TRUE,
                collapsed = TRUE)
          ),
          fluidRow(
            box(width = 4,
                title = "Robbery",
                p("Includes offences where a person uses force or threat of force to steal."),
                br(),
                h4("Specific offences"),
                DTOutput("robbery"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Shoplifting",
                p("Includes theft from shops or stalls."),
                br(),
                h4("Specific offences"),
                DTOutput("shop"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Theft from the person",
                p("Includes crimes that involve theft directly from the victim (including handbag, wallet, cash, mobile phones) but without the use or threat of physical force."),
                br(),
                h4("Specific offences"),
                DTOutput("theft"),
                collapsible = TRUE,
                collapsed = TRUE)
          ),
          fluidRow(
            box(width = 4,
                title = "Vehicle crime",
                p("Includes theft from or of a vehicle or interference with a vehicle."),
                br(),
                h4("Specific offences"),
                DTOutput("vehicle"),
                collapsible = TRUE,
                collapsed = TRUE),
            box(width = 4,
                title = "Violence and sexual offences",
                p("Includes offences against the person such as common assaults, Grievous Bodily Harm and sexual offences."),
                br(),
                h4("Specific offences"),
                DTOutput("violence"),
                collapsible = TRUE,
                collapsed = TRUE)
          )
    
  )
  )
)


dashboardPage(
  header,
  sidebar, 
  body
)  