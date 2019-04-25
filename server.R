library(shiny)
library(ggplot2)
library(ggfortify)
library(rgeos)
library(rgdal)
library(leaflet)
library(plotly)
library(DT)
library(forecast)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)

function(input, output, session){

  load("data.RData")
  
  output$month <- renderText(input$month)
  
  output$crime <- renderText(input$crime_group)
  
  summarydata <- reactive({
    
    if (input$crime_group == "All crime") {
      
    dataSet<-crime_summary[crime_summary$Month==input$month & crime_summary$NAME!="City of London", ]
    
    dataSet<- dataSet %>% group_by(NAME, Month) %>% summarise(Total = sum(Total))
    
    # Copy our GIS data
    joinedDataset<-boroughs[boroughs$NAME!="City of London",]
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="NAME"))
    
    # Return dataset
    joinedDataset
    
    }
    else {
      
    # Get a subset of the income data which is contingent on the input variables
    dataSet<-crime_summary[crime_summary$Month==input$month & crime_summary$Crime.type==input$crime_group & crime_summary$NAME!="City of London", ]
    
    # Copy our GIS data
    joinedDataset<-boroughs[boroughs$NAME!="City of London",]
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by="NAME"))
  
    # Return dataset
    joinedDataset
    }
  })

  graphdata <- reactive({
    
    if (input$crime_group == "All crime") {
      
    graphdata <- crime_summary[crime_summary$NAME!="City of London",]
    
    }
    else {
    # Get a subset of the income data which is contingent on the input variables
    graphdata<-crime_summary[crime_summary$Crime.type==input$crime_group & crime_summary$NAME!="City of London",]
    }
    # Convert month column to date format
    graphdata$Month <- parse_date_time(graphdata$Month, orders = "Ym")
    
    # Return dataset
    graphdata
  })

  output$infoBox1 <- renderInfoBox({
    data <- summarydata()
    
    total <- sum(data$Total, na.rm = TRUE)
    
    infoBox(
      "Monthly Total",
      total,
      icon = icon("equals"),
      color = "navy"
    )
  })
    
    
  output$infoBox2 <- renderInfoBox({ 
  data <- summarydata()
  
  highest <- data@data[which.max(data$Total),]
  
  infoBox(
    "Highest Crime Borough",
    highest$NAME,
    icon = icon("arrow-up"),
    color = "navy"
  )
  
})    
  
  output$infoBox3 <- renderInfoBox({ 
    data <- summarydata()
    
    lowest <- data@data[which.min(data$Total),]
    
    infoBox(
      "Lowest Crime Borough",
      lowest$NAME[1],
      icon = icon("arrow-down"),
      color = "navy"
    )
    
  })  
  
  # Due to use of leafletProxy below, this should only be called once
  output$londonMap<-renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=10 # set to 10 as 9 is a bit too zoomed out
      )       
    
  })
  
  observe({
    
    theData <- summarydata()
    
    if (input$crime_group == "All crime") { 
      
      # colour palette mapped to data
      pal <- colorBin("magma", theData$Total, pretty = TRUE) 
      
      # set text for the clickable popup labels
      borough_popup <- paste0("<strong>Borough: </strong>", 
                              theData$NAME, 
                              "<br><strong>",
                              "Month </strong>",
                              theData$Month,
                              "<br><strong>",
                              "Total: </strong>", 
                              theData$Total
      )
      
      # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
      leafletProxy("londonMap", data = theData) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = theData,
                    fillColor = pal(theData$Total), 
                    fillOpacity = 0.8, 
                    color = "#BDBDC3", 
                    weight = 2,
                    popup = borough_popup) %>%
        addLegend("bottomright", pal = pal, values = ~Total,
                  title = "Crime Volume",
                  opacity = 1
        )
    }
    else {
    
    # colour palette mapped to data
    pal <- colorBin("magma", theData$Total, pretty = TRUE) 
    
    # set text for the clickable popup labels
    borough_popup <- paste0("<strong>Borough: </strong>", 
                            theData$NAME, 
                            "<br><strong>",
                            "Month </strong>",
                            theData$Month,
                            "<br><strong>",
                            "Crime: </strong>",
                            theData$Crime.type,
                            "<br><strong>",
                            "Total: </strong>", 
                            theData$Total
    )
    
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("londonMap", data = theData) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Total), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = borough_popup) %>%
    addLegend("bottomright", pal = pal, values = ~Total,
              title = "Crime Volume",
              opacity = 1
    )
}
  })
  
  # table of results, rendered using data table
  output$boroughTable <- DT::renderDataTable(
    if (input$crime_group == "All crime") {
    
    datatable({
        dataSet<-summarydata()
        dataSet<-dataSet@data[,c(1,4)] # Just get name and value columns
        names(dataSet)<-c("Borough", "Total" )
        dataSet
      }, 
      options = list(lengthMenu = c(5, 10, 33), pageLength = 5, order = c(2, 'desc'))
      )
    }
    else {
      
    datatable({
    dataSet<-summarydata()
    dataSet<-dataSet@data[,c(1,4,5)] # Just get name and value columns
    names(dataSet)<-c("Borough", "Crime Type", "Total" )
    dataSet
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, order = c(3, 'desc'))
  )
}  
  )
  # This needs split down into high level and BCU tier comparisons 
  output$crimeTrend <- renderPlotly({
  
     lineData <- graphdata()
   
     lineData <- lineData %>%
       group_by(Month) %>%
       summarise(total = sum(Total))
     
    ggplotly(
     ggplot(lineData, aes(x = Month, y = total)) +
       geom_line() +
       geom_smooth(method = "auto") +
       labs(x="",y="Number of offences") +
       theme_minimal() +
       scale_y_continuous(labels = comma) +
       theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
  
    )
    })

  output$crimeSN <- renderPlotly({
    # Sutton, Croydon, Bromley
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Sutton" | lineData$NAME=="Croydon" | lineData$NAME=="Bromley",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
})
  
  output$crimeSE <- renderPlotly({
  # Lewisham, Greenwich, Bexley
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Lewisham" | lineData$NAME=="Greenwich" | lineData$NAME=="Bexley",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
})
  
  output$crimeAS <- renderPlotly({
  # Southwark, Lambeth

  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Southwark" | lineData$NAME=="Lambeth",]
 
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
})  
  
  output$crimeSW <- renderPlotly({
  # Wandsworth, Richmond, Merton, Kingston
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Wandsworth" | lineData$NAME=="Richmond upon Thames" | lineData$NAME=="Merton" | lineData$NAME=="Kingston upon Thames",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeWA <- renderPlotly({
  # Hillingdon, Ealing, Hounslow
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Hillingdon" | lineData$NAME=="Ealing" | lineData$NAME=="Hounslow",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeNW <- renderPlotly({
  # Barnet, Harrow, Brent

  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Barnet" | lineData$NAME=="Harrow" | lineData$NAME=="Brent",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
      ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
        scale_y_continuous(labels = comma) +
        labs(x="Date", y="Number of offences") +
        theme_minimal() +
        theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
})   
  
  output$crimeNA <- renderPlotly({
  # Enfield, Haringey
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Enfield" | lineData$NAME=="Haringey",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  )   %>% layout(hovermode = 'compare') 

}) 
  
  output$crimeNE <- renderPlotly({
  # Waltham Forest, Newham
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Waltham Forest" | lineData$NAME=="Newham",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeCN <- renderPlotly({
  # Camden, Islington
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Camden" | lineData$NAME=="Islington",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeCE <- renderPlotly({
  # Hackney, Tower Hamlets
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Hackney" | lineData$NAME=="Tower Hamlets",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeAW <- renderPlotly({
  # Westminster, Kensington & Cheslea, Hammersmith & Fulham
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Westminster" | lineData$NAME=="Kensington and Chelsea" | lineData$NAME=="Hammersmith and Fulham",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 
  
  output$crimeEA <- renderPlotly({
  # Redbridge, Barking & Dagenham, Havering
  
  lineData <- graphdata()
  
  lineData <- lineData[lineData$NAME=="Redbridge" | lineData$NAME=="Barking and Dagenham" | lineData$NAME=="Havering",]
  
  lineData <- lineData %>%
    group_by(NAME, Month) %>%
    summarise(Total = sum(Total))
  
  ggplotly(
    ggplot(lineData, aes(x = Month, y = Total, color = NAME)) +
      geom_line() +
      scale_y_continuous(labels = comma) +
      labs(x="Date", y="Number of offences") +
      theme_minimal() +
      theme(legend.position="top", legend.title = element_blank(), axis.text.x=element_text(hjust=1))
    
  ) %>% layout(hovermode = 'compare')
  
}) 

  timeseries <- reactive({
    if (input$crime_group == "All crime") {
      # Create a subset of data based on crime type input
      forecastdata <- crime_summary[crime_summary$NAME!="City of London",]  
    }
    else {
      # Create a subset of data based on crime type input
      forecastdata <- crime_summary[crime_summary$Crime.type==input$crime_group & crime_summary$NAME!="City of London",]
    }
    # Create further subset based on scope input
    if (input$scope == "Pan-London") {
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "South Area") {
      # Sutton, Croydon, Bromley
      forecastdata <- forecastdata[forecastdata$NAME=="Sutton" | forecastdata$NAME=="Croydon" | forecastdata$NAME=="Bromley",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "South East") {
      # Lewisham, Greenwich, Bexley
      forecastdata <- forecastdata[forecastdata$NAME=="Lewisham" | forecastdata$NAME=="Greenwich" | forecastdata$NAME=="Bexley",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "Central South") {
      # Southwark, Lambeth
      forecastdata <- forecastdata[forecastdata$NAME=="Southwark" | forecastdata$NAME=="Lambeth",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "South West") {
      # Wandsworth, Richmond upon Thames, Merton, Kingston upon Thamse
      forecastdata <- forecastdata[forecastdata$NAME=="Wandsworth" | forecastdata$NAME=="Richmond upon Thames" | forecastdata$NAME=="Merton" | forecastdata$NAME=="Kingston upon Thames",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "West Area") {
      # Hillingdon, Ealing, Hounslow
      forecastdata <- forecastdata[forecastdata$NAME=="Hillingdon" | forecastdata$NAME=="Ealing" | forecastdata$NAME=="Hounslow",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "North West") {
      # Barnet, Harrow, Brent
      forecastdata <- forecastdata[forecastdata$NAME=="Barnet" | forecastdata$NAME=="Harrow" | forecastdata$NAME=="Brent",]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "North Area") {
      # Enfield, Haringey
      forecastdtata <- forecastdata[forecastdata$NAME=='Enfield' | forecastdata$NAME=='Haringey',]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "North East") {
     # Waltham Forest, Newham  
      forecastdata <- forecastdata[forecastdata$NAME=='Waltham Forest' | forecastdata$NAME=='Newham',]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "Central North") {
    # Camden, Islington    
      forecastdata <- forecastdata[forecastdata$NAME=='Camden' | forecastdata$NAME=='Islington', ]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }                           
    else if (input$scope == "Central East") {
    # Hackney, Tower Hamlets
      forecastdata <- forecastdata[forecastdata$NAME=='Hackney' | forecastdata$NAME=='Tower Hamlets',]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }  
    else if (input$scope == "Central West") {
    # Westminster, Kensington & Cheslea, Hammersmith & Fulham 
      forecastdata <- forecastdata[forecastdata$NAME=='Westminster' | forecastdata$NAME=='Kensington and Chelsea' | forecastdata$NAME=='Hammersmith and Fulham', ]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    else if (input$scope == "East Area") {
    # Redbridge, Barking & Dagenham, Havering                                
      forecastdata <- forecastdata[forecastdata$NAME=='Redbridge' | forecastdata$NAME=='Barking and Dagenham' | forecastdata$NAME=='Havering', ]
      
      # Aggregate data to monthly totals
      forecastdata <- forecastdata %>% group_by(Month) %>% summarise(total = sum(Total))
      
      # Turn monthly totals into timeseries for forecasting
      forecastdata <- ts(forecastdata$total, start = c(2014, 1), frequency = 12)
      
      # Return dataset
      forecastdata
    }
    
    
  })
  
  training <- reactive({
    # Load copy of timeseries data
    training <- timeseries()
    
    # Take a subset of the time series excluding the most recent 12 months
    training <- window(training, end = c(2017, 12))
    
    # Return dataset
    training
  })
  
  test <- reactive({
    # Load copy of timeseries data
    test <- timeseries()
    
    # Take a subset of the timeseries to test models
    test <- window(test, start = c(2018, 1))
    
    # Return dataset
    test
  })


  output$accuracy_table <- renderDataTable({
    
    training <- training()
    
    test <- test()
    
    if (input$forecast == "fit_arima") {
      
      fit <- auto.arima(training)
      
      predict <- forecast(fit, test)
      
      acc <- round(accuracy(predict, test), 4)
      
      datatable(t(acc), options = list(dom = 't'))
      
    }
      else if (input$forecast == "fit_ets") {
      # Fit model
      fit <- ets(y = training)
        
      predict <- forecast(fit)
        
      acc <- round(accuracy(predict, test), 4)
        
      datatable(t(acc), options = list(dom = 't'))
      
    } else if (input$forecast == "fit_hw") {
      # Fit model
      fit <- ets(y = training, model = "MAM")
      
      predict <- forecast(fit)
      
      acc <- round(accuracy(predict, test), 4)
      
      datatable(t(acc), options = list(dom = 't'))
      
    } else if (input$forecast == "fit_net") {
      fit <- nnetar(y = training)
      
      predict <- forecast(fit)
      
      acc <- round(accuracy(predict, test), 4)
      
      datatable(t(acc), options = list(dom = 't'))
    }
  })

  training_model <- reactive({
    
    # Load training data to develop model
    data <- training()
    
    if (input$forecast == "fit_arima") {
      # Fit model
      fit_ar <- auto.arima(data)
      
    } else if (input$forecast == "fit_ets") {
      # Fit model
      fit_ets <- ets(y = data)
      
    } else if (input$forecast == "fit_hw") {
      # Fit model
      fit_ets <- ets(y = data, model = "MAM")
      
    } else if (input$forecast == "fit_net") {
      fit_nn <- nnetar(y = data)
    }
  })
  
  
  final_model <- reactive({
    
    # Load training data to develop model
    data <- timeseries()
    
    if (input$forecast == "fit_arima") {
      # Fit model
      fit_ar <- auto.arima(data)
      
    } else if (input$forecast == "fit_ets") {
      # Fit model
      fit_ets <- ets(y = data)
      
    } else if (input$forecast == "fit_hw") {
      # Fit model
      fit_ets <- ets(y = data, model = "MAM")
      
    } else if (input$forecast == "fit_net") {
      fit_nn <- nnetar(y = data)
    }
  })
  
  output$fcplot <- renderPlot({
    
    final_model <- final_model()
    
    fc <- forecast(final_model, h = input$periods, PI=TRUE, level = c(80,95,99))
    
    autoplot(fc) +
      guides(fill=guide_legend(title="Prediction Interval")) +
      labs(x="Date",
           y="Number of offences") +
      scale_y_continuous(labels = comma) +
      theme(axis.text.x = element_text(hjust = 1)) +
      theme_minimal()
    
  })  
   
  output$fcdecomp <- renderPlot({
    
    theme_set(theme_minimal())
    
    data <- timeseries()
    
    autoplot(decompose(data))
    
  })
  
  output$fcdiag <- renderPlot({
    
    theme_set(theme_minimal())
    
    model <- final_model()
    
    fc <- forecast(model, h = input$periods, PI=TRUE, level = c(80,95,99))
    
    checkresiduals(fc, plot = TRUE)

  })   
  
  output$predtab <- renderDataTable({
    
    model <- final_model()
    
    fc <- forecast(model, h = input$periods, PI=TRUE, level = c(80,95,99))
    
    fc <- as.data.frame(fc)
    
    datatable(round(fc, 2))
    
  })  
  
  output$test_print <- renderPrint({
    
    model <- final_model()
    
    print(model)
  })
  


output$burglary <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Burglary",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$bicycle <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Bicycle Theft",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$criminal <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Criminal Damage and Arson",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$criminal <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Criminal Damage and Arson",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$drugs <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Drugs",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$other_c <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Other Crime",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$other_t <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Other Theft",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$public <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Public Order",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$weapon <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Possession of Weapons",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$robbery <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Robbery",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$shop <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Shoplifting",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$theft <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Theft from the Person",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$vehicle <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Vehicle Crime",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

output$violence <- renderDataTable(
  datatable({
    dataSet <- police_uk_category_mappings
    dataSet <- dataSet[dataSet$'Police.uk Category' == "Violence and Sexual Offences",]
    dataSet <- dataSet[,c(1,2,3)]
  }, 
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
  )
)

}
