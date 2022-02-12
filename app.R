#DANIEL DEAN ASUNCION
#CS 424
#Project 1 - Subway

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(scales)

#Assume all the necessary data is in the directory is .csv
temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, read.csv)
ridership <- do.call(rbind, allData2)

#Assign each of the chosen stations to their own data frame
uichalsted <- subset(ridership, stationname == "UIC-Halsted")
ohare <- subset(ridership, stationname == "O'Hare Airport")
lasalle <- subset(ridership, stationname == "LaSalle")


#Create a more usable date format for each data frame
newDates_m1 <- as.Date(uichalsted$date, "%m/%d/%Y")
uichalsted$newDate<-newDates_m1

newDates_m2 <- as.Date(ohare$date, "%m/%d/%Y")
ohare$newDate <- newDates_m2

newDates_m3 <- as.Date(lasalle$date, "%m/%d/%Y")
lasalle$newDate <- newDates_m3

#Variables for possible user inputs
availableStations <- c("LaSalle", "O'Hare Airport", "UIC-Halsted")
weekdayOrder <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
years <- c(2001:2021)
visualSelection <- c("Daily Total", "Weekday Total", "Monthly Total", "Yearly Total")

#Create Shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("CTA Station Comparison", tabName = "CTA_Comparison", icon = NULL),
                     menuItem("About", tabName = "AboutTab")
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "CTA_Comparison",
              
        fluidRow(
          column(width = 6,
            fluidRow(
              column(width = 4,
                selectInput("graph1Station", "Select a Station (1) to Visualize", availableStations, selected = "UIC-Halsted")
              ),
              column(width = 4,
                selectInput("graph1Year", "Select the Year to Visualize", years, selected = 2021)
              ),
              column(width = 4,
                selectInput("graph1visType", "Select how to Visualize the Data", visualSelection, selected = "Yearly Total")
              )
            ),
            box( title = textOutput("graph1Title"), solidHeader = TRUE, status = "primary", width = 100,
                 plotOutput("graph1Yearly", height = 280))
          ),
          column(width = 6,
            fluidRow(
              column(width = 4,
                     selectInput("graph2Station", "Select a Station (2) to Visualize", availableStations, selected = "O'Hare Airport")
              ),
              column(width = 4,
                     selectInput("graph2Year", "Select the Year to Visualize", years, selected = 2021)
              ),
              column(width = 4,
                     selectInput("graph2visType", "Select how to Visualize the Data", visualSelection, selected = "Yearly Total")
              )
            ),

            box(title = textOutput("graph2Title"), solidHeader = TRUE, status = "primary", width = 100,
                plotOutput("graph2Yearly", height = 280))
      
          )
        ),

        fluidRow(
          column(width = 6,
                 fluidRow(
                   column(width = 4,
                          selectInput("graph3Station", "Select a Station (3) to Visualize", availableStations, selected = "LaSalle")
                   ),
                   column(width = 4,
                          selectInput("graph3Year", "Select the Year to Visualize", years, selected = 2021)
                   ),
                   column(width = 4,
                          selectInput("graph3visType", "Select how to Visualize the Data", visualSelection, selected = "Yearly Total")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          box( title = textOutput("graph3Title"), solidHeader = TRUE, status = "primary", width = 100,
                               plotOutput("graph3Yearly", height = 280)
                          )
                   )
                 )
          ),
          column(width = 6,
                 tabsetPanel(
                   id = 'datasets',
                   tabPanel("Graph 1 Data", DT::dataTableOutput("dataSet1")),
                   tabPanel("Graph 2 Data", DT::dataTableOutput("dataSet2")),
                   tabPanel("Graph 3 Data", DT::dataTableOutput("dataSet3"))
                 )
          )
        )
      ),
      tabItem(tabName = "AboutTab",
              h2("About the Project"),
              verbatimTextOutput("aboutPage"))
    )
  )
)

server <- function(input, output) {
  
  #Parse the data for the first graph for the year chosen
  graph1YearReactive <- reactive({
    if(input$graph1Station == "UIC-Halsted"){
      subset(uichalsted, format(uichalsted$newDate, '%Y') == input$graph1Year)
    }
    else if(input$graph1Station == "O'Hare Airport"){
      subset(ohare, format(ohare$newDate, '%Y') == input$graph1Year)
    }
    else if(input$graph1Station == "LaSalle"){
      subset(lasalle, format(lasalle$newDate, '%Y') == input$graph1Year)
    }
    else{
      #No station
    }
  })
  
  #Parse the data for the second graph for the year chosen
  graph2YearReactive <- reactive({
    if(input$graph2Station == "UIC-Halsted"){
      subset(uichalsted, format(uichalsted$newDate, '%Y') == input$graph2Year)
    }
    else if(input$graph2Station == "O'Hare Airport"){
      subset(ohare, format(ohare$newDate, '%Y') == input$graph2Year)
    }
    else if(input$graph2Station == "LaSalle"){
      subset(lasalle, format(lasalle$newDate, '%Y') == input$graph2Year)
    }
    else{
      #Nothing
    }
  })
  
  #Parse the data for the third graph for the year chosen
  graph3YearReactive <- reactive({
    if(input$graph3Station == "UIC-Halsted"){
      subset(uichalsted, format(uichalsted$newDate, '%Y') == input$graph3Year)
    }
    else if(input$graph3Station == "O'Hare Airport"){
      subset(ohare, format(ohare$newDate, '%Y') == input$graph3Year)
    }
    else if(input$graph3Station == "LaSalle"){
      subset(lasalle, format(lasalle$newDate, '%Y') == input$graph3Year)
    }
    else{
      #Nothing
    }
  })
  
  #Reactive data frame used to calculate total rides per year for first graph
  graph1TotalReactive <- reactive({
    if(input$graph1Station == "UIC-Halsted"){
      uichalsted
    }
    else if(input$graph1Station == "O'Hare Airport"){
      ohare
    }
    else if(input$graph1Station == "LaSalle"){
      lasalle
    }
    else{
      #Nothing
    }
  })
  
  #Reactive data frame used to calculate total rides per year for the second graph
  graph2TotalReactive <- reactive({
    if(input$graph2Station == "UIC-Halsted"){
      uichalsted
    }
    else if(input$graph2Station == "O'Hare Airport"){
      ohare
    }
    else if(input$graph2Station == "LaSalle"){
      lasalle
    }
    else{
      #Nothing
    }
  })
  
  #Reactive data frame used to calculate total rides per year for the third graph
  graph3TotalReactive <- reactive({
    if(input$graph3Station == "UIC-Halsted"){
      uichalsted
    }
    else if(input$graph3Station == "O'Hare Airport"){
      ohare
    }
    else if(input$graph3Station == "LaSalle"){
      lasalle
    }
    else{
      #Nothing
    }
  })
  
  
  #Dynamically generate Graph 1 Title
  output$graph1Title <- renderText({
    if(input$graph1Station == "UIC-Halsted"){
      return(paste("UIC-Halsted Ridership by Chosen Year: ", input$graph1Year))
    }
    else if(input$graph1Station == "O'Hare Airport"){
      return(paste("O'Hare Airport Ridership by Chosen Year: ", input$graph1Year))
    }
    else if(input$graph1Station == "LaSalle"){
      return(paste("LaSalle Ridership by Chosen Year: ", input$graph1Year))
    }
    else{
      #Do nothing
    }
  })
  
  #Show Graph 1 dynamically based on user input
  output$graph1Yearly <- renderPlot({

    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Condition to calculate first graph by "Daily Total"
    if(input$graph1visType == "Daily Total"){

      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph2visType == "Daily Total" && input$graph3visType != "Daily Total"){
          max(c(max(graph1Data$rides), max(graph2Data$rides)))
        }
        else if(input$graph3visType == "Daily Total" && input$graph2visType != "Daily Total"){
          max(c(max(graph1Data$rides), max(graph3Data$rides)))
        }
        else if(input$graph2visType == "Daily Total" && input$graph3visType == "Daily Total"){
          max(c(max(graph1Data$rides), max(graph2Data$rides), max(graph3Data$rides)))
        }
        else{
          max(graph1Data$rides)
        }
      
      #Plot the bar graph by "Daily Total" based on graph 1's calculated data frame
      ggplot(graph1Data, aes(x=newDate, y=rides)) + 
      geom_bar(stat="identity", fill = "#009E73") + 
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = "Total Rides per Day") + 
      xlab("Day") + ylab("Total Rides") +
      theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
      scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate first graph by "Monthly Total"
    else if(input$graph1visType == "Monthly Total"){
      
      #Data frame of graph 1 is used to calculate y-axis limit
      oneYearByMonth <- aggregate(list(rides = graph1Data$rides), list(date = format(graph1Data$newDate, "%m/01/%Y")), sum)
      newDates2 <- as.Date(oneYearByMonth$date, "%m/%d/%Y")
      oneYearByMonth$totalByMonth <- newDates2
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2ByMonth <- aggregate(list(rides = graph2Data$rides), list(date = format(graph2Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph2 <- as.Date(graph2ByMonth$date, "%m/%d/%Y")
      graph2ByMonth$totalByMonth <- newDates2_graph2
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3ByMonth <- aggregate(list(rides = graph3Data$rides), list(date = format(graph3Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph3 <- as.Date(graph3ByMonth$date, "%m/%d/%Y")
      graph3ByMonth$totalByMonth <- newDates2_graph3
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph2visType == "Monthly Total" && input$graph3visType != "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph2ByMonth$rides)))
        }
        else if(input$graph3visType == "Monthly Total" && input$graph2visType != "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph3ByMonth$rides)))
        }
        else if(input$graph2visType == "Monthly Total" && input$graph3visType == "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph2ByMonth$rides), max(graph3ByMonth$rides)))
        }
        else{
         max(oneYearByMonth$rides)
        }
      
      #Plot the bar graph by "Monthly Total" based on graph 1's calculated data frame
      ggplot(oneYearByMonth, aes(x=totalByMonth, y=rides)) + 
      geom_bar(stat="identity", fill = "#009E73") + 
      scale_x_date(breaks = oneYearByMonth$totalByMonth, date_labels = "%b") +
      labs(title = "Total Rides per Month") +
      xlab("Month") + ylab("Total Rides") +
      theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
      scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate first graph by "Weekday Total"
    else if(input$graph1visType == "Weekday Total"){
      
      #Data frame of graph 1 is used to calculate y-axis limit
      weekdaysThisYear <- graph1Data
      weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
      oneYearByWeekday <- aggregate(list(rides = weekdaysThisYear$rides), list(weekdayTotal = weekdaysThisYear$weekday), sum)
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2Weekdays <- graph2Data
      graph2Weekdays$weekday <- weekdays(graph2Weekdays$newDate)
      graph2YearByWeekday <- aggregate(list(rides = graph2Weekdays$rides), list(weekdayTotal = graph2Weekdays$weekday), sum)
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3Weekdays <- graph3Data
      graph3Weekdays$weekday <- weekdays(graph3Weekdays$newDate)
      graph3YearByWeekday <- aggregate(list(rides = graph3Weekdays$rides), list(weekdayTotal = graph3Weekdays$weekday), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph2visType == "Weekday Total" && input$graph3visType != "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph2YearByWeekday$rides)))
        }
        else if(input$graph3visType == "Weekday Total" && input$graph2visType != "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else if(input$graph2visType == "Weekday Total" && input$graph3visType == "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph2YearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else{
          max(oneYearByWeekday$rides)
        }
      
      #Plot the bar graph by "Weekday Total" based on graph 1's calculated data frame
      ggplot(oneYearByWeekday, aes(x=factor(weekdayTotal, level = weekdayOrder), y=rides)) + 
        geom_bar(stat="identity", fill = "#009E73") +
        labs(title = "Total Rides per Weekday") +
        xlab("Weekday") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate first graph by "Yearly Total"
    else{
      
      #Data frames needed to calculate total rides per year
      graph1Total <- aggregate(list(rides = graph1TotalData$rides), list(thisYear = format(graph1TotalData$newDate, "%Y")), sum)
      graph2Total <- aggregate(list(rides = graph2TotalData$rides), list(thisYear = format(graph2TotalData$newDate, "%Y")), sum)
      graph3Total <- aggregate(list(rides = graph3TotalData$rides), list(thisYear = format(graph3TotalData$newDate, "%Y")), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph2visType == "Yearly Total" && input$graph3visType != "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph2Total$rides)))
        }
        else if(input$graph3visType == "Yearly Total" && input$graph2visType != "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph3Total$rides)))
        }
        else if(input$graph2visType == "Yearly Total" && input$graph3visType == "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph2Total$rides), max(graph3Total$rides)))
        }
        else{
          max(graph1Total$rides)
        }
      
      #Plot the bar graph by "Yearly Total" based on graph 1's calculated data frame
      ggplot(graph1Total, aes(x = thisYear, y = rides)) + 
        geom_bar(stat="identity", fill = "#009E73") +
        labs(title = "Total Rides per Year") +
        xlab("Year") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
  })
  
  #Dynamically generate Graph 2 Title
  output$graph2Title <- renderText({
    if(input$graph2Station == "UIC-Halsted"){
      return(paste("UIC-Halsted Ridership by Chosen Year: ", input$graph2Year))
    }
    else if(input$graph2Station == "O'Hare Airport"){
      return(paste("O'Hare Airport Ridership by Chosen Year: ", input$graph2Year))
    }
    else if(input$graph2Station == "LaSalle"){
      return(paste("LaSalle Ridership by Chosen Year: ", input$graph2Year))
    }
    else{
      #Do nothing
    }
  })
  
  #Show Graph 2 dynamically based on user input
  output$graph2Yearly <- renderPlot({
    
    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Condition to calculate second graph by "Daily Total"
    if(input$graph2visType == "Daily Total"){
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Daily Total" && input$graph3visType != "Daily Total"){
          max(c(max(graph1Data$rides), max(graph2Data$rides)))
        }
        else if(input$graph3visType == "Daily Total" && input$graph1visType != "Daily Total"){
          max(c(max(graph2Data$rides), max(graph3Data$rides)))
        }
        else if(input$graph1visType == "Daily Total" && input$graph3visType == "Daily Total"){
          max(c(max(graph1Data$rides), max(graph2Data$rides), max(graph3Data$rides)))
        }
        else{
          max(graph2Data$rides)
        }
      
      #Plot the bar graph by "Daily Total" based on graph 2's calculated data frame
      ggplot(graph2Data, aes(x=newDate, y=rides)) + 
        geom_bar(stat="identity", fill = "#0072B2") + 
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        labs(title = "Total Rides per Day") + 
        xlab("Day") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate second graph by "Monthly Total"
    else if(input$graph2visType == "Monthly Total"){
      #Data frame of graph 1 is used to calculate y-axis limit
      oneYearByMonth <- aggregate(list(rides = graph1Data$rides), list(date = format(graph1Data$newDate, "%m/01/%Y")), sum)
      newDates2 <- as.Date(oneYearByMonth$date, "%m/%d/%Y")
      oneYearByMonth$totalByMonth <- newDates2
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2ByMonth <- aggregate(list(rides = graph2Data$rides), list(date = format(graph2Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph2 <- as.Date(graph2ByMonth$date, "%m/%d/%Y")
      graph2ByMonth$totalByMonth <- newDates2_graph2
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3ByMonth <- aggregate(list(rides = graph3Data$rides), list(date = format(graph3Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph3 <- as.Date(graph3ByMonth$date, "%m/%d/%Y")
      graph3ByMonth$totalByMonth <- newDates2_graph3
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Monthly Total" && input$graph3visType != "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph2ByMonth$rides)))
        }
        else if(input$graph3visType == "Monthly Total" && input$graph1visType != "Monthly Total"){
          max(c(max(graph2ByMonth$rides), max(graph3ByMonth$rides)))
        }
        else if(input$graph1visType == "Monthly Total" && input$graph3visType == "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph2ByMonth$rides), max(graph3ByMonth$rides)))
        }
        else{
          max(graph2ByMonth$rides)
        }
      
      #Plot the bar graph by "Monthly Total" based on graph 2's calculated data frame
      ggplot(graph2ByMonth, aes(x=totalByMonth, y=rides)) + 
        geom_bar(stat="identity", fill = "#0072B2") + 
        scale_x_date(breaks = graph2ByMonth$totalByMonth, date_labels = "%b") +
        labs(title = "Total Rides per Month") +
        xlab("Month") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate second graph by "Weekday Total"
    else if(input$graph2visType == "Weekday Total"){
      
      #Data frame of graph 1 is used to calculate y-axis limit
      weekdaysThisYear <- graph1Data
      weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
      oneYearByWeekday <- aggregate(list(rides = weekdaysThisYear$rides), list(weekdayTotal = weekdaysThisYear$weekday), sum)
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2Weekdays <- graph2Data
      graph2Weekdays$weekday <- weekdays(graph2Weekdays$newDate)
      graph2YearByWeekday <- aggregate(list(rides = graph2Weekdays$rides), list(weekdayTotal = graph2Weekdays$weekday), sum)
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3Weekdays <- graph3Data
      graph3Weekdays$weekday <- weekdays(graph3Weekdays$newDate)
      graph3YearByWeekday <- aggregate(list(rides = graph3Weekdays$rides), list(weekdayTotal = graph3Weekdays$weekday), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Weekday Total" && input$graph3visType != "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph2YearByWeekday$rides)))
        }
        else if(input$graph3visType == "Weekday Total" && input$graph1visType != "Weekday Total"){
          max(c(max(graph2YearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else if(input$graph1visType == "Weekday Total" && input$graph3visType == "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph2YearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else{
          max(graph2YearByWeekday$rides)
        }
      
      #Plot the bar graph by "Weekday Total" based on graph 2's calculated data frame
      ggplot(graph2YearByWeekday, aes(x=factor(weekdayTotal, level = weekdayOrder), y=rides)) + 
        geom_bar(stat="identity", fill = "#0072B2") +
        labs(title = "Total Rides per Weekday") +
        xlab("Weekday") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate second graph by "Yearly Total"
    else{
      
      #Data frames needed to calculate total rides per year
      graph1Total <- aggregate(list(rides = graph1TotalData$rides), list(thisYear = format(graph1TotalData$newDate, "%Y")), sum)
      graph2Total <- aggregate(list(rides = graph2TotalData$rides), list(thisYear = format(graph2TotalData$newDate, "%Y")), sum)
      graph3Total <- aggregate(list(rides = graph3TotalData$rides), list(thisYear = format(graph3TotalData$newDate, "%Y")), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Yearly Total" && input$graph3visType != "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph2Total$rides)))
        }
        else if(input$graph3visType == "Yearly Total" && input$graph1visType != "Yearly Total"){
          max(c(max(graph2Total$rides), max(graph3Total$rides)))
        }
        else if(input$graph1visType == "Yearly Total" && input$graph3visType == "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph2Total$rides), max(graph3Total$rides)))
        }
        else{
          max(graph2Total$rides)
        }
      
      #Plot the bar graph by "Yearly Total" based on graph 2's calculated data frame
      ggplot(graph2Total, aes(x = thisYear, y = rides)) + 
        geom_bar(stat="identity", fill = "#0072B2") +
        labs(title = "Total Rides per Year") +
        xlab("Year") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
  })
  
  #Dynamically generate Graph 3 Title
  output$graph3Title <- renderText({
    if(input$graph3Station == "UIC-Halsted"){
      return(paste("UIC-Halsted Ridership by Chosen Year: ", input$graph3Year))
    }
    else if(input$graph3Station == "O'Hare Airport"){
      return(paste("O'Hare Airport Ridership by Chosen Year: ", input$graph3Year))
    }
    else if(input$graph3Station == "LaSalle"){
      return(paste("LaSalle Ridership by Chosen Year: ", input$graph3Year))
    }
    else{
      #Do nothing
    }
  })
  
  #Show Graph 3 dynamically based on user input
  output$graph3Yearly <- renderPlot({
    
    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Condition to calculate third graph by "Daily Total"
    if(input$graph3visType == "Daily Total"){
      
      yAxisMax <-
        if(input$graph1visType == "Daily Total" && input$graph2visType != "Daily Total"){
          max(c(max(graph1Data$rides), max(graph3Data$rides)))
        }
      else if(input$graph2visType == "Daily Total" && input$graph1visType != "Daily Total"){
        max(c(max(graph2Data$rides), max(graph3Data$rides)))
      }
      else if(input$graph1visType == "Daily Total" && input$graph3visType == "Daily Total"){
        max(c(max(graph1Data$rides), max(graph2Data$rides), max(graph3Data$rides)))
      }
      else{
        max(graph3Data$rides)
      }
      
      #Plot the bar graph by "Daily Total" based on graph 3's calculated data frame
      ggplot(graph3Data, aes(x=newDate, y=rides)) + 
        geom_bar(stat="identity", fill = "#D55E00") + 
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        labs(title = "Total Rides per Day") + 
        xlab("Day") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate second graph by "Monthly Total"
    else if(input$graph3visType == "Monthly Total"){
      
      #Data frame of graph 1 is used to calculate y-axis limit
      oneYearByMonth <- aggregate(list(rides = graph1Data$rides), list(date = format(graph1Data$newDate, "%m/01/%Y")), sum)
      newDates2 <- as.Date(oneYearByMonth$date, "%m/%d/%Y")
      oneYearByMonth$totalByMonth <- newDates2
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2ByMonth <- aggregate(list(rides = graph2Data$rides), list(date = format(graph2Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph2 <- as.Date(graph2ByMonth$date, "%m/%d/%Y")
      graph2ByMonth$totalByMonth <- newDates2_graph2
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3ByMonth <- aggregate(list(rides = graph3Data$rides), list(date = format(graph3Data$newDate, "%m/01/%Y")), sum)
      newDates2_graph3 <- as.Date(graph3ByMonth$date, "%m/%d/%Y")
      graph3ByMonth$totalByMonth <- newDates2_graph3
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Monthly Total" && input$graph2visType != "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph3ByMonth$rides)))
        }
        else if(input$graph2visType == "Monthly Total" && input$graph1visType != "Monthly Total"){
          max(c(max(graph2ByMonth$rides), max(graph3ByMonth$rides)))
        }
        else if(input$graph1visType == "Monthly Total" && input$graph2visType == "Monthly Total"){
          max(c(max(oneYearByMonth$rides), max(graph2ByMonth$rides), max(graph3ByMonth$rides)))
        }
        else{
          max(graph3ByMonth$rides)
        }
      
      #Plot the bar graph by "Monthly Total" based on graph 3's calculated data frame
      ggplot(graph3ByMonth, aes(x=totalByMonth, y=rides)) + 
        geom_bar(stat="identity", fill = "#D55E00") + 
        scale_x_date(breaks = graph3ByMonth$totalByMonth, date_labels = "%b") +
        labs(title = "Total Rides per Month") +
        xlab("Month") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate third graph by "Weekday Total"
    else if(input$graph3visType == "Weekday Total"){
      
      #Data frame of graph 1 is used to calculate y-axis limit
      weekdaysThisYear <- graph1Data
      weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
      oneYearByWeekday <- aggregate(list(rides = weekdaysThisYear$rides), list(weekdayTotal = weekdaysThisYear$weekday), sum)
      
      #Data frame of graph 2 is used to calculate y-axis limit
      graph2Weekdays <- graph2Data
      graph2Weekdays$weekday <- weekdays(graph2Weekdays$newDate)
      graph2YearByWeekday <- aggregate(list(rides = graph2Weekdays$rides), list(weekdayTotal = graph2Weekdays$weekday), sum)
      
      #Data frame of graph 3 is used to calculate y-axis limit
      graph3Weekdays <- graph3Data
      graph3Weekdays$weekday <- weekdays(graph3Weekdays$newDate)
      graph3YearByWeekday <- aggregate(list(rides = graph3Weekdays$rides), list(weekdayTotal = graph3Weekdays$weekday), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph2visType == "Weekday Total" && input$graph1visType != "Weekday Total"){
          max(c(max(graph3YearByWeekday$rides), max(graph2YearByWeekday$rides)))
        }
        else if(input$graph1visType == "Weekday Total" && input$graph2visType != "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else if(input$graph1visType == "Weekday Total" && input$graph2visType == "Weekday Total"){
          max(c(max(oneYearByWeekday$rides), max(graph2YearByWeekday$rides), max(graph3YearByWeekday$rides)))
        }
        else{
          max(graph3YearByWeekday$rides)
        }
      
      #Plot the bar graph by "Weekday Total" based on graph 3's calculated data frame
      ggplot(graph3YearByWeekday, aes(x=factor(weekdayTotal, level = weekdayOrder), y=rides)) + 
        geom_bar(stat="identity", fill = "#D55E00") +
        labs(title = "Total Rides per Weekday") +
        xlab("Weekday") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
    #Condition to calculate third graph by "Yearly Total"
    else{
      
      #Data frames needed to calculate total rides per year
      graph1Total <- aggregate(list(rides = graph1TotalData$rides), list(thisYear = format(graph1TotalData$newDate, "%Y")), sum)
      graph2Total <- aggregate(list(rides = graph2TotalData$rides), list(thisYear = format(graph2TotalData$newDate, "%Y")), sum)
      graph3Total <- aggregate(list(rides = graph3TotalData$rides), list(thisYear = format(graph3TotalData$newDate, "%Y")), sum)
      
      #Calculate y-axis limit dynamically to match the y-axis limit of another graph if they have the same visualization type
      yAxisMax <-
        if(input$graph1visType == "Yearly Total" && input$graph2visType != "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph3Total$rides)))
        }
        else if(input$graph2visType == "Yearly Total" && input$graph1visType != "Yearly Total"){
          max(c(max(graph2Total$rides), max(graph3Total$rides)))
        }
        else if(input$graph1visType == "Yearly Total" && input$graph2visType == "Yearly Total"){
          max(c(max(graph1Total$rides), max(graph2Total$rides), max(graph3Total$rides)))
        }
        else{
          max(graph3Total$rides)
        }
      
      #Plot the bar graph by "Yearly Total" based on graph 3's calculated data frame
      ggplot(graph3Total, aes(x = thisYear, y = rides)) + 
        geom_bar(stat="identity", fill = "#D55E00") +
        labs(title = "Total Rides per Year") +
        xlab("Year") + ylab("Total Rides") +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold", size = 14),
              axis.title.y = element_text(face = "bold", size = 14),
              axis.text.x = element_text(face = "bold", size = 11),
              axis.text.y = element_text(face = "bold", size = 11)) + 
        scale_y_continuous(labels = label_number_si(), limits = (c(0, yAxisMax)))
    }
  })
  
  #Generate the data for the first graph to a table
  output$dataSet1 <- DT::renderDataTable({
    
    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Data frame for Graph 1 Table
    graph1DataTable <-
      #Condition to output first graph by "Yearly Total" to Graph 1 Table
      if(input$graph1visType == "Yearly Total"){
        aggregate(list(Total_Rides = graph1TotalData$rides), list(Year = format(graph1TotalData$newDate, "%Y")), sum)
      }
      #Condition to output first graph by "Daily Total" to Graph 1 Table
      else if(input$graph1visType == "Daily Total"){
        graph1Data
      }
      #Condition to output first graph by "Weekday Total" to Graph 1 Table
      else if((input$graph1visType == "Weekday Total")){
        weekdaysThisYear <- graph1Data
        weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
        oneYearByWeekday <- aggregate(list(Total_Rides = weekdaysThisYear$rides), list(Weekday = weekdaysThisYear$weekday), sum)
        oneYearByWeekday$Weekday <- ordered(oneYearByWeekday$Weekday, levels = weekdayOrder)
        oneYearByWeekday[order(oneYearByWeekday$Weekday),]
      }
      #Condition to output first graph by "Monthly Total" to Graph 1 Table
      else if((input$graph1visType == "Monthly Total")){
        oneYearByMonth <- aggregate(list(Total_Rides = graph1Data$rides), list(Month = format(graph1Data$newDate, "%b")), sum)
        oneYearByMonth$Month <- ordered(oneYearByMonth$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        oneYearByMonth[order(oneYearByMonth$Month),]
      }
      else{
        #Do nothing
      }
    
      #Generate the table
      DT::datatable(graph1DataTable, options = list("pageLength" = 7))
  })
  
  #Generate the data for the second graph to a table
  output$dataSet2 <- DT::renderDataTable({
    
    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Data frame for Graph 2 Table
    graph2DataTable <-
      #Condition to output second graph by "Yearly Total" to Graph 2 Table
      if(input$graph2visType == "Yearly Total"){
        aggregate(list(Total_Rides = graph2TotalData$rides), list(Year = format(graph2TotalData$newDate, "%Y")), sum)
      }
      #Condition to output second graph by "Daily Total" to Graph 2 Table
      else if(input$graph2visType == "Daily Total"){
        graph2Data
      }
      #Condition to output second graph by "Weekday Total" to Graph 2 Table
      else if((input$graph2visType == "Weekday Total")){
        weekdaysThisYear <- graph2Data
        weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
        oneYearByWeekday <- aggregate(list(Total_Rides = weekdaysThisYear$rides), list(Weekday = weekdaysThisYear$weekday), sum)
        oneYearByWeekday$Weekday <- ordered(oneYearByWeekday$Weekday, levels = weekdayOrder)
        oneYearByWeekday[order(oneYearByWeekday$Weekday),]
      }
      #Condition to output second graph by "Monthlyy Total" to Graph 2 Table
      else if((input$graph2visType == "Monthly Total")){
        oneYearByMonth <- aggregate(list(Total_Rides = graph2Data$rides), list(Month = format(graph2Data$newDate, "%b")), sum)
        oneYearByMonth$Month <- ordered(oneYearByMonth$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        oneYearByMonth[order(oneYearByMonth$Month),]
      }
      else{
        #Do nothing
      }
    
    #Generate the table
    DT::datatable(graph2DataTable, options = list("pageLength" = 7))
  })
  
  #Generate the data for the third graph to a table
  output$dataSet3 <- DT::renderDataTable({
    
    #Data frames needed to calculate total rides by day, by weekday, or by month for the chosen year
    graph1Data <- graph1YearReactive()
    graph2Data <- graph2YearReactive()
    graph3Data <- graph3YearReactive()
    
    #Data frames needed to calculate total rides per year
    graph1TotalData <- graph1TotalReactive()
    graph2TotalData <- graph2TotalReactive()
    graph3TotalData <- graph3TotalReactive()
    
    #Data frame for Graph 3 Table
    graph3DataTable <-
      #Condition to output third graph by "Yearly Total" to Graph 3 Table
      if(input$graph3visType == "Yearly Total"){
        aggregate(list(Total_Rides = graph3TotalData$rides), list(Year = format(graph3TotalData$newDate, "%Y")), sum)
      }
      #Condition to output third graph by "Daily Total" to Graph 3 Table
      else if(input$graph3visType == "Daily Total"){
        graph3Data
      }
      #Condition to output third graph by "Weekday Total" to Graph 3 Table
      else if((input$graph3visType == "Weekday Total")){
        weekdaysThisYear <- graph3Data
        weekdaysThisYear$weekday <- weekdays(weekdaysThisYear$newDate)
        oneYearByWeekday <- aggregate(list(Total_Rides = weekdaysThisYear$rides), list(Weekday = weekdaysThisYear$weekday), sum)
        oneYearByWeekday$Weekday <- ordered(oneYearByWeekday$Weekday, levels = weekdayOrder)
        oneYearByWeekday[order(oneYearByWeekday$Weekday),]
      }
      #Condition to output third graph by "Monthly Total" to Graph 3 Table
      else if((input$graph3visType == "Monthly Total")){
        oneYearByMonth <- aggregate(list(Total_Rides = graph3Data$rides), list(Month = format(graph3Data$newDate, "%b")), sum)
        oneYearByMonth$Month <- ordered(oneYearByMonth$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
        oneYearByMonth[order(oneYearByMonth$Month),]
      }
      else{
        #Do Nothing
      }
    
    #Generate the table
    DT::datatable(graph3DataTable, options = list("pageLength" = 7))
  })

  #Render About Page
  output$aboutPage <- renderText({
   "Developed By: Daniel Dean Asuncion / dasunc2 /  CS 424 Spring 2022\n
Project Released On: 2/12/2022\n
Original Data From: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\n
About the App:\n
The purpose of this project is to be able to visualize 3 separate CTA 'L' Stations and give the user\n
the ability to compare each of the 3 stations by year, by day, by weekday, or by month.
   "
 })
}

shinyApp(ui = ui, server = server)