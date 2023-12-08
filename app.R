#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loading packages
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(DT)
library(readxl)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Training Monitoring"),
  
  dashboardSidebar(
    
    fileInput(inputId = "session_file", 
              label = "Select the session data file:"),
    
    selectInput(inputId = "player_name", label = "Select the player:", ""),
    
    sliderInput(inputId = "terminal_weeks", 
                label = "Select the weeks to display:", 
                min = 0, max = 2, value = c(0,1))
  ),
  
  dashboardBody(
    
    fluidRow(
      box(title = "Acute and chronic loads", plotOutput("loadHist")),
      box(title = "Ratio 7:28", plotOutput("ratioHist"))
    ),
    
    fluidRow(
      box(title = "Load distribution", plotOutput("pieChart"))
    ),
    
    fluidRow(
      box(title = "Session data table", DTOutput("weekSessionDataTable")),
      box(title = "Daily data table", DTOutput("weekDailyDataTable"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Updating player name selection
  observe({
    
    # Check that file has been loaded
    req(input$session_file)
    
    # Getting sheet names
    players <- excel_sheets(input$session_file$datapath)
    
    # Updating selectInput choices 
    updateSelectInput(session, "player_name", choices = players)
  })
  
  # Updating week selection
  observe({
    
    # Check that data frame has been built
    req(daily_data())
    
    # Getting first and last weeks
    first_week <- min(daily_data()$WeekNumber)
    last_week <- max(daily_data()$WeekNumber)
    
    # Updating sliderInput
    updateSliderInput(session, "terminal_weeks", min = first_week, max = last_week, value = c(first_week, first_week+3))
  })
  
  
  # Building session data
  session_data <- reactive({
    
    # Checking if a session file has been selected
    if (is.null(input$session_file) || input$player_name==""){
      return(NULL)
    }
    
    # Loading data from xlsx
    session_data <- read_excel(input$session_file$datapath, sheet = input$player_name)
    session_data$Date <- as.Date(session_data$Date)
    
    # Calculating session load
    session_data$SessionLoad <- session_data$Duration * session_data$RPE
    
    # Getting the number of the week
    session_data$WeekNumber <- week(session_data$Date)
    
    return(session_data)
  })
  
  # Building daily data
  daily_data <- reactive({
    
    # Checking if a session file has been selected
    if (is.null(input$session_file) || input$player_name==""){
      return(NULL)
    }
    
    # Defining start date of the calendar on a Monday
    start_date <- min(session_data()$Date)
    week_day <- weekdays(start_date)
    if (week_day != "lundi") {
      start_date <- start_date - days(weekdays(start_date) %in% c("mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    }
    
    # Defining end date of the calendar on a Sunday
    end_date <- max(session_data()$Date)
    week_day_end <- weekdays(end_date)
    if (week_day_end != "dimanche") {
      end_date <- end_date + days(weekdays(end_date) %in% c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi"))
    }
    
    # Building daily data frame
    daily_data <- data.frame(Date = seq(start_date, end_date, by = "day"))
    
    # Getting the number of the week
    daily_data$WeekNumber <- week(daily_data$Date)
    
    # For each day
    for (i in 1:nrow(daily_data)) {
      # Getting the current date
      current_date <- daily_data$Date[i]
      
      # Filtering sessions which happened this day
      sessions_in_day <- session_data() %>% filter(Date == current_date)
      
      # Updating values
      daily_data$DailyLoad[i] <- sum(sessions_in_day$SessionLoad)
      daily_data$NbSessions[i] <- nrow(sessions_in_day)
    }
    
    
    ## Calculating acute and chronic loads
    
    # Initializing the first day
    daily_data$AcuteLoad <- NA
    daily_data$ChronicLoad <- NA
    
    # Initializing the first day
    daily_data$AcuteLoad[1] <- daily_data$DailyLoad[1]
    daily_data$ChronicLoad[1] <- daily_data$DailyLoad[1]
    
    # For each day
    for (i in 2:nrow(daily_data)) {
      # Calculating acute load
      daily_data$AcuteLoad[i] <- (2/8) * daily_data$DailyLoad[i] + (1 - 2/8) * daily_data$AcuteLoad[i-1]
      
      # Calculating chronic load
      daily_data$ChronicLoad[i] <- (2/29) * daily_data$DailyLoad[i] + (1 - 2/29) * daily_data$ChronicLoad[i-1]
    }
    
    # Calculating ratio
    daily_data$Ratio <- daily_data$AcuteLoad / daily_data$ChronicLoad
    daily_data$Ratio <- ifelse(is.na(daily_data$Ratio), 0, daily_data$Ratio) # To avoid NA values
    
    return(daily_data)
  })
  
  
  # Building week_session_data based on selected weeks
  week_session_data <- reactive({
    
    # Checking if session_data is NULL
    if (is.null(session_data())) {
      return(NULL)
    }
    
    selected_weeks <- seq(input$terminal_weeks[1], input$terminal_weeks[2])
    
    return(session_data() %>%
             filter(WeekNumber %in% selected_weeks))
  })
  
  # Building week_daily_data based on selected weeks
  week_daily_data <- reactive({
    
    # Checking if daily_data is NULL
    if (is.null(daily_data())) {
      return(NULL)
    }
    
    selected_weeks <- seq(input$terminal_weeks[1], input$terminal_weeks[2])
    
    return(daily_data() %>%
             filter(WeekNumber %in% selected_weeks))
  })
  
  # Displaying acute and chronic loads
  output$loadHist <- renderPlot({
    
    # Check if week_daily_data is NULL
    if (is.null(week_daily_data())) {
      return(NULL)
    }
    
    ggplot(week_daily_data(), aes(x = Date)) +
      geom_bar(aes(y = DailyLoad), stat = "identity", fill = "grey", color = "grey") +
      geom_line(aes(y = AcuteLoad, color = "AcuteLoad"), linewidth = 1) +
      geom_line(aes(y = ChronicLoad, color = "ChronicLoad"), linewidth = 1) +
      geom_text(aes(y = DailyLoad, label = NbSessions), vjust = -0.5, color = "black", size = 3) +
      labs(x = "Date", y = "Daily load") +
      scale_color_manual(values = c("AcuteLoad" = "cornflowerblue", "ChronicLoad" = "red")) +
      theme_minimal()
  })
  
  # Displaying ratio 7:28
  output$ratioHist <- renderPlot({
    
    # Check if week_daily_data is NULL
    if (is.null(week_daily_data())) {
      return(NULL)
    }
    
    # Defining the top for the danger zone for the display
    top_danger_zone = max(max(week_daily_data()$DailyLoad), max(week_daily_data()$Ratio * 1000))
    
    ggplot(week_daily_data(), aes(x = Date)) +
      geom_bar(aes(y = DailyLoad), stat = "identity", fill = "grey", color = "grey") +
      geom_line(aes(y = Ratio * 1000), color = "black", size = 1) +
      geom_text(aes(y = DailyLoad, label = NbSessions), vjust = -0.5, color = "black", size = 3) +
      labs(x = "Date", y = "Values") +
      theme_minimal() +
      scale_y_continuous(
        name = "DailyLoad",
        sec.axis = sec_axis(~./1000, name = "Ratio")
      ) +
      geom_ribbon(
        aes(ymin = 0.8 * 1000, ymax = 1.3 * 1000),
        fill = "green", alpha = 0.3
      ) +
      geom_ribbon(
        aes(ymin = 1.5 * 1000, ymax = top_danger_zone + 300),
        fill = "red", alpha = 0.3
      )
  })
  
  # Displaying load distribution
  output$pieChart <- renderPlot({
    
    # Check if week_session_data is NULL
    if (is.null(week_session_data())) {
      return(NULL)
    }
    
    # Calculating the load sum for each type
    type_sum <- aggregate(SessionLoad ~ Type, data = week_session_data(), sum)
    
    # Calculating proportions
    type_sum$Proportion <- type_sum$SessionLoad / sum(type_sum$SessionLoad)
    
    ggplot(type_sum, aes(x = "", y = Proportion, fill = Type)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      geom_text(aes(label = scales::percent(Proportion)), position = position_stack(vjust = 0.5)) +
      theme_void()
    })
  
  # Displaying week_session_data table
  output$weekSessionDataTable <- renderDT({
    
    # Check if week_session_data is NULL
    if (is.null(week_session_data())) {
      return(NULL)
    }
    
    # Change data format to have the good one
    formatted_data <- week_session_data()
    formatted_data$Date <- format(formatted_data$Date, "%d-%m-%Y")
    
    return(formatted_data)
  })
  
  # Displaying week_daily_data table
  output$weekDailyDataTable <- renderDT({
    # Check if week_daily_data is NULL
    if (is.null(week_daily_data())) {
      return(NULL)
    }

    # Change data format to have the good one
    formatted_data <- week_daily_data()
    formatted_data$Date <- format(formatted_data$Date, "%d-%m-%Y")
    
    return(formatted_data)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
