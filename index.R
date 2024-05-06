# Load necessary libraries
library(dplyr)
library(shiny)
library(readxl)
library(tidyr)
library(DT)
library(randomForest)
library(readr)
library(plotly)
# Load data
AllPitches <- read_csv("main_dataset.csv") %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  mutate(
    ExitSpeed = as.numeric(na_if(ExitSpeed, "NULL")), 
    Distance = as.numeric(na_if(Distance, "NULL")), 
    Direction = as.numeric(na_if(Direction, "NULL")), 
    Angle = as.numeric(na_if(Angle, "NULL")),
    PlateLocHeight = as.numeric(na_if(PlateLocHeight, "NULL")),
    PlateLocSide = as.numeric(na_if(PlateLocSide, "NULL")),
    SpinRate = as.numeric(na_if(SpinRate, "NULL")),
    InducedVertBreak = as.numeric(na_if(InducedVertBreak, "NULL")),
    HorzBreak = as.numeric(na_if(HorzBreak, "NULL")),
    TypeOfHitBall = case_when(
      (BatterSide == "Right" & Direction < -20) ~ "PULL",
      (BatterSide == "Right" & Direction >= -20 & Direction <= 20) ~ "STRAIGHT",
      (BatterSide == "Right" & Direction > 20) ~ "OPPO",
      (BatterSide == "Left" & Direction > 20) ~ "PULL",
      (BatterSide == "Left" & Direction >= -20 & Direction <= 20) ~ "STRAIGHT",
      (BatterSide == "Left" & Direction < -20) ~ "OPPO"
    )
  )

# Load the organization mapping
mapping <- read_csv("mapping.csv")

# Add Organization to AllPitches based on the mapping
AllPitches <- AllPitches %>%
  left_join(mapping, by = c("BatterTeam" = "BatterTeam"))  # Use the mapping to add Organization

# Load the trained model
rf_model2 <- readRDS(normalizePath("rf_model2.rds"))

# Apply the model
AllPitches$PredictedPitchType <- predict(rf_model2, AllPitches)

# Use the model in a reactive context
predictions <- shiny::reactive({
  predict(rf_model2(), AllPitches)
})

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("Organization", "Select organization:",
                  choices = c("ALL", sort(unique(AllPitches$ORGANIZATION))),
                  selected = "ALL"),
      uiOutput("BatterUI"),
      selectizeInput("PitchCall", "Select pitch call(s):",
                     choices = c("ALL", sort(unique(AllPitches$PitchCall))),
                     selected = "ALL",
                     multiple = TRUE),
      selectInput("PitcherThrows", "Select pitcher hand:",
                  choices = c("ALL", sort(unique(AllPitches$PitcherThrows))),
                  selected = "ALL"),
      dateRangeInput("Date", "Select date range:",
                     start = min(AllPitches$Date), end = max(AllPitches$Date)),
      selectInput("TypeOfHitBall", "Select type of hit ball:",
                  choices = c("ALL", sort(unique(AllPitches$TypeOfHitBall))),
                  selected = "ALL"),
      selectInput("PredictedPitchType", "Select predicted pitch type:",
                  choices = c("ALL", as.character(unique(AllPitches$PredictedPitchType))),
                  selected = "ALL"),
      sliderInput("ExitSpeedRange", "Select Exit Speed range:",
                  min = 0, max = max(as.numeric(AllPitches$ExitSpeed), na.rm = TRUE),
                  value = c(0, max(as.numeric(AllPitches$ExitSpeed), na.rm = TRUE)), step = 1),
      sliderInput("DistanceRange", "Select Distance range:",
                  min = 0, max = max(as.numeric(AllPitches$Distance), na.rm = TRUE),
                  value = c(0, max(as.numeric(AllPitches$Distance), na.rm = TRUE)), step = 1)
    ),
    mainPanel(
      uiOutput("polarPlots")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # Initial UI for selecting batter
  output$BatterUI <- renderUI({
    selectizeInput("Batter", "Select batter:",
                   choices = c("ALL", sort(unique(AllPitches$Batter))),
                   selected = "ALL",
                   multiple = TRUE,
                   options = list(maxItems = 9))
  })
  
  # Dynamically update the choices based on the selected organization
  observeEvent(input$Organization, {
    org_selected <- input$Organization
    if(org_selected == "ALL") {
      choices_batter = c("ALL", sort(unique(AllPitches$Batter)))
    } else {
      # Select only the batters from the selected organization
      choices_batter = c("ALL", sort(unique(AllPitches$Batter[AllPitches$ORGANIZATION == org_selected])))
    }
    updateSelectizeInput(session, "Batter", "Select batter:", choices = choices_batter)
  })
  
  # Generate polar plots UI
  output$polarPlots <- renderUI({
    req(length(input$Batter) > 0)  # only render when there are selected batters
    plot_output_list <- lapply(1:length(input$Batter), function(i) {
      plotname <- paste0("polar_plot_", i)
      plotlyOutput(plotname)
    })
    
    do.call(fluidRow, plot_output_list)
  })
  
  # Define filtered_data here
  filtered_data <- reactive({
    AllPitches %>%
      filter(if(input$Organization != "ALL") ORGANIZATION == input$Organization else TRUE) %>%  # Filter by ORGANIZATION
      filter(if (!is.null(input$Batter) && input$Batter != "ALL") Batter == input$Batter else TRUE) %>%
      filter(if("ALL" %in% input$PitchCall) TRUE else PitchCall %in% input$PitchCall) %>%
      filter(if(input$PitcherThrows != "ALL") PitcherThrows == input$PitcherThrows else TRUE) %>%
      filter(Date >= input$Date[1] & Date <= input$Date[2]) %>%
      filter(if(input$PredictedPitchType != "ALL") PredictedPitchType == input$PredictedPitchType else TRUE) %>%
      filter(if(input$ExitSpeedRange[1] == 0) TRUE else (ExitSpeed >= input$ExitSpeedRange[1] & ExitSpeed <= input$ExitSpeedRange[2])) %>%
      filter(if(input$DistanceRange[1] == 0) TRUE else (Distance >= input$DistanceRange[1] & Distance <= input$DistanceRange[2])) %>%
      filter(if(input$TypeOfHitBall != "ALL") TypeOfHitBall == input$TypeOfHitBall else TRUE)
  })
  
  # Generate polar plots
  observe({
    for (i in 1:length(input$Batter)) {
      local({
        my_i <- i
        output[[paste0("polar_plot_", my_i)]] <- renderPlotly({
          batter <- input$Batter[my_i]
          
          # Filter the data for this specific batter
          data <- AllPitches %>%
            filter(if(input$Organization != "ALL") ORGANIZATION == input$Organization else TRUE) %>%  # Filter by ORGANIZATION
            filter(Batter == batter) %>%
            filter(if("ALL" %in% input$PitchCall) TRUE else PitchCall %in% input$PitchCall) %>%
            filter(if(input$PitcherThrows != "ALL") PitcherThrows == input$PitcherThrows else TRUE) %>%
            filter(Date >= input$Date[1] & Date <= input$Date[2]) %>%
            filter(if(input$PredictedPitchType != "ALL") PredictedPitchType == input$PredictedPitchType else TRUE) %>%
            filter(if(input$ExitSpeedRange[1] == 0) TRUE else (ExitSpeed >= input$ExitSpeedRange[1] & ExitSpeed <= input$ExitSpeedRange[2])) %>%
            filter(if(input$DistanceRange[1] == 0) TRUE else (Distance >= input$DistanceRange[1] & Distance <= input$DistanceRange[2])) %>%
            filter(if(input$TypeOfHitBall != "ALL") TypeOfHitBall == input$TypeOfHitBall else TRUE)
          
          # Directly generate the plot with plot_ly
          plot_ly(data, type = 'scatterpolar', mode = 'markers') %>%
            add_trace(
              r = ~Distance,
              theta = ~Direction,
              text = ~paste("Date: ", Date,
                            "<br>Distance: ", Distance,
                            "<br>Batter: ", Batter,
                            "<br>RelSpeed: ", RelSpeed,
                            "<br>Exit velocity: ", ExitSpeed,
                            "<br>Predicted Pitch Type: ", PredictedPitchType,  # Added PitchType to hover info
                            "<br>Angle: ", Angle),
              hoverinfo = "text",
              marker = list(size = 6, color = ~ExitSpeed, colorscale = "RdBu")  # Use ExitSpeed for color directly
            ) %>%
            layout(
              polar = list(
                radialaxis = list(
                  visible = T,
                  range = c(0, max(data$Distance, na.rm = TRUE))
                ),
                angularaxis = list(
                  direction = "clockwise",
                  period = 360
                )
              )
            )
        })
      })
    }
  })
  
  
  # Define color scale for Pitch Type
  pitchTypeColorScale <- c(
    "FF" = "blue",
    "CH" = "red",
    "FT" = "green",
    "FC" = "purple",
    "CU" = "orange",
    "SL" = "pink"
  )
  
 
  # Calculate color_scale reactively only once for polar plot
  color_scale_polar <- reactive({
    if (input$polar_color == 'PredictedPitchType') {
      pitchTypeColorScale[filtered_data()[[input$polar_color]]]
    } else {
      filtered_data()[[input$polar_color]]
    }
  })

}
shinyApp(ui, server)
