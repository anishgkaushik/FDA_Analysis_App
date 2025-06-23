# Load necessary libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(randomForest)
library(dplyr)

# Load your saved Random Forest model
rf_model <- readRDS("/Users/anishgkaushik/rf_model_final.rds")

# Load your dataset (for column information and scaling)
df <- read.csv("~/Desktop/Capstone Project/Dataset/FDA Inspections.csv", header = TRUE)

# Data Preparation
colnames(df) = gsub(" ", "_", colnames(df))
colnames(df) = gsub("/", "_", colnames(df))
colnames(df) <- gsub("[ /]", "_", colnames(df))  # Clean column names
df$Zip <- as.numeric(df$Zip)
df <- df[!is.na(df$Zip), ]
df$Country.Area <- as.factor(df$Country.Area)
df$State <- as.factor(df$State)
df$Posted.Citations <- as.factor(df$Posted.Citations)

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "FDA Inspections Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Feature Importance", tabName = "importance", icon = icon("bar-chart")),
      menuItem("Data Overview", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Welcome to the FDA Inspections Dashboard", width = 12, status = "primary",
                    "This dashboard provides insights into FDA inspections, allows predictions of inspection classifications, and visualizes feature importance.")
              ),
              fluidRow(
                box(title = "Total Inspections by Location", width = 6, plotlyOutput("location_plot")),
                box(title = "Inspection Trends by Year", width = 6, plotlyOutput("yearly_trend_plot"))
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Input Features for Prediction", width = 4, status = "primary",
                    numericInput("FEI_Number", "FEI Number:", value = 1),
                    numericInput("Zip", "Zip Code:", value = 10000),
                    selectInput("Country_Area", "Country Area:", choices = levels(df$Country.Area)),
                    selectInput("State", "State:", choices = levels(df$State)),
                    numericInput("Fiscal_Year", "Fiscal Year:", value = 2022),
                    actionButton("predict_btn", "Predict Inspection Classification")
                ),
                box(title = "Prediction Result", width = 8, status = "primary", verbatimTextOutput("prediction_result"))
              )
      ),
      tabItem(tabName = "importance",
              fluidRow(
                box(title = "Feature Importance", width = 12, status = "primary", plotlyOutput("feature_importance_plot"))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Data Overview", width = 12, status = "primary", tableOutput("data_overview"))
              )
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  # Reactive expression to create a data frame from user inputs
  input_data <- reactive({
    data.frame(
      FEI.Number = input$FEI.Number,
      Zip = input$Zip,
      Country.Area = as.factor(input$Country.Area),
      State = as.factor(input$State),
      Fiscal.Year = input$Fiscal.Year
    )
  })
  
  # Predict inspection classification when button is clicked
  observeEvent(input$predict_btn, {
    new_data <- input_data()
    prediction <- predict(rf_model, newdata = new_data)
    output$prediction_result <- renderText({
      paste("Predicted Classification:", prediction)
    })
  })
  
  # Plot feature importance
  output$feature_importance_plot <- renderPlotly({
    importance_values <- importance(rf_model)
    feature_importance_df <- data.frame(Feature = rownames(importance_values), Importance = importance_values[, 1])
    
    plot_ly(feature_importance_df, x = ~Feature, y = ~Importance, type = 'bar', marker = list(color = 'blue')) %>%
      layout(
        title = "Feature Importance",
        xaxis = list(title = "Feature"),
        yaxis = list(title = "Importance")
      )
  })
  
  # Display first few rows of the dataset
  output$data_overview <- renderTable({
    head(df, 20)
  })
  
  # Plot total inspections by location (e.g., by state)
  output$location_plot <- renderPlotly({
    location_summary <- df %>% group_by(State) %>% summarise(Total_Inspections = n())
    
    plot_ly(location_summary, x = ~State, y = ~Total_Inspections, type = 'bar', marker = list(color = 'orange')) %>%
      layout(
        title = "Total Inspections by State",
        xaxis = list(title = "State"),
        yaxis = list(title = "Total Inspections")
      )
  })
  
  # Plot inspection trends by year
  output$yearly_trend_plot <- renderPlotly({
    yearly_summary <- df %>% group_by(Fiscal.Year) %>% summarise(Total_Inspections = n())
    
    plot_ly(yearly_summary, x = ~Fiscal.Year, y = ~Total_Inspections, type = 'scatter', mode = 'lines+markers', line = list(color = 'green')) %>%
      layout(
        title = "Inspection Trends by Year",
        xaxis = list(title = "Fiscal Year"),
        yaxis = list(title = "Total Inspections")
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)