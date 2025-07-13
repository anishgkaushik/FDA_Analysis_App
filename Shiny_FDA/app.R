# Shiny_FDA/app.R
# FDA Shiny page with prediction

library(shiny)
library(plotly)
library(readr) 
library(dplyr)
library(glmnet) 

cfg   <- yaml::read_yaml("../config.yml")
enc   <- readRDS(cfg$paths$enc_data)    # list(X, y)
perf  <- readRDS(cfg$paths$perf)        # tibble(date, cv_error)
glm_model = readRDS(cfg$paths$glm_model)

ui <- navbarPage("FDA Pipeline Dashboard",
  # Dashboard tab
  tabPanel("Dashboard",
    fluidRow(
      # column(4, plotlyOutput("vol_plot")), 
      column(4, plotlyOutput("balance_plot")),
      column(4, plotlyOutput("perf_plot"))
    )
  ),
  # Prediction tab
  tabPanel("Predict",
    sidebarLayout(
      sidebarPanel(
        selectInput("state", "State:", choices = sub("State\\.", "", colnames(enc$X)[grepl("^State\\.", colnames(enc$X))])),
        numericInput("year",  "Fiscal Year:",  value = as.integer(format(Sys.Date(), "%Y"))),
        actionButton("go",   "Predict")
      ),
      mainPanel(
        verbatimTextOutput("pred_class"),
        plotlyOutput("pred_probs")
      )
    )
  )
)

server <- function(input, output, session) {
  # Class balance pie chart
  output$balance_plot <- renderPlotly({
    tbl <- tibble(class = enc$y) %>% count(class)
    plot_ly(tbl, labels=~class, values=~n, type="pie")
  })

  # Model performance over time
  output$perf_plot <- renderPlotly({
    plot_ly(perf, x = ~date, y = ~cv_error, type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "CV Error"))
  })

  # # Placeholder for volume plot (implement if you have logs)
  # output$vol_plot <- renderPlotly({
  #   NULL
  # })

  # Reactive new data based on user inputs
  newdata <- eventReactive(input$go, {
  # Build blank feature row
    df <- as.data.frame(matrix(0, 1, ncol(enc$X)))
    colnames(df) <- colnames(enc$X)
    # One-hot encode state
    state_col <- paste0("State.", input$state)
    if (state_col %in% names(df)) df[[state_col]] <- 1
    # Fiscal year
    if ("Fiscal_Year" %in% names(df)) df$Fiscal_Year <- input$year
    df
  })

  # Display predicted class
  output$pred_class <- renderPrint({
    req(newdata())
   mat <- data.matrix(newdata())
    # get probabilities
    probs <- predict(glm_model, newx=mat, s="lambda.min", type="response")
    # manual class = highest prob
    cl <- colnames(probs)[max.col(probs)]
    cat("Predicted class:", cl, "\n")
  })

  # Display predicted class using explicit glmnet method
  output$pred_class <- renderPrint({
    req(newdata())
    mat <- as.matrix(newdata())
    pred <- predict(
      glm_model,
      newx = mat,
      s    = "lambda.min",
      type = "class"
    )
    cat("Predicted class:", as.character(pred), "\n")
  })

  # Display predicted probabilities using explicit glmnet method
  output$pred_probs <- renderPlotly({
    req(newdata())
       mat   <- data.matrix(newdata())
    probs <- predict(glm_model, newx=mat, s="lambda.min", type="response")
    dfp   <- tibble(class=colnames(probs), probability=as.numeric(probs[1,]))
    plot_ly(dfp, x=~class, y=~probability, type="bar") %>%
      layout(xaxis=list(title="Class"), yaxis=list(title="Probability"))  })
}

shinyApp(ui, server)
