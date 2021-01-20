
# Load packages
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(randomForest)
library(shiny)
library(shinythemes)
library(stringr)
library(tidyr)

source("../code/helper-functions.R")

rfs <- readRDS("../data/rfs141516.rds")

# Define UI for miles per gallon app ----
ui <- navbarPage(
  
  title = "WhoseEgg",
  theme = shinytheme("flatly"),
  
  tabPanel(
    title = "Overview",
    fluidPage(
      h2("Add information about the app:"), 
      p("information on the random forest model, how to use the app, previous papers, etc.")
    )
  ),
  
  tabPanel(
    title = "Input Data",
    h2("Egg Characteristics"),
    selectInput(
      inputId = "input_opt",
      label = "Select a manner in which to provide input values for the random forest",
      choices = c("Upload Excel file", "Upload csv file", "Manually input values")
    ),
    conditionalPanel(
      condition = "input.input_opt == 'Upload Excel file'",
      fileInput("excel", "Select an Excel file to upload")
    ),
    conditionalPanel(
      condition = "input.input_opt == 'Upload csv file'",
      fileInput("csv", "Select a csv file to upload")
    ), 
    conditionalPanel(
      condition = "input.input_opt == 'Manually input values'",
      # Location variables
      dateInput("Date", "Date of Collection (YYYY-MM-DD):", format = "yyyy-mm-dd"),
      textInput("Temperature", "Water Temperature (C):"),
      textInput("Conductivity", "Conductivity (muS/cm):"),
      # Categorical egg measurements
      selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
      selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
      selectInput("Egg_Stage", "Egg Development Stage:", c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "Diffuse")),
      selectInput("Compact_Diffuse", "Compact or Diffuse Embryo:", c("", "Compact", "Diffuse")),
      selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No")),
      # Quantitative egg measurements
      textInput("Membrane_Ave", "Membrane Average (mm):"),
      textInput("Membrane_SD", "Membrane Standard Deviation (mm):"),
      textInput("Yolk_Ave", "Embryo Average (mm):"),
      textInput("Yolk_SD", "Embryo Standard Deviation (mm):"),
      textInput("Larval_Length", "Late Stage Embryo Midline Length (mm):")
    )
  ),
  
  tabPanel(
    title = "Random Forest",
    column(
      width = 5,
      h2("Random forest prediction"),
      tableOutput("pred_table"),
      h2("Predictor variables inputs"),
      tableOutput("input_table")
    ),
    column(
      width = 7,
      h2("Random forest probabilities for all taxonomic levels"),
      plotOutput("prob_plot")
    )
  ),
  
  tabPanel(
    title = "Help",
    p("Place to put helpful information including a glossary for egg characteristics")
  ),
  
  tabPanel(
    title = "Questions",
    p("Name for app?"),
    p("How to order the variables?"),
    p("Random forests with reduced variables? (currently full)"),
    p("What does the egg stage of 'D' stand for?"),
    p("Should we change the level of ACGC to Invasive Carp?"),
    p("Should we order the levels in the plot by highest to lowest probabilities?")
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Put the specified input variables in a data frame
  prepareInputs <- reactive({
    if (input$input_opt == "Upload Excel file") {
      file <- input$excel
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("xlsx", "xls"), "Please upload an Excel file"))
      readxl::read_excel(file$datapath)
    } else if (input$input_opt == "Upload csv file") {
      file <- input$csv
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      read.csv(file$datapath)
    } else if (input$input_opt == "Manually input values") {
      inputs_to_df(input)
    }
  })
  
  # Obtain random forest predictions for the given inputs
  getRFpreds <- reactive({
    
    # Prepare the inputs for the random forest
    inputs_clean <-
      #prepareInputs() %>%
      example_vars %>%
      compute_variables() %>%
      adjust_variable_types() %>%
      adjust_factor_levels()
    
    # Get the predictions and random forest probabilities
    list(
      family_pred  = as.character(predict(rfs$Family_ACGC, inputs_clean)),
      genus_pred   = as.character(predict(rfs$Genus_ACGC, inputs_clean)),
      species_pred = as.character(predict(rfs$Common_Name_ACGC, inputs_clean)),
      family_prob  = data.frame(predict(rfs$Family_ACGC, inputs_clean, type = "prob")),
      genus_prob   = data.frame(predict(rfs$Genus_ACGC, inputs_clean, type = "prob")),
      species_prob = data.frame(predict(rfs$Common_Name_ACGC, inputs_clean, type = "prob"))
    )
    
  })
  
  # Create a table with the input values
  output$input_table <- renderTable({
    # Adjust format of df for viewing
    prepareInputs() %>%
      mutate_all(.funs = as.character) %>%
      pivot_longer(names_to = "Variable",
                   values_to = "Input Value",
                   cols = everything())
  })
  
  # Create a table with random forest prediction results
  output$pred_table <- renderTable({
    
    # Get the random forest predictions
    RFpreds <- getRFpreds()
    
    # Put the random forest results in a table
    data.frame(
      'Taxonomic Level' = c("Family", "Genus", "Species"),
      'Prediction' = c(
        RFpreds$family_pred,
        RFpreds$genus_pred,
        RFpreds$species_pred
      ),
      'Prediction Probability' = c(
        RFpreds$family_prob %>% pull(RFpreds$family_pred),
        RFpreds$genus_prob %>% pull(RFpreds$genus_pred),
        RFpreds$species_prob %>% pull(str_replace(RFpreds$species_pred, " ", "."))
      ),
      'Prediction Interval' = rep('to do', 3),
      check.names = FALSE
    )
    
  })
  
  # Create plots with the random forest probabilities for all taxonomic levels
  output$prob_plot <- renderPlot({
    
    # Get the random forest predictions
    RFpreds <- getRFpreds()
    
    # Create the plots
    cowplot::plot_grid(
      rf_prob_plot(RFpreds$family_prob, "Family"),
      rf_prob_plot(RFpreds$genus_prob, "Genus"),
      rf_prob_plot(RFpreds$species_prob, "Species"),
      ncol = 1
    )
    
  }, height = 800)
  
}

shinyApp(ui, server)