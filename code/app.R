
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
      h2("Predicting Fish Taxonomy Based on Egg Characteristics"), 
      p("Add:"),
      p("- information on the random forest model"), 
      p("- how to use the app"),
      p("- previous papers"), 
      p("- picture(s) of fish or logo"),
      p("- etc.")
    )
  ),
  
  tabPanel(
    title = "Input Data",
    fluidRow(
      column(
        h2("Egg Characteristics"),
        p("The random forest predictions of the taxonomy of the fish eggs are based on various egg characteristics. This page provides various ways to provide these egg characteristics. Either a spreadsheet (.xls, .xlsx, or .csv) file may be provided, or values may be entered manually."),
        h3("Data Input Option"),
        selectInput(
          inputId = "input_opt",
          label = "Select a manner in which to provide input values for the random forest",
          choices = c(" ", "Upload Excel file", "Upload csv file", "Manually input values")
        ),
        width = 4
      ),
      column(
        conditionalPanel(
          h3("Excel Spreadsheet"),
          condition = "input.input_opt == 'Upload Excel file'",
          fileInput("excel", "Select an Excel file to upload")
        ),
        conditionalPanel(
          condition = "input.input_opt == 'Upload csv file'",
          fileInput("csv", "Select a csv file to upload")
        ),
        conditionalPanel(
          condition = "input.input_opt == 'Manually input values'",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Egg Identification", 
              textInput("Egg_ID", "Egg ID:")
            ),
            tabPanel(
              "Environment Variables",
              dateInput("Date", "Date of Collection (YYYY-MM-DD):", format = "yyyy-mm-dd"),
              textInput("Temperature", "Water Temperature (C):"),
              textInput("Conductivity", "Conductivity (muS/cm):")
            ),
            tabPanel(
              "Categorical Egg Measurements",
              column(
                selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
                selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
                selectInput("Egg_Stage", "Egg Development Stage:", c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "Diffuse")),
                width = 6
              ),
              column(
                selectInput("Compact_Diffuse", "Compact or Diffuse Embryo:", c("", "Compact", "Diffuse")),
                selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No")),
                width = 6
              )
            ),
            tabPanel(
              "Quantitative Egg Measurements",
              column(
                textInput("Membrane_Ave", "Membrane Average (mm):"),
                textInput("Yolk_Ave", "Embryo Average (mm):"),
                textInput("Larval_Length", "Late Stage Embryo Midline Length (mm):"),
                width = 6
              ),
              column(
                textInput("Membrane_SD", "Membrane Standard Deviation (mm):"), 
                textInput("Yolk_SD", "Embryo Standard Deviation (mm):"),
                width = 6
              )
            )
          )
        ),
        width = 8
      )
    ),
    conditionalPanel(
      condition = "input.input_opt != ' '",
      h3("Predictor variables inputs"),
      DT::dataTableOutput("input_table")
    )
  ),
  
  tabPanel(
    title = "Predictions",
    column(
      width = 5,
      h2("Random Forest Predictions"),
      tableOutput("pred_table")
    ),
    column(
      width = 7,
      h3("Random forest probabilities for all taxonomic levels"),
      plotOutput("prob_plot")
    )
  ),
  
  tabPanel(
    title = "Help",
    h2("Help Page"),
    p("Place to put helpful information including a glossary for egg characteristics")
  ),
  
  tabPanel(
    title = "Questions",
    p("Random forests with reduced variables? (currently full)")
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
  output$input_table <- DT::renderDataTable({
    # Adjust format of df for viewing
    datatable(prepareInputs() %>%
                    mutate_all(.funs = as.character), #%>%
                  # pivot_longer(names_to = "Variable",
                  #              values_to = "Input Value",
                  #              cols = everything()), 
              options = list(scrollX=TRUE, scrollCollapse=TRUE, 
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '1px', targets = "_all"))))
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