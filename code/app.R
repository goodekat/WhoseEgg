
##### ------------------------------------------------------------------------
##### SET UP
##### ------------------------------------------------------------------------

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

# Source the helper functions used by the app
source("../code/helper-functions.R")

# Load the random forest models (trained on the years of 2014-2016)
rfs <- readRDS("../data/rfs141516.rds")

##### ------------------------------------------------------------------------
##### APP UI
##### ------------------------------------------------------------------------

ui <- navbarPage(
  
  # Format information
  title = "WhoseEgg",
  theme = shinytheme("flatly"),
  
  # HOMEPAGE
  tabPanel(title = "Overview",
           fluidPage(
             h2("WhoseEgg Homepage"),
             p(" "),
             p("Welcome to the the WhoseEgg app!"),
             p(" "),
             p(" "),
             p("Add:"),
             p("- information on the random forest model"),
             p("- how to use the app"),
             p("- previous papers"),
             p("- picture(s) of fish or logo"),
             p("- etc.")
           )),
  
  # INPUT EGG CHARACTERISTICS
  tabPanel(
    
    title = "Egg Characteristics",
    h2("Input of Egg Characteristics"),
    
    ## INSTRUCTIONS
    sidebarPanel(
      h3("Instructions"),
      p("XXX Edit this text...The random forest predictions of the taxonomy 
        of the fish eggs are based on egg characteristics. This page provides 
        two ways to input the egg characteristics needed to obtain random 
        forest predictions. The values may be provided via a spreadsheet or 
        entered manually."
      ),
      p(" "),
      p(
        "See the help page for more information on the egg characteristics 
        used by the random forest and additional details about correctly 
        inputting the values."
      ),
      h3("Data Input Option"),
      selectInput(
        inputId = "input_opt",
        label = "Select a manner in which to provide predictor variables",
        choices = c("Upload spreadsheet", "Manual input (V1)", "Manual input (V2)")
      ),
      p("Note: The manual option is best suited for working with a small number of eggs."),
      width = 3
    ),
    
    ## INPUTS 
    mainPanel(
      
      #### SPREADSHEET INPUTS
      conditionalPanel(
        condition = "input.input_opt == 'Upload spreadsheet'",
        h3("Spreadsheet Input"),
        p("XXX Add some explanatory text."),
        fileInput("spreadsheet", "Select a file to upload (.csv, .xlsx, .xls)")
      ),
      
      #### MANUAL INPUTS (Version 1)
      conditionalPanel(
        condition = "input.input_opt == 'Manual input (V1)'",
        fluidPage(
          fluidRow(
            h3("Manual Input of Predictor Variables"),
            p("All variables must be specified in order to obtain a random forest prediction. XXX Add more text.")
          ),
          fluidRow(
            h4("Egg Identification"),
            column(textInput("Egg_ID", "Egg ID (any format acceptable):"), width = 4)
          ),
          fluidRow(
            h4("Environmental Information"),
            column(
              dateInput("Date", "Date of Collection (YYYY-MM-DD):", format = "yyyy-mm-dd"),
              width = 4
            ),
            column(textInput("Temperature", "Water Temperature (C):"),
                   width = 4),
            column(
              textInput("Conductivity", "Conductivity (\u03BCS/cm):"),
              width = 4
            )
          ),
          fluidRow(
            h4("Categorical Egg Measurements"),
            column(
              selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
              selectInput(
                "Compact_Diffuse",
                "Compact or Diffuse Embryo:",
                c("", "Compact", "Diffuse")
              ),
              width = 4
            ),
            column(
              selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
              selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No")),
              width = 4
            ),
            column(selectInput(
              "Egg_Stage",
              "Egg Development Stage:",
              c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "Diffuse")
            ),
            width = 4)
          ),
          fluidRow(
            h4("Quantitative Egg Measurements"),
            column(
              textInput("Membrane_Ave", "Membrane Average (mm):"),
              textInput("Membrane_SD", "Membrane Standard Deviation (mm):"),
              width = 4
            ),
            column(
              textInput("Yolk_Ave", "Embryo Average (mm):"),
              textInput("Yolk_SD", "Embryo Standard Deviation (mm):"),
              width = 4
            ),
            column(
              textInput("Larval_Length", "Late Stage Embryo Midline Length (mm):"),
              width = 4
            )
          )
        )
      ),
      
      #### MANUAL INPUTS (Version 2)
      conditionalPanel(
        condition = "input.input_opt == 'Manual input (V2)'",
        fluidPage(
          fluidRow(
            h3("Manual Input of Values"),
            p("All variables must be specified in order to obtain a random forest prediction. XXX Add more text.")
          ),
          fluidRow(
            column(
              textInput("Egg_ID", "Egg ID (any format acceptable):"), 
              dateInput("Date", "Date of Collection (YYYY-MM-DD):", format = "yyyy-mm-dd"),
              textInput("Conductivity", "Conductivity (\u03BCS/cm):"),
              textInput("Temperature", "Water Temperature (C):"),
              width = 4
              ),
            column(
              selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
              selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
              selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No")),
              selectInput("Compact_Diffuse", "Compact or Diffuse Embryo:", c("", "Compact", "Diffuse")),
              selectInput("Egg_Stage", "Egg Development Stage:", c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "Diffuse")),
              width = 4
            ),
            column(
              textInput("Membrane_Ave", "Membrane Average (mm):"),
              textInput("Membrane_SD", "Membrane Standard Deviation (mm):"),
              textInput("Yolk_Ave", "Embryo Average (mm):"),
              textInput("Yolk_SD", "Embryo Standard Deviation (mm):"),
              textInput("Larval_Length", "Late Stage Embryo Midline Length (mm):"),
              width = 4
            )
          )
        )
      ),
      
    # TABLES OF INPUTS
    h3("Egg Characteristics"),
    p("XXX Add text explaining what is contained in the two data tables."),
    p("Note that some of the variables used by the random forest a functions
          of the variables required to be provided on this page and are thus not
          required to be provided."),
    tabsetPanel(
      type = "tabs",

                # Tab for input data
                tabPanel(
                  "Input Data",
                  div(DT::dataTableOutput("input_table"), style = "font-size: 100%; width: 100%")
                ),

                # Tab for processed data
                tabPanel(
                  "Processed Data for Random Forest",
                  div(DT::dataTableOutput("processed_table"), style = "font-size: 100%; width: 100%")
                )
      )
    )
  ),
  
  # RANDOM FOREST PREDICTIONS
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
  
  # HELP PAGE
  tabPanel(
    title = "Help",
    h2("Help Page"),
    p(
      "Place to put helpful information including a glossary for egg characteristics"
    )
  )
  
)

##### ------------------------------------------------------------------------
##### APP SERVER
##### ------------------------------------------------------------------------

server <- function(input, output) {
  
  # Put the input variables in a data frame
  input_data <- reactive({
    if (input$input_opt == "Upload spreadsheet") {
      file <- input$spreadsheet
      validate(need(!is.null(file), "Please provide input values"))
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("csv", "xlsx", "xls"), "Please upload either a .csv, .xlsx, or .xls file"))
      if (ext == "csv") {
        read.csv(file$datapath)
      } else{
        readxl::read_excel(file$datapath)  
      }
    } else if (input$input_opt == "Manual input (V1)") {
      inputs_to_df(input)
    } else if (input$input_opt == "Manual input (V2)") {
      inputs_to_df(input)
    }
  })
  
  # Process the input data for the random forest 
  processed_inputs <- reactive({
    
    # Check to make sure all necessary inputs have been provided
    validate(need(
      check_for_vars(input_data()),
      paste("Currently missing the following input variables: \n", paste(get_missing_vars(input_data()), collapse = "\n "))
    ))
    
    if (check_for_vars(input_data())) {
      input_data() %>%
        compute_variables() %>%
        adjust_variable_types() %>%
        adjust_factor_levels() %>%
        sort_vars()
    } else {
      NULL
    }
  })
  
  # Obtain random forest predictions for the given inputs
  get_rf_preds <- reactive({
    
    # Prepare the inputs for the random forest
    inputs_clean <- processed_inputs()
    
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
    datatable(
      input_data() %>% mutate_all(.funs = as.character),
      options = list(
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(width = '1px', targets = "_all"))
      )
    )
  })
  
  # Create a table with the processed input values
  output$processed_table <- DT::renderDataTable({
    datatable(
      processed_inputs(),
      options = list(
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(width = '1px', targets = "_all"))
      )
    )
  })
  
  # Create a table with random forest prediction results
  output$pred_table <- renderTable({
    
    # Get the random forest predictions
    RFpreds <- get_rf_preds()
    
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
    RFpreds <- get_rf_preds()
    
    # Create the plots
    cowplot::plot_grid(
      rf_prob_plot(RFpreds$family_prob, "Family"),
      rf_prob_plot(RFpreds$genus_prob, "Genus"),
      rf_prob_plot(RFpreds$species_prob, "Species"),
      ncol = 1
    )
    
  }, height = 800)
  
}

##### ------------------------------------------------------------------------
##### RUN APP
##### ------------------------------------------------------------------------

shinyApp(ui, server)
