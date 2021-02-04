
##### ------------------------------------------------------------------------
##### SET UP
##### ------------------------------------------------------------------------

# Load packages
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(purrr)
library(randomForest)
library(shiny)
library(shinythemes)
library(stringr)
library(tidyr)

# Source the helper functions used by the app
source("../code/helper-functions.R")

# Load the random forest models (trained on the years of 2014-2016)
rfs <- readRDS("../data/rfs_for_app.rds")

# Load data template
template <- read.csv("../data/template.csv")

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
    
    title = "Data Input",
    
    ## INSTRUCTIONS
    sidebarPanel(
      h3("Instructions"),
      p("This page contains the tools for providing the fish egg characteristics
        used by the random forests to make predictions of the fish taxonomies.",
        br(),
        br(),
        strong("Follow these steps to provide the egg characterstics:"), 
        br(),
        br(),
        "1. Download a spreadsheet template.",
        br(),
        br(),
        "2. Add observed egg characteristics to the downloaded spreadsheet.",
        br(),
        br(),
        "3. Uplaod the completed spreadsheet.",
        br(),
        br(),
        "4. Preview the input and processed data to check for correctness.",
        br(),
        br(),
        em("See the help page for additional details on the egg characteristics,
        which includes defintions, units, and example photos.")
      ),
      width = 3
    ),
    
    ## INPUTS 
    mainPanel(
      
      h2("Input of Egg Characteristics"),
      
      ### SPREADSHEET INPUTS
      fluidRow(
        column(
          h3("Template Download"),
          p("The spreadsheet template contains columns for an egg ID and 13 
            egg characteristics. After uploading the completed spreadhsheet, 
            some additional will be computed. See the 'Processed Data' tab 
            below for the complete set of 18 random forest predictor variables."),
          p(em("It is okay to include additional variables, but they will be 
               excluded prior to processing for the random forest.")),
          downloadButton("downloadData", "Download Template"),
          width = 6
        ),
        column(
          h3("Spreadsheet Upload"),
          p("Use the button below to provide a completed spreadsheet."),
          p(em("There is no maximum number for the number of observations in 
               a spreadsheet.")),
          fileInput("spreadsheet", "Select a file to upload (.csv, .xlsx, .xls)"),
          width = 6
        )
      ),
      
      # TABLES OF INPUTS
      h3("Egg Characteristics"),
      p("The 'Input Data' tab below contains the data uploaded to WhoseEgg. The 
        'Processed Data' contains the egg ID and the predictor variables that 
        will be used by the random forest. Some of the predictor variables are
        computed from the input data."),
      tabsetPanel(
        type = "tabs",
        
        # Tab for input data
        tabPanel(
          "Input Data",
          div(DT::dataTableOutput("input_table"), style = "font-size: 100%; width: 100%")
        ),
        
        # Tab for processed data
        tabPanel(
          "Processed Data",
          div(DT::dataTableOutput("processed_table"), style = "font-size: 100%; width: 100%")
        )
      )
    )
  ),
  
  # RANDOM FOREST PREDICTIONS
  tabPanel(
    title = "Predictions",
    sidebarPanel(
      h3("How to use this page"),
      width = 3
    ),
    mainPanel(
      h2("Results from Random Forests"),
      fluidRow(
        column(
          h3("Table of Predictions"),
          div(DT::dataTableOutput("pred_table"), style = "font-size: 100%; width: 100%"),
          width = 12
        )
      ),
      h3("Visualizations of Predictions"),
      tabsetPanel(
        type = "tabs",
        
        # Tab for summary visualizations
        tabPanel(
          "Summary of predictions",
          plotOutput("pred_plot"),
          width = 12
        ),
        
        # Tab individual probabilities
        tabPanel(
          "Individual egg predictions",
          plotOutput("prob_plot"),
          width = 12
        )
      ) 
    )
  ),
  
  # DOWNLOADS PAGE
  tabPanel(
    title = "Downloads",
    h2("Downloads"),
    p(
      "Page for downloading the processed data and predictions"
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
  
  # Template download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("WhoseEggtemplate", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Put the input variables in a data frame
  input_data <- reactive({
    file <- input$spreadsheet
    validate(need(!is.null(file), "Please provide input values"))
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(
      ext %in% c("csv", "xlsx", "xls"),
      "Please upload either a .csv, .xlsx, or .xls file"
    ))
    if (ext == "csv") {
      read.csv(file$datapath)
    } else{
      readxl::read_excel(file$datapath)
    }
  })
  
  # Process the input data for the random forest 
  processed_inputs <- reactive({
    
    # Check to make sure all necessary inputs have been provided
    validate(need(
      check_for_vars(input_data()),
      paste("Currently missing the following input variables: \n", paste(get_missing_vars(input_data()), collapse = "\n "))
    ))
    
    input_data() %>%
      compute_variables() %>%
      adjust_variable_types() %>%
      adjust_factor_levels() %>%
      sort_vars()
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
        pageLength = 5,
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
        pageLength = 5,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(width = '1px', targets = "_all"))
      )
    )
  })
  
  # Create a table with random forest prediction results
  output$pred_table <- DT::renderDataTable({

    # Get the random forest predictions
    RFpreds <- get_rf_preds()

    # Put the random forest results in a table
    datatable(
      data.frame(
        'Egg ID' = processed_inputs()$Egg_ID,
        "Family" = RFpreds$family_pred,
        "Family Probability" = get_rf_prob(RFpreds, "family"),
        "Family Pred Int" = "to do",
        "Genus" = RFpreds$genus_pred,
        "Genus Probability" = get_rf_prob(RFpreds, "genus"),
        "Genus Pred Int" = "to do",
        "Species" = RFpreds$species_pred,
        "Species Probability" = get_rf_prob(RFpreds, "species"),
        "Species Pred Int" = "to do",
        check.names = FALSE
      ),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        columnDefs = list(list(width = '1px', targets = "_all"))
      ),
      selection = 'single'
    )
    
  })
  
  # Create plots summarizing the random forest predictions
  output$pred_plot <- renderPlot({
    
    # Get the random forest predictions
    RFpreds <- get_rf_preds()
    
    # Create the plots
    rf_pred_plot(RFpreds)
    
  })
  
  # Create plots with the random forest probabilities for all taxonomic levels
  output$prob_plot <- renderPlot({

    # Check to make sure all necessary inputs have been provided
    validate(need(
      !is.null(input$pred_table_rows_selected),
      "Please select a row in the table of predictions to view plots."
    ))
    
    # Get the random forest predictions
    RFpreds <- get_rf_preds()

    # Create the plots
    rf_prob_plot(RFpreds, input$pred_table_rows_selected)

  })

}

##### ------------------------------------------------------------------------
##### RUN APP
##### ------------------------------------------------------------------------

shinyApp(ui, server)
