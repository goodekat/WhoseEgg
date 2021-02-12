
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
        "3. Upload the completed spreadsheet.",
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
          downloadButton("downloadTemplate", "Download Template"),
          br(),
          h3("Spreadsheet Upload"),
          p("Use the button below to provide a completed spreadsheet."),
          p(em("There is no maximum number for the number of observations in 
               a spreadsheet.")),
          fileInput("spreadsheet", "Select a file to upload (.csv, .xlsx, .xls)"),
          width = 12
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
      h3("Instructions"),
      p(
        "This page is used to compute and display the random forest predictions
        for the data provided via the 'Data Input' tab. Visualizations of the 
        predictions are included for further exploration of the predictions.",
        br(),
        br(),
        strong("Follow these steps to view the random forest predictions:"), 
        br(),
        br(),
        "1. Make sure to provide input data using the 'Data Input' tab and view the
        processed data to check for correctness.",
        br(),
        br(),
        "2. Click the button below to generate the random forest predictions.",
        br(),
        br(),
        actionButton("getpreds", "Get Predictions"),
        br(),
        br(),
        "3. View the table and visualizations of the predictions that will appear 
        in the main panel of this page."
      ),
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
    sidebarPanel(
      h3("Instructions"),
      width = 3
    ),
    mainPanel(
      h2("Downloads"),
      br(),
      downloadButton("downloadPreds", "Download Predictions")
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
  
  ## INPUTS ------------------------------------------------------------------
  
  # Template download
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("WhoseEggtemplate", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("../data/template.xlsx", file)
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
      paste(
        "Currently missing the following input variables: \n", 
        paste(get_missing_vars(input_data()), collapse = "\n ")
      )
    ))
    # Process the inputs as needed for the random forest
    input_data() %>%
      compute_variables() %>%
      adjust_variable_types() %>%
      adjust_factor_levels() %>%
      sort_vars()
  })
  
  # Create a table with the input values
  output$input_table <- DT::renderDataTable({
    input_data() %>%
      mutate_all(.funs = as.character) %>%
      datatable(
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
    processed_inputs() %>%
      select(all_of(rf_pred_vars)) %>%
      datatable(
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          autoWidth = FALSE,
          columnDefs = list(list(width = '1px', targets = "_all"))
        )
      )
  })
  
  ## PREDICTIONS -------------------------------------------------------------
  
  # Obtain random forest predictions for the given inputs
  data_and_preds <- reactive({
    # Prepare the inputs for the random forest
    inputs_clean <- processed_inputs() %>% select(all_of(rf_pred_vars))
    # Get the predictions and random forest probabilities
    pred_list <-
      list(
        family_pred  = as.character(predict(rfs$Family_ACGC, inputs_clean)),
        genus_pred   = as.character(predict(rfs$Genus_ACGC, inputs_clean)),
        species_pred = as.character(predict(rfs$Common_Name_ACGC, inputs_clean)),
        family_prob  = data.frame(predict(rfs$Family_ACGC, inputs_clean, type = "prob")),
        genus_prob   = data.frame(predict(rfs$Genus_ACGC, inputs_clean, type = "prob")),
        species_prob = data.frame(predict(rfs$Common_Name_ACGC, inputs_clean, type = "prob"))
      )
    # Put the predictions in a data frame with the input values
    processed_inputs() %>%
      mutate(
        Family_Pred = pred_list$family_pred,
        Family_Prob = get_rf_prob(pred_list, "family"),
        Genus_Pred   = pred_list$genus_pred,
        Genus_Prob = get_rf_prob(pred_list, "genus"),
        Species_Pred = pred_list$species_pred,
        Species_Prob = get_rf_prob(pred_list, "species")
      ) %>%
      bind_cols(
        pred_list$family_prob %>% rename_all(.funs = function(x) paste0("Family_Prob_", x)),
        pred_list$genus_prob %>% rename_all(.funs = function(x) paste0("Genus_Prob_", x)),
        pred_list$species_prob %>% rename_all(.funs = function(x) paste0("Species_Prob_", x))
      )
  })
  
  # Display predictions after button is clicked
  observeEvent(input$getpreds, { 
    # Create a table with random forest prediction results
    output$pred_table <- DT::renderDataTable({
      # Put the random forest results in a table
      data_and_preds() %>%
        select(
          Egg_ID,
          Family_Pred,
          Family_Prob,
          Genus_Pred,
          Genus_Prob,
          Species_Pred,
          Species_Prob
        ) %>%
        datatable(
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
      rf_pred_plot(data_and_preds())
    })
    
    # Create plots with the random forest probabilities for all taxonomic levels
    output$prob_plot <- renderPlot({
      # Check to make sure all necessary inputs have been provided
      validate(need(
        !is.null(input$pred_table_rows_selected),
        "Please select a row in the table of predictions to view plots."
      ))
      # Create the plots
      rf_prob_plot(data_and_preds(), input$pred_table_rows_selected)
    })
  })
  
  ## DOWNLOADS ---------------------------------------------------------------
  
  # Data frame with prediction download
  output$downloadPreds <- downloadHandler(
    filename = function() {
      paste("WhoseEggPredictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_and_preds(), file, row.names = FALSE)
    }
  )

}

##### ------------------------------------------------------------------------
##### RUN APP
##### ------------------------------------------------------------------------

shinyApp(ui, server)
