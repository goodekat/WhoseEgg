
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
    
    title = "Egg Characteristics",
    h2("Input of Egg Characteristics"),
    
    ## INSTRUCTIONS
    sidebarPanel(
      h3("Instructions"),
      p("XXX Edit this text...The random forest predictions of the taxonomy 
        of the fish eggs are based on egg characteristics."
      ),
      p(" "),
      p(
        "See the help page for more information on the egg characteristics 
        used by the random forest and additional details about correctly 
        inputting the values."
      ),
      width = 3
    ),
    
    ## INPUTS 
    mainPanel(
      ### SPREADSHEET INPUTS
      h3("Spreadsheet Input"),
      p("XXX Add some explanatory text."),
      fileInput("spreadsheet", "Select a file to upload (.csv, .xlsx, .xls)"),
      
      # TABLES OF INPUTS
      h3("Egg Characteristics"),
      p("XXX Add text explaining what is contained in the two data tables."),
      p(
        "Note that some of the variables used by the random forest a functions
          of the variables required to be provided on this page and are thus not
          required to be provided."
      ),
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
    fluidRow(
      column(
        h2("Random Forest Predictions"),
        DT::dataTableOutput("pred_table"),
        width = 12
      )
    ),
    fluidRow(
      column(
        h3("Random forest probabilities for all taxonomic levels"),
        plotOutput("prob_plot"),
        width = 12
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
  output$pred_table <- DT::renderDataTable({
    
    # Get the random forest predictions
    RFpreds <- get_rf_preds()
    
    # Put the random forest results in a table
    data.frame(
      'Egg ID' = "to do",
      "Family" = RFpreds$family_pred,
      "Family Probability" = RFpreds$family_prob %>% pull(RFpreds$family_pred),
      "Genus" = RFpreds$genus_pred,
      "Genus Probability" =  RFpreds$genus_prob %>% pull(RFpreds$genus_pred),
      "Species" = RFpreds$species_pred,
      "Species Probability" = RFpreds$species_prob %>% pull(str_replace(RFpreds$species_pred, " ", ".")),
      'Prediction Interval' = 'to do',
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
      ncol = 3
    )
    
  }, height = 300)
  
}

##### ------------------------------------------------------------------------
##### RUN APP
##### ------------------------------------------------------------------------

shinyApp(ui, server)
