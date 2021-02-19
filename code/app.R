
##### SET UP #####

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

##### APP UI #####

ui <- navbarPage(
  
  ## FORMAT INFO
  title = "WhoseEgg",
  theme = shinytheme("flatly"),
  
  ## HOMEPAGE
  tabPanel(title = "Overview",
           fluidPage(
               img(src="5-eggs-in-a-row.png",width="800px"),
               br(),
               column(
                 h2("Welcome to the WhoseEgg App"),
                 p("WhoseEgg is a Shiny app for predicting the taxonomy of fish eggs 
                 to identiy invasive carp eggs. The predictions are based on a 
                 collection of characteristics and provided via the use of random 
                 forest models. The models are based on Camacho et al. (2019),
                 who successfully use random forests to identify invasive carp, and
                 Goode et al. (2021), who validate the models from Camacho et al. (2019)."),
                 h3("How to use the app"),
                 p("In order to obtain predictions for a set of eggs, follow the 
                 three step process outlined in the flow chart below.
                 Additional details about the steps are provided on the tab
                 corresponding to the step. See the help page for details about 
                 the egg characteristics."),
                 img(src="steps.png",width="800px"),
                 br(),
                 h3("References"),
                 p("Camacho, C.A., Sullivan, C.J., Weber, M.J. and Pierce, C.L. (2019), 
                   Morphological Identification of Bighead Carp, Silver Carp, and Grass
                   Carp Eggs Using Random Forests Machine Learning Classification. North 
                   Am J Fish Manage, 39: 1373-1384. https://doi.org/10.1002/nafm.10380"),
                 p("Goode, K.J., Weber, M.J., Matthews, A., and Pierce, C.L. (2021), XXX"),
                 width = 8
               ),
           )),
  
  ## INPUT EGG CHARACTERISTICS
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
      conditionalPanel(
        condition = "!is.na(output.need_data)", 
        span(textOutput("need_data"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_file_type_v1)", 
        span(textOutput("error_file_type_v1"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_egg_id_v1)", 
        span(textOutput("error_missing_egg_id_v1"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_vars_v1)", 
        span(textOutput("error_missing_vars_v1"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(error_wrong_fct_levels_v1)", 
        span(textOutput("error_wrong_fct_levels_v1"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.warning_missing_vals_v1)", 
        span(textOutput("warning_missing_vals_v1"), style = "color:#3498db")
      ), 
      conditionalPanel(
        condition = "!is.na(output.warning_vars_outside_ranges_v1)", 
        span(textOutput("warning_vars_outside_ranges_v1"), style = "color:#3498db")
      ), 
      br(),
      tabsetPanel(
        type = "tabs",
        # Tab for input data
        tabPanel(
          "Input Data",
          conditionalPanel(
            condition = "!is.na(output.message_provide_data_v1)", 
            span(textOutput("message_provide_data_v1"), style = "color:grey")
          ),
          div(DT::dataTableOutput("input_table"), style = "font-size: 100%; width: 100%")
        ),
        # Tab for processed data
        tabPanel(
          "Processed Data",
          conditionalPanel(
            condition = "!is.na(output.message_provide_data_v2)", 
            span(textOutput("message_provide_data_v2"), style = "color:grey")
          ),
          div(DT::dataTableOutput("processed_table"), style = "font-size: 100%; width: 100%")
        )
      ),
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
        in the main panel of this page.",
        br(),
        br(),
        em("Note: If a new spreadsheet is provided after predictions have been computed once, the predictions will be automatically updated.")
      ),
      width = 3
    ),
    mainPanel(
      h2("Results from Random Forests"),
      conditionalPanel(
        condition = "!is.na(output.error_file_type_v2)", 
        span(textOutput("error_file_type_v2"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_egg_id_v2)", 
        span(textOutput("error_missing_egg_id_v2"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_vars_v2)", 
        span(textOutput("error_missing_vars_v2"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(error_wrong_fct_levels_v2)", 
        span(textOutput("error_wrong_fct_levels_v2"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(output.warning_missing_vals_v2)", 
        span(textOutput("warning_missing_vals_v2"), style = "color:#3498db")
      ), 
      conditionalPanel(
        condition = "!is.na(output.warning_vars_outside_ranges_v2)", 
        span(textOutput("warning_vars_outside_ranges_v2"), style = "color:#3498db")
      ), 
      fluidRow(
        column(
          h3("Table of Predictions"),
          br(),
          conditionalPanel(
            condition = "!is.na(output.message_pred_table)", 
            span(textOutput("message_pred_table"), style = "color:grey")
          ),
          div(DT::dataTableOutput("pred_table"), style = "font-size: 100%; width: 100%"),
          width = 12
        )
      ),
      h3("Visualizations of Predictions"),
      br(),
      conditionalPanel(
        condition = "!is.na(output.message_pred_plots)", 
        span(textOutput("message_pred_plots"), style = "color:grey")
      ),
      br(),
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

##### APP SERVER #####

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
    ext <- tools::file_ext(file$datapath)
    req(file)
    if (ext == "csv") {
      read.csv(file$datapath)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(file$datapath)
    } else{
      NULL
    }
  })
  
  # Process the input data for the random forest 
  processed_inputs <- reactive({
    if (!is.null(input_data())) {
      # Check for correct data entry
      validate(need(check_for_vars(input_data()), message = FALSE))
      validate(need(check_fct_levels(input_data()), message = FALSE))
      validate(need(check_for_egg_ids(input_data()), message = FALSE))
      # Process the inputs as needed for the random forest
      input_data() %>%
        compute_variables() %>%
        adjust_variable_types() %>%
        adjust_factor_levels() %>%
        sort_vars()
    }
  })
  
  # Create a table with the input values
  output$input_table <- DT::renderDataTable({
    if (!is.null(input_data())){
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
    }
  })
  
  # Create a table with the processed input values
  output$processed_table <- DT::renderDataTable({
    if (!is.null(input_data())) {
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
    }
  })
  
  ## PREDICTIONS -------------------------------------------------------------
  
  # Obtain random forest predictions for the given inputs
  data_and_preds <- reactive({
    if (!is.null(input_data())) {
      # Prepare the inputs for the random forest
      inputs_clean <- processed_inputs() %>% select(all_of(rf_pred_vars)) %>% na.omit()
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
      preds <- 
        data.frame(
          Egg_ID = inputs_clean$Egg_ID,
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
      left_join(processed_inputs(), preds, by = "Egg_ID")
    }
  })
  
  # Display predictions after button is clicked
  observeEvent(input$getpreds, { 
    if (!is.null(input_data())) {
      # Create a table with random forest prediction results
      output$pred_table <- DT::renderDataTable({
        # Put the random forest results in a table
        data_and_preds() %>%
          na.omit() %>%
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
        rf_pred_plot(na.omit(data_and_preds()))
      })
      
      # Create plots with the random forest probabilities for all taxonomic levels
      output$prob_plot <- renderPlot({
        # Check to make sure all necessary inputs have been provided
        validate(need(
          !is.null(input$pred_table_rows_selected) |
            !check_for_vars(input_data()) |
            !check_fct_levels(input_data()) | 
            !check_for_egg_ids(input_data()), 
          "Please select a row in the table of predictions to view plots."
        ))
        #Create the plots
        rf_prob_plot(na.omit(data_and_preds()), input$pred_table_rows_selected)
      })
    }
    
  })
  
  ## MESSAGES ----------------------------------------------------------------
  
  # Messages when no spreadsheet provided
  message_provide_data <- reactive({
    if (is.null(input$spreadsheet)) {
      "Please provide input values"
    } else if (is.null(input_data())) {
      "Please provide input values"
    } else NA
  })
  output$message_provide_data_v1 <- message_provide_data
  output$message_provide_data_v2 <- message_provide_data
  outputOptions(output, "message_provide_data_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "message_provide_data_v2", suspendWhenHidden = FALSE)
  
  # Provide a message where prediction table will be
  output$message_pred_table <- reactive({
    if (input$getpreds == 0 | is.null(input$spreadsheet)) {
      "A table with predictions will appear here after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else if (is.null(input_data()) |
               !check_for_vars(input_data()) |
               !check_fct_levels(input_data()) | 
               !check_for_egg_ids(input_data())) {
      "A table with predictions will appear here after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else {
      NA
    }
  })
  outputOptions(output, "message_pred_table", suspendWhenHidden = FALSE)
  
  # Provide a message where prediction visualizations will be
  output$message_pred_plots <- reactive({
    if (input$getpreds == 0 | is.null(input$spreadsheet)) {
      "Visualizations of predictions will appear below after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else if (is.null(input_data()) |
               !check_for_vars(input_data()) | 
               !check_for_egg_ids(input_data()) |
               !check_fct_levels(input_data())) {
      "Visualizations of predictions will appear below after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else {
      NA
    }
  })
  outputOptions(output, "message_pred_plots", suspendWhenHidden = FALSE)

  ## WARNINGS ----------------------------------------------------------------
  
  # Check for missing values
  warning_missing_vals <- reactive({
    if (!is.null(input_data())) {
      if (sum(is.na(processed_inputs())) > 0) {
        "Warning: Missing values detected in the processed data. 
        Random forests cannot return predictions for observations with missing values.
        These observations will be excluded on the 'Predictions' page."
      } else NA
    }
  })
  output$warning_missing_vals_v1 <- warning_missing_vals
  output$warning_missing_vals_v2 <- warning_missing_vals 
  outputOptions(output, "warning_missing_vals_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_missing_vals_v2", suspendWhenHidden = FALSE)
  
  # Check to make sure all necessary inputs have been provided
  warning_vars_outside_ranges <- reactive({
    if (!is.null(input_data())) {
      if (!check_var_ranges(processed_inputs())) {
        paste(
          "Warning: Some continuous variables outside of ranges in training data.
          This will lead to model extrapolation and possibly poor predictions.
          Egg IDs with values outside of ranges: ",
          paste(get_outside_var_ranges(input_data()), collapse = ", ")
        )
      } else {
        NA
      }
    }
  })
  output$warning_vars_outside_ranges_v1 <- warning_vars_outside_ranges
  output$warning_vars_outside_ranges_v2 <- warning_vars_outside_ranges
  outputOptions(output, "warning_vars_outside_ranges_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_vars_outside_ranges_v2", suspendWhenHidden = FALSE)
  
  ## ERRORS ------------------------------------------------------------------
  
  # Check that an appropriate file type was provided
  error_file_type <- reactive({
    if (is.null(input_data())) {
      "Error: Please provide a .csv, .xlsx, or .xls file."
    } else { NA }
  })
  output$error_file_type_v1 <- error_file_type
  output$error_file_type_v2 <- error_file_type
  outputOptions(output, "error_file_type_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_file_type_v2", suspendWhenHidden = FALSE)
  
  # Check all egg ids provided
  error_missing_egg_id <- reactive({
    if (!is.null(input_data())) {
      if (!check_for_egg_ids(input_data())) {
        "Egg_ID missing: Must provide all Egg IDs"
      } else { NA }
    }
  })
  output$error_missing_egg_id_v1 <- error_missing_egg_id
  output$error_missing_egg_id_v2 <- error_missing_egg_id
  outputOptions(output, "error_missing_egg_id_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_egg_id_v2", suspendWhenHidden = FALSE)
  
  # Check to make sure all necessary inputs have been provided
  error_missing_vars <- reactive({
    if (!is.null(input_data())) {
      if (!check_for_vars(input_data())) {
        paste(
          "Error: Currently missing the following input variables: \n", 
          paste(get_missing_vars(input_data()), collapse = ", ")
        )
      } else { NA }
    }
  })
  output$error_missing_vars_v1 <- error_missing_vars
  output$error_missing_vars_v2 <- error_missing_vars
  outputOptions(output, "error_missing_vars_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_vars_v2", suspendWhenHidden = FALSE)
  
  # Check to make sure all necessary inputs have been provided
  error_wrong_fct_levels <- reactive({
    if (!is.null(input_data())) {
      if (!check_fct_levels(input_data())) {
        paste(
          "Error: Categorical variable levels found in data that are not permitted: \n", 
          paste(get_wrong_fct_levels(input_data()), collapse = ", ")
        )
      } else { NA }
    }
  })
  output$error_wrong_fct_levels_v1 <- error_wrong_fct_levels
  output$error_wrong_fct_levels_v2 <- error_wrong_fct_levels
  outputOptions(output, "error_wrong_fct_levels_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_wrong_fct_levels_v2", suspendWhenHidden = FALSE)
  
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

##### RUN APP #####

shinyApp(ui, server)
