
##### SET UP #####

# Load packages
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(markdown)
library(purrr)
library(randomForest)
library(shiny)
library(shinythemes)
library(stringr)
library(tidyr)

# Source the helper functions used by the app
source("helper-functions.R")

# Load the random forest models (trained on the years of 2014-2016)
rfs <- readRDS("data/rfs_for_app.rds")

# Prepare text files
rmdfiles <- c("text/05-help-faq.Rmd")
knitr::knit(rmdfiles, output = "text/05-help-faq.md", quiet = T)

##### APP UI #####

ui <- navbarPage(
  
  ## FORMAT INFO
  title = "WhoseEgg",
  id = "inTabset",
  theme = shinytheme("flatly"),
  position = "fixed-top",
  
  ## HOMEPAGE
  tabPanel(
    
    # Google analytics
    tags$head(includeHTML("google-analytics.html")),
    
    # Add padding to work with fixed upper panel
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    
    title = div("Overview", style = "font-size:14px;"),
    value = "overview",
    fluidPage(
      br(),
      column(width = 1),
      column(
        width = 9,
        img(src = "eggs-in-a-row.jpeg", width = "900px"),
        h3(strong("Welcome to the WhoseEgg App")),
        span(
          includeMarkdown("text/01-overview-header.Rmd"),
          "For information about the random forest models, information on how 
          the egg characteristics were measured for training the random forests,
          and handling data from different locations, see the",
          actionLink("overview2help", "help page"),
          ".",
          style = "font-size:14px;"
        ),
        br(),
        br(),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            h5("How to Use the App"),
            br(),
            p(
              "Follow the steps below to obtain predictions. Additional instructions
              are inlcuded on the page corresponding to a step.",
              style = "font-size:14px;"
            ),
            img(src = "flow-chart.jpeg", width = "800px"),
            br(),
            br()
          ), 
          tabPanel(
            h5("Locations in Training Data"),
            br(),
            span(
              includeMarkdown("text/01-overview-locations.Rmd"), 
              "For more information on using WhoseEgg with data collected in 
              different regions, see the FAQ on the",
              actionLink("overview2helpagain", "help page"), ".",
              style = "font-size:14px;"
            ),
            br(),
            br(),
            img(src = "locations.jpeg", width = "600px")
          ), 
          tabPanel(
            h5("Species in Training Data"),
            br(),
            span(
              includeMarkdown("text/01-overview-species.Rmd"), 
              "For more information on using WhoseEgg with data collected in 
              locations where additional species may be present, see the FAQ on the",
              actionLink("overview2helpagainx2", "help page"), ".",
              br(),
              br(),
              tableOutput("species_table"),
              style = "font-size:14px;"
            )
          ),
          tabPanel(
            h5("User Tips"),
            br(),
            span(includeMarkdown("text/01-overview-tips.Rmd"), style = "font-size:14px;")
          ),
          tabPanel(
            h5("Contributors and Contact"),
            br(),
            span(includeMarkdown("text/01-overview-cc.Rmd"), style = "font-size:14px;")
          )
        ),
        hr(),
        span(
          p(em("Funding for WhoseEgg was provided by the U.S. Fish and Wildlife Service through Grant #F20AP11535-00.")),
          p(em("Data privacy statement: Data uploaded to WhoseEgg will not be saved by WhoseEgg or distributed.")),
          style = "font-size:14px;"
        )
      )
    )
  ), 
  
  ## INPUT EGG CHARACTERISTICS
  tabPanel(
    
    title = div("Data Input", style = "font-size:14px;"),
    value = "inputs",
    
    ## INSTRUCTIONS
    sidebarPanel(
      style = "position:fixed;width:22%;",
      h3("Instructions"),
      span(
      p(
        "1. Download a spreadsheet template.",
        br(),
        br(),
        downloadButton("downloadTemplate", "Download Template"),
        br(),
        br(),
        "2. Add observed values to downloaded spreadsheet or a similarly 
        formatted spreadsheet following the spreadsheet specifications 
        in the main panel.", 
        br(),
        br(),
        span(
          em("Note: See the", actionLink("input2help", "help page"),
          "for detailed information on the egg characteristics.")
        ),
        br(),
        br(),
        "3. Upload a completed spreadsheet (saved as .csv, 
        .xlsx, or .xls).",
        br(),
        fileInput("spreadsheet", ""),
        "4. Preview the input and processed data displayed in the main 
        panel to check for correctness.",
        br(),
        br(),
        "5. Go to 'Predictions' tab to obtain predictions.",
        br(),
        br(),
        actionButton('jump2pred', 'Jump to Predictions Tab')
      ), style = "font-size:14px;"),
      width = 3
    ),
    
    ## INPUTS 
    mainPanel(
      h3(strong("Input of Egg Characteristics")),
      conditionalPanel(
        condition = "!is.na(output.need_data)", 
        span(textOutput("need_data"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_file_type_v1)", 
        span(textOutput("error_file_type_v1"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_egg_id_v1)", 
        span(textOutput("error_missing_egg_id_v1"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_vars_v1)", 
        span(textOutput("error_missing_vars_v1"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(error_wrong_fct_levels_v1)", 
        span(textOutput("error_wrong_fct_levels_v1"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_na_in_dates_v1)", 
        span(textOutput("error_na_in_dates_v1"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.warning_missing_vals_v1)", 
        span(textOutput("warning_missing_vals_v1"), style = "color:#f39c13")
      ), 
      conditionalPanel(
        condition = "!is.na(output.warning_vars_outside_ranges_v1)", 
        span(textOutput("warning_vars_outside_ranges_v1"), style = "color:#f39c13")
      ),
      hr(),
      h4("Overview"),
      span(
        includeMarkdown("text/02-input-header.Rmd"),
        style = "font-size:14px;"
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          h4("Spreadsheet Specifications"),
          tabsetPanel(
            type = "tabs",
            tabPanel("Variable Requirements", br(), span(includeMarkdown("text/02-input-variables.Rmd"), style = "font-size:14px;")),
            tabPanel("Observation Requirements", br(), span(includeMarkdown("text/02-input-observations.Rmd"), style = "font-size:14px;")),
            tabPanel("Template Helpers", br(), span(includeMarkdown("text/02-input-template-helpers.Rmd"), style = "font-size:14px;")),
            tabPanel("Additional Variables", br(), span(includeMarkdown("text/02-input-additional-vars.Rmd"), style = "font-size:14px;"))
          ),
          hr(),
          h4("Egg Characteristics"),
          tabsetPanel(
            type = "tabs",
            # Tab for input data
            tabPanel(
              "Input Data",
              conditionalPanel(
                condition = "!is.na(output.message_provide_data_v1)", 
                span(textOutput("message_provide_data_v1"), style = "color:grey")
              ),
              div(DT::dataTableOutput("input_table"), style = "font-size: 100%; width: 100%"),
              br(),
              br()
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
      )
    )
  ),
  
  # RANDOM FOREST PREDICTIONS
  tabPanel(
    
    title = div("Predictions", style = "font-size:14px;"),
    value = "predictions",
    
    sidebarPanel(
      style = "position:fixed;width:22%;",
      h3("Instructions"),
      span(
      p(
        "1. Provide egg data using the 'Data Input' tab and view the
        processed data to check for correctness.",
        br(),
        br(),
        "2. Click the button below to generate the random forest predictions.",
        br(),
        br(),
        actionButton("getpreds", "Get Predictions"),
        br(),
        br(),
        span(
          em(strong("Note:"), "If a new spreadsheet is provided after predictions have been
           computed once, the predictions will be automatically updated."),
          style = 'color:#3498db'
        ),
        br(),
        br(),
        "3. View the table and visualizations of the predictions that will appear 
        in the main panel of this page.",
        br(),
        br(),
        "4. Go to 'Downloads' tab to download data with predictions.",
        br(),
        br(),
        actionButton('jump2download', 'Jump to Downloads Tab')
      ), style = "font-size:14px;"),
      width = 3
    ),
    mainPanel(
      h3(strong("Results from Random Forests")),
      conditionalPanel(
        condition = "!is.na(output.error_file_type_v2)", 
        span(textOutput("error_file_type_v2"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_egg_id_v2)", 
        span(textOutput("error_missing_egg_id_v2"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_vars_v2)", 
        span(textOutput("error_missing_vars_v2"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(error_wrong_fct_levels_v2)", 
        span(textOutput("error_wrong_fct_levels_v2"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_na_in_dates_v2)", 
        span(textOutput("error_na_in_dates_v2"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.warning_missing_vals_v2)", 
        span(textOutput("warning_missing_vals_v2"), style = "color:#f39c13")
      ), 
      conditionalPanel(
        condition = "!is.na(output.warning_vars_outside_ranges_v2)", 
        span(textOutput("warning_vars_outside_ranges_v2"), style = "color:#f39c13")
      ),
      hr(),
      h4("Overview"),
      span(
        includeMarkdown("text/03-predictions-header.Rmd"),
        style = "font-size:14px;"
      ),
      hr(),
      fluidRow(
        column(
          h4("Table of Predictions"),
          column(
            conditionalPanel(
              condition = "!is.na(output.message_pred_table)", 
              span(textOutput("message_pred_table"), style = "color:grey")
            ), width = 9
          ),
          div(DT::dataTableOutput("pred_table"), style = "font-size: 100%; width: 100%"),
          width = 12
        )
      ),
      hr(),
      h4("Visualizations of Predictions"),
      tabsetPanel(
        type = "tabs",
        # Tab for summary visualizations
        tabPanel(
          "Summary of predictions",
          br(),
          span(
            strong("Frequency of taxonomic level predictions"),
            br(),
            br(),
            "Each plot shows the levels of family, genus, and
            species represented by the predictions. The length of the 
            bars represent the total number of eggs predicted to be 
            within a level.",
            style = "font-size:14px;"
          ),
          br(),
          br(),
          column(
            conditionalPanel(
              condition = "!is.na(output.message_pred_plots_v1)", 
              span(textOutput("message_pred_plots_v1"), style = "color:grey")
            ), 
            width = 10
          ),
          plotOutput("pred_plot"),
          width = 12
        ),
        
        # Tab individual probabilities
        tabPanel(
          "Individual egg predictions",
          br(),
          span(
            strong("Random forest probabilities of all taxonomic levels for 
                   specified observation"),
            br(),
            br(),
            "The random forests return probabilities for all taxonomic levels for 
            each egg observation. These graphics show the probabilities for each 
            taxonomic level for an egg. The levels with a taxonomy are ordered 
            from top to bottom by highest to lowest random forest probability.",
            style = "font-size:14px;"
          ),
          br(),
          br(),
          column(
            conditionalPanel(
              condition = "!is.na(output.message_pred_plots_v2)", 
              span(textOutput("message_pred_plots_v2"), style = "color:grey")
            ), 
            width = 12
          ),
          plotOutput("prob_plot"),
          width = 12
        )
      ) 
    )
  ),
  
  # DOWNLOADS PAGE
  tabPanel(
    
    title = div("Downloads", style = "font-size:14px;"),
    value = "downloads",
    
    sidebarPanel(
      style = "position:fixed;width:22%;",
      h3("Instructions"),
      span(
      p(
        "1. Provide egg data using the 'Data Input' tab and compute random
        forest predictions using the 'Predictions' tab.",
        br(),
        br(),
        "2. Click the button below to preview a table with the egg data and
        predictions joined.",
        br(),
        br(),
        actionButton("preview", "Preview Data"),
        br(),
        br(),
        "3. Specify whether to download the spreadhseet as an Excel or csv file.",
        selectInput("download_file_type", " ", c("xlsx" = "xlsx", "xls" = "xls", "csv" = "csv")),
        "4. Click the button below to download the prepared spreadsheet.",
        br(),
        br(),
        downloadButton("downloadPreds", "Download Predictions")
      ), style = "font-size:14px;"),
      width = 3
    ),
    mainPanel(
      h3(strong("Download Data with Predictions")),
      conditionalPanel(
        condition = "!is.na(output.warning_vars_outside_ranges_v3)", 
        span(textOutput("warning_vars_outside_ranges_v3"), style = "color:#f39c13")
      ),
      conditionalPanel(
        condition = "!is.na(error_wrong_fct_levels_v3)", 
        span(textOutput("error_wrong_fct_levels_v3"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_vars_v3)", 
        span(textOutput("error_missing_vars_v3"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_missing_egg_id_v3)", 
        span(textOutput("error_missing_egg_id_v3"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_na_in_dates_v3)", 
        span(textOutput("error_na_in_dates_v3"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.error_file_type_v3)", 
        span(textOutput("error_file_type_v3"), style = "color:#e44c3d")
      ),
      conditionalPanel(
        condition = "!is.na(output.warning_missing_vals_v3)", 
        span(textOutput("warning_missing_vals_v3"), style = "color:#f39c13")
      ),
      hr(),
      h4("Overview"),
      span(
        includeMarkdown("text/04-downloads-header.Rmd"),
        style = "font-size:14px;"
      ),
      hr(),
      fluidRow(
        column(
          h4("Download Preview Table"),
          column(
            conditionalPanel(
              condition = "!is.na(output.message_downoad_table)",
              span(textOutput("message_download_table"), style = "color:grey")
            ),
            width = 10
          ),
          div(DT::dataTableOutput("download_table"), style = "font-size: 100%; width: 100%"),
          width = 12
          )
        )
      ) 
  ),
  
  # HELP PAGE
  tabPanel(
    title = div("Help", style = "font-size:14px;"),
    value = "help",
    column(width = 1),
    column(
      width = 9,
      h3(strong("Help Page")),
      span(
        includeMarkdown("text/05-help-header.Rmd"),
        style = "font-size:14px;"
      ),
      tabsetPanel(
        type = "tabs",
        # Tab for variable definitions
        tabPanel(
          "Environmental Variables",
          br(),
          includeMarkdown("text/05-help-vars-env.Rmd"),
          width = 12
        ),
        # Tab for input table specifications
        tabPanel(
          "Morphological Variables",
          br(),
          span(includeMarkdown("text/05-help-vars-morph.Rmd"), style = "font-size:14px;"),
          width = 12
        ),
        # Tab details on the random forests
        tabPanel(
          "Random Forest Details",
          br(),
          span(includeMarkdown("text/05-help-random-forest.Rmd"), style = "font-size:14px;"),
          width = 12
        ),
        tabPanel(
          "FAQ",
          br(),
          span(includeMarkdown("text/05-help-faq.md"), style = "font-size:14px;"),
          width = 12
        )
      ) 
    )
  ),
  
  # REFERENCES
  tabPanel(
    title = div("References", style = "font-size:14px;"),
    column(width = 1),
    column(
      width = 9,
      img(src = "larval-ac.jpeg", width = "900px"),
      br(),
      br(),
      span(
        includeMarkdown("text/06-references.Rmd"),
        style = "font-size:14px;"
      )
    )
  )
  
)

##### APP SERVER #####

server <- function(input, output, session) {
  
  ## LINKS BETWEEN TABS ------------------------------------------------------
  
  # Jump to help page from overview
  observeEvent(input$overview2help, {
    updateTabsetPanel(session, "inTabset", "help")
  })
  
  # Another button to jump to help page from overview
  observeEvent(input$overview2helpagain, {
    updateTabsetPanel(session, "inTabset", "help")
  })
  
  # Another button to jump to help page from overview
  observeEvent(input$overview2helpagainx2, {
    updateTabsetPanel(session, "inTabset", "help")
  })
  
  # Jump to predictions from inputs
  observeEvent(
    input$jump2pred, {
      updateTabsetPanel(session, "inTabset", selected = "predictions")
    }
  )
  
  # Jump to downloads from predictions
  observeEvent(
    input$jump2download, {
      updateTabsetPanel(session, "inTabset", selected = "downloads")
    }
  )
  
  # Jump to help page
  observeEvent(input$input2help, {
    updateTabsetPanel(session, "inTabset", "help")
  })
  
  ## OVERVIEW ------------------------------------------------------------------
  
  # Create a table with the input values
  output$species_table <- function() {
    eggdata %>% 
      count(Family_ACGC, Genus_ACGC, Common_Name_ACGC) %>% 
      rename(
        "Family" = "Family_ACGC",
        "Genus" = "Genus_ACGC",
        "Common Name" = "Common_Name_ACGC",
        "Number of Eggs in Training Data" = "n"
      ) %>%
      knitr::kable("html", align = "lllc") %>%
      kableExtra::column_spec(
        column = 1:4,
        width = "3cm"
      ) %>%
      kableExtra::collapse_rows(
        columns = 1:3,
        valign = "top"
      )
  }
  
  ## INPUTS ------------------------------------------------------------------
  
  # Template download
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("WhoseEggtemplate", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("data/template.xlsx", file)
    }
  )
  
  # Put the input variables in a data frame
  input_data <- reactive({
    file <- input$spreadsheet
    ext <- tools::file_ext(file$name)
    req(file)
    if (ext == "csv") {
      read.csv(file$datapath)
    } else {
      file.rename(file$datapath, paste(file$datapath, ext, sep = "."))
      readxl::read_excel(paste(file$datapath, ext, sep = "."), 1)
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
      processed <- 
        input_data() %>%
        compute_variables() %>%
        adjust_variable_types() %>%
        adjust_factor_levels() %>%
        sort_vars()
      processed
    }
  })
  
  # Create a table with the input values
  output$input_table <- DT::renderDataTable({
    if (!is.null(input_data())){
      input_data() %>%
        mutate_all(.funs = as.character) %>%
        datatable(
          options = list(
            scrollY = "400px",
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
            scrollY = "400px",
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
          rename(
            "Egg ID" = "Egg_ID",
            "Family Pred" = "Family_Pred",
            "Family Prob" = "Family_Prob",
            "Genus Pred" = "Genus_Pred",
            "Genus Prob" = "Genus_Prob",
            "Species Pred" = "Species_Pred",
            "Species Prob" = "Species_Prob"
          ) %>%
          datatable(
            options = list(
              scrollY = "400px",
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
      pred_plot_obj = function() {
        rf_pred_plot(na.omit(data_and_preds()))
      }
      output$pred_plot <- renderPlot({
        pred_plot_obj()
      }, height = 500, width = 850)
      
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
        # Create the plots
        rf_prob_plot(na.omit(data_and_preds()), input$pred_table_rows_selected)
      }, height = 500, width = 850)
    }
    
  })
  
  ## DOWNLOADS ---------------------------------------------------------------
  
  # Download data and predictions
  observeEvent(input$preview, { 
    if (!is.null(input_data())) {
      # Create a table with random forest prediction results
      output$download_table <- DT::renderDataTable({
        # Put the random forest results in a table
        data_and_preds() %>%
          datatable(
            options = list(
              scrollY = "400px",
              pageLength = 5,
              scrollX = TRUE,
              scrollCollapse = TRUE,
              autoWidth = FALSE,
              columnDefs = list(list(width = '1px', targets = "_all"))
            ),
            selection = 'single'
          )
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
  message_pred_plots <- reactive({
    if (input$getpreds == 0 | is.null(input$spreadsheet)) {
      "Visualizations of predictions will appear here after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else if (is.null(input_data()) |
               !check_for_vars(input_data()) | 
               !check_for_egg_ids(input_data()) |
               !check_fct_levels(input_data())) {
      "Visualizations of predictions will appear here after inputs are provided via
      the 'Data Input' page and the 'Get Predictions' button is clicked."
    } else {
      NA
    }
  })
  output$message_pred_plots_v1 <- message_pred_plots
  output$message_pred_plots_v2 <- message_pred_plots
  output$message_pred_plots_v3 <- message_pred_plots
  output$message_pred_plots_v4 <- message_pred_plots
  outputOptions(output, "message_pred_plots_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "message_pred_plots_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "message_pred_plots_v3", suspendWhenHidden = FALSE)
  outputOptions(output, "message_pred_plots_v4", suspendWhenHidden = FALSE)

  # Provide a message where download table will be
  output$message_download_table <- reactive({
    if (input$preview == 0 | is.null(input$spreadsheet)) {
      "A table with the data for downloading will appear here after inputs are 
      provided via the 'Data Input' page and the 'Preview Data' button is clicked."
    } else if (is.null(input_data()) |
               !check_for_vars(input_data()) |
               !check_fct_levels(input_data()) | 
               !check_for_egg_ids(input_data())) {
      "A table with the data for downloading will appear here after inputs are 
      provided via the 'Data Input' page and the 'Preview Data' button is clicked."
    } else {
      NA
    }
  })
  outputOptions(output, "message_download_table", suspendWhenHidden = FALSE)
  
  # Data frame with prediction download
  output$downloadPreds <- downloadHandler(
    filename = function() {
      paste0("WhoseEggPredictions.", input$download_file_type)
    },
    content = function(file) {
      if (input$download_file_type == "xlsx") {
        writexl::write_xlsx(data_and_preds(), file)
      } else if (input$download_file_type == "xls") {
        WriteXLS::WriteXLS(data_and_preds(), file)
      } else if (input$download_file_type == "csv") {
        write.csv(data_and_preds(), file, row.names = FALSE)
      }
    }
  )
  
  ## ERRORS ------------------------------------------------------------------
  
  # Check that an appropriate file type was provided
  error_file_type <- reactive({
    if (is.null(input_data())) {
      "Error: Please provide a .csv, .xlsx, or .xls file."
    } else { NA }
  })
  output$error_file_type_v1 <- error_file_type
  output$error_file_type_v2 <- error_file_type
  output$error_file_type_v3 <- error_file_type
  outputOptions(output, "error_file_type_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_file_type_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "error_file_type_v3", suspendWhenHidden = FALSE)
  
  # Check all egg ids provided
  error_missing_egg_id <- reactive({
    if (!is.null(input_data())) {
      if (!check_for_egg_ids(input_data())) {
        "Error: Must provide all Egg IDs"
      } else { NA }
    }
  })
  output$error_missing_egg_id_v1 <- error_missing_egg_id
  output$error_missing_egg_id_v2 <- error_missing_egg_id
  output$error_missing_egg_id_v3 <- error_missing_egg_id
  outputOptions(output, "error_missing_egg_id_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_egg_id_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_egg_id_v3", suspendWhenHidden = FALSE)
  
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
  output$error_missing_vars_v3 <- error_missing_vars
  outputOptions(output, "error_missing_vars_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_vars_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "error_missing_vars_v3", suspendWhenHidden = FALSE)
  
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
  output$error_wrong_fct_levels_v3 <- error_wrong_fct_levels
  outputOptions(output, "error_wrong_fct_levels_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_wrong_fct_levels_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "error_wrong_fct_levels_v3", suspendWhenHidden = FALSE)
  
  # Check that it is possible to compute dates
  error_na_in_dates <- reactive({
    if (!is.null(input_data()) & "Day" %in% names(input_data())) {
      if (!check_dates(input_data())) {
        paste(
          "Error: Not possible to compute Julian day for the following egg IDs: \n", 
          paste(get_na_dates(input_data()), collapse = ", ")
        )
      } else { NA }
    }
  })
  output$error_na_in_dates_v1 <- error_na_in_dates
  output$error_na_in_dates_v2 <- error_na_in_dates
  output$error_na_in_dates_v3 <- error_na_in_dates
  outputOptions(output, "error_na_in_dates_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "error_na_in_dates_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "error_na_in_dates_v3", suspendWhenHidden = FALSE)
  
  ## WARNINGS ----------------------------------------------------------------
  
  # Check for missing values
  warning_missing_vals <- reactive({
    if (!is.null(input_data())) {
      if (sum(is.na(processed_inputs() %>% select(all_of(rf_pred_vars)))) > 0) {
        "Warning: Missing values detected in the processed data. 
        Random forests cannot return predictions for observations with missing values.
        These observations will be excluded on the 'Predictions' page."
      } else NA
    }
  })
  output$warning_missing_vals_v1 <- warning_missing_vals
  output$warning_missing_vals_v2 <- warning_missing_vals
  output$warning_missing_vals_v3 <- warning_missing_vals
  outputOptions(output, "warning_missing_vals_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_missing_vals_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_missing_vals_v3", suspendWhenHidden = FALSE)
  
  # Check for variables that will lead to extrapolation
  warning_vars_outside_ranges <- reactive({
    if (!is.null(input_data())) {
      if (!check_var_ranges(processed_inputs())) {
        paste(
          "Warning: Some variables outside of ranges in training data.
          This will lead to model extrapolation and possibly poor predictions.
          Egg IDs with values outside of ranges: ",
          paste(get_outside_var_ranges(processed_inputs()), collapse = ", ")
        )
      } else {
        NA
      }
    }
  })
  output$warning_vars_outside_ranges_v1 <- warning_vars_outside_ranges
  output$warning_vars_outside_ranges_v2 <- warning_vars_outside_ranges
  output$warning_vars_outside_ranges_v3 <- warning_vars_outside_ranges
  outputOptions(output, "warning_vars_outside_ranges_v1", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_vars_outside_ranges_v2", suspendWhenHidden = FALSE)
  outputOptions(output, "warning_vars_outside_ranges_v3", suspendWhenHidden = FALSE)
  
}

##### RUN APP #####

shinyApp(ui, server)
