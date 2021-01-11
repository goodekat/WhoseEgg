
library(dplyr)
library(DT)
library(forcats)
library(randomForest)
library(shiny)
library(shinydashboard)
library(tidyr)

source("../code/helper-functions.R")

rfs <- readRDS("../data/rfs141516.rds")

# Define UI for miles per gallon app ----
ui <- dashboardPage(
    
    # App title ----
    dashboardHeader(title = "Name of App", titleWidth = 375),
    
    # Sidebar panel for inputs ----
    dashboardSidebar(
        width = 375,
        sidebarMenu(
            h2("Questions:"),
            p("How to order the variables?"),
            p("Random forests with reduced variables? (currently full)"),
            p("What does the egg stage of 'D' stand for?"),
            h2("Input predictor variables"),
            selectInput(
                inputId = "input_opt", 
                label = "Select a manner in which to provide input values for the random forest", 
                choices = c("Upload csv file", "Manually input values")
                ),
            conditionalPanel(
                condition = "input.input_opt == 'Upload csv file'",
                fileInput("csv", "Select a csv file to upload")
                ),
            conditionalPanel(
                condition = "input.input_opt == 'Manually input values'", 
                textInput("Month", "Month of Collection:"),
                textInput("Julian_Day", "Julian Day of Collection:"),
                textInput("Temperature", "Water Temperature (C):"),
                textInput("Conductivity", "Conductivity (muS/cm):"),
                textInput("Membrane_Ave", "Membrane Average (mm):"),
                textInput("Membrane_SD", "Membrane Standard Deviation (mm):"),
                textInput("Membrane_CV", "Membrance Coefficient of Variation:"),
                textInput("Yolk_Ave", "Embryo Average (mm):"),
                textInput("Yolk_SD", "Embryo Standard Deviation (mm):"),
                textInput("Yolk_CV", "Embryo Coefficient of Variation:"),
                textInput("Yolk_to_Membrane_Ratio", "Perivitelline Space Index:"),
                textInput("Larval_Length", "Late Stage Embryo Midline Length (mm):"),
                selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
                selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
                selectInput("Egg_Stage", "Egg Development Stage:", c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "D")),
                selectInput("Compact_Diffuse", "Compact or Diffuse Embryo:", c("", "Compact", "Diffuse")),
                selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No"))
                )
            )
        ),
    
    # Main panel for displaying outputs ----
    dashboardBody(
        fluidRow(
            box(
                h2("Random forest prediction"),
                tableOutput("pred_table"),
                h2("Predictor variables input to random forest"),
                tableOutput("input_table")
                ),
            box(
                h2("Random forest prediction probabilities by taxonomic level"),
                h4("Family"),
                DTOutput("prob_table_family"),
                h4("Genus"),
                DTOutput("prob_table_genus"),
                h4("Species"),
                DTOutput("prob_table_species")
                )
            )
        )
    )

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
    output$input_table <- renderTable({
        
        # Put the specified input variables in a data frame
        if (input$input_opt == "Upload csv file") {
            file <- input$csv
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            inputs_temp <- read.csv(file$datapath)
        } else if (input$input_opt == "Manually input values") {
            inputs_temp <- inputs_to_df(input)
        }
        
        # Adjust format of df for viewing
        inputs_temp %>%
            mutate_all(.funs = as.character) %>%
            pivot_longer(names_to = "Variable",
                         values_to = "Input Value",
                         cols = everything())
    })
        
    output$pred_table <- renderTable({
        
        # Put the specified input variables in a data frame
        if (input$input_opt == "Upload csv file") {
            file <- input$csv
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            inputs_temp <- read.csv(file$datapath)
        } else if (input$input_opt == "Manually input values") {
            inputs_temp <- inputs_to_df(input)
        }
        
        # Prepare the inputs for the random forest
        inputs_clean <-
            inputs_temp %>%
            adjust_variable_types() %>%
            adjust_factor_levels()

        # Obtain the random forest predictions and put them in a table
        data.frame(
            'Taxonomic Level' = c("Family", "Genus", "Species"),
            'Prediction' = c(
                as.character(predict(rfs$Family_ACGC, inputs_clean)),
                as.character(predict(rfs$Genus_ACGC, inputs_clean)),
                as.character(predict(rfs$Common_Name_ACGC, inputs_clean))
            ),
            check.names = FALSE
        )

    })
    
    output$prob_table_family <- renderDT({
        
        # Put the specified input variables in a data frame
        if (input$input_opt == "Upload csv file") {
            file <- input$csv
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            inputs_temp <- read.csv(file$datapath)
        } else if (input$input_opt == "Manually input values") {
            inputs_temp <- inputs_to_df(input)
        }
        
        # Prepare the inputs for the random forest
        inputs_clean <-
            inputs_temp %>%
            adjust_variable_types() %>%
            adjust_factor_levels()
        
        predict(rfs$Family_ACGC, inputs_clean, type = "prob") %>%
            data.frame() %>%
            pivot_longer(names_to = "Level", values_to = "Prediction probability", cols = everything())
        
    })
    
    output$prob_table_genus <- renderDT({
        
        # Put the specified input variables in a data frame
        if (input$input_opt == "Upload csv file") {
            file <- input$csv
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            inputs_temp <- read.csv(file$datapath)
        } else if (input$input_opt == "Manually input values") {
            inputs_temp <- inputs_to_df(input)
        }
        
        # Prepare the inputs for the random forest
        inputs_clean <-
            inputs_temp %>%
            adjust_variable_types() %>%
            adjust_factor_levels()
        
        predict(rfs$Genus_ACGC, inputs_clean, type = "prob") %>%
            data.frame() %>%
            pivot_longer(names_to = "Level", values_to = "Prediction probability", cols = everything())
        
    })
    
    output$prob_table_species <- renderDT({
        
        # Put the specified input variables in a data frame
        if (input$input_opt == "Upload csv file") {
            file <- input$csv
            ext <- tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file"))
            inputs_temp <- read.csv(file$datapath)
        } else if (input$input_opt == "Manually input values") {
            inputs_temp <- inputs_to_df(input)
        }
        
        # Prepare the inputs for the random forest
        inputs_clean <-
            inputs_temp %>%
            adjust_variable_types() %>%
            adjust_factor_levels()
        
        predict(rfs$Common_Name_ACGC, inputs_clean, type = "prob") %>%
            data.frame() %>%
            pivot_longer(names_to = "Level", values_to = "Prediction probability", cols = everything())
        
    })
    
}

shinyApp(ui, server)