#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Next step: Change input factors to match random forest

library(shiny)

rfs <- readRDS("../data/rfs141516.rds")
source("../code/helper-functions.R")

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
    
    # App title ----
    headerPanel("Predicting Invasive Carp with a Random Forest Model"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        h2("Random forest input variables"),
        p("Q: How to order the variables?"),
        p("Q: Random forests with reduced variables? (currently full)"),
        p("Q: What does the egg stage of 'D' stand for?"),
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
        textInput("Larval_Length", "Late Stage Embryo Midline Length (mm)"),
        selectInput("Deflated", "Deflated Membrane:", c("", "Yes", "No")),
        selectInput("Pigment", "Pigment Presence:", c("", "Yes", "No")),
        selectInput("Egg_Stage", "Egg Development Stage", c("", "1", "2", "3", "4", "5", "6", "7", "8", "Broken", "D")),
        selectInput("Compact_Diffuse", "Compact or Diffuse Embryo", c("", "Compact", "Diffuse")),
        selectInput("Sticky_Debris", "Debris on Egg:", c("", "Yes", "No"))
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        mainPanel(
            tableOutput("selected_var")
        )
    )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
    
    output$selected_var <- renderTable({ 
        
        input_vars = data.frame(
            "Month" = input$Month,
            "Julian_Day" = input$Julian_Day,
            "Temperature" = input$Temperature,
            "Conductivity" = input$Conductivity,
            "Membrane_Ave" = input$Membrane_Ave,
            "Membrane_SD" = input$Membrane_SD,
            "Membrane_CV" = input$Membrane_CV,
            "Yolk_Ave" = input$Yolk_Ave,
            "Yolk_SD" = input$Yolk_SD,
            "Yolk_CV" = input$Yolk_CV,
            "Yolk_to_Membrane_Ratio" = input$Yolk_to_Membrane_Ratio,
            "Larval_Length" = input$Larval_Length,
            "Deflated" = input$Deflated,
            "Pigment" = input$Pigment,
            "Egg_Stage" = input$Egg_Stage,
            "Compact_Diffuse" = input$Compact_Diffuse,
            "Sticky_Debris" = input$Sticky_Debris
        )
        
        input_vars

    })
    
}

shinyApp(ui, server)