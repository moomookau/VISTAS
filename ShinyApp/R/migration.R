###########################################################
# This file contains the code for the migration module
###########################################################

# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R
migrationPreLoad <- function() {
  
}

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
migrationUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(
    navbarPage(
      "Migration Analysis",
      tabPanel("Usage Guide"),
      tabPanel("Flow Map",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("flowMapType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
                   selectInput(
                     inputId = ns("flowMapCountry"),
                     label = "Country:",
                     choices = c("Singapore", "India"),
                     selected = "Singapore"
                   ),
                   selectInput(
                     inputId = ns("flowMapDate"),
                     label = "Select year",
                     choices = 2015:2019
                   )
                 ),
                 mainPanel("Flow Map")
               )),
      tabPanel("Choropleth",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("choroplethType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
                   selectInput(
                     inputId = ns("choroplethCountry"),
                     label = "Country:",
                     choices = c("Singapore", "India"),
                     selected = "Singapore"
                   ),
                   selectInput(
                     inputId = ns("choroplethDate"),
                     label = "Select year",
                     choices = 2015:2019
                   )
                 ),
                 mainPanel("Choropleth",
                           verbatimTextOutput(outputId = ns("test"))),
               )),
      tabPanel("Sankey Diagram",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("sankeyType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
                   selectInput(
                     inputId = ns("sankeyCountry"),
                     label = "Country:",
                     choices = c("Singapore", "India"),
                     selected = "Singapore"
                   ),
                   selectInput(
                     inputId = ns("sankeyDate"),
                     label = "Select year",
                     choices = 2015:2019
                   ),
                   selectInput(
                     inputId = ns("sankeyColour"),
                     label = "Colour by:",
                     choices = c("Region", "Income"),
                     selected = "Region"
                   )
                 ),
                 mainPanel("Sankey Diagram")
               )),
      tabPanel("Chord Diagram",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("chordType"),
                     label = "Type of Chord Diagram",
                     choices = c("Single Region", "Aggregate"),
                     selected = "Single Region"
                   ),
                   selectInput(
                     inputId = ns("chordRegion"),
                     label = "Region:",
                     choices = c("East Asia & Pacific"),
                     selected = "East Asia & Pacific"
                   ),
                   selectInput(
                     inputId = ns("chordDate"),
                     label = "Select year",
                     choices = 2015:2019
                   )
                 ),
                 mainPanel("Chord Diagram")
               ))
    )
  )
}

migrationServer <- function(id = "migration") {
  moduleServer(id,
               function(input, output, session) {
                 output$test <- renderText({
                   input$choroplethDate
                 })
               })
}