library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(dashboardthemes)
library(tidyverse)
library(readxl)

loadData()
regressionPreLoad()
migrationPreLoad()
industryskillPreLoad()

ui <- dashboardPage(
    dashboardHeader(title="VISTAS"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon=icon("home")),
            menuItem("Regression Analysis", tabName = "regression", icon=icon("chart-line")),
            menuItem("Migration Analysis", tabName = "migration", icon=icon("exchange")),
            menuItem("Industry/Skills Needs Analysis", tabName = "industryskill", icon=icon("briefcase")),
            menuItem("Options", tabName = "options", icon=icon("tools"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "intro"),
            tabItem(tabName = "regression",
                    regressionUI()),
            tabItem(tabName = "migration",
                    migrationUI()
                    ),
            tabItem(tabName = "industryskill",
                    industryskillUI()),
            tabItem(tabName = "options",
                    themeSelector(),
                    selectInput(
                        inputId = "theme",
                        label = 'Dashboard Theme',
                        choices =  c('blue_gradient', 'flat_red', 'grey_light','grey_dark',
                                     'onenote', 'poor_mans_flatly', 'purple_gradient'),
                        selected = "grey_light")
                    )
        ),
        uiOutput("myTheme")
    )
)

server <- function(input, output) {
    output$myTheme <- renderUI( shinyDashboardThemes(theme = input$theme))
    regressionServer()
    migrationServer()
    industryskillServer()
}

shinyApp(ui, server)