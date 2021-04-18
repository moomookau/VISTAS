# We load the libraries
library(shiny) # Base Shiny library
library(shinydashboard) # Used for Shiny Dashboard
library(shinyWidgets) # Used for Shiny Widgets
library(shinythemes) # Used for Shiny Themes
library(dashboardthemes) # Used for Shiny Dashboard Themes
library(tidyverse) # Tidy universe of libraries
library(readxl) # Used to read excel files
library(leaflet) # Used to draw interactive maps

# Define the UI
ui <- dashboardPage(
    # Set the Dashboard Header
    dashboardHeader(title = "VISTAS"),
    # Create a Dashboard Sidebar
    dashboardSidebar(
        sidebarMenu(
            # Create an Intro Menu
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            # Create a Regression Menu
            menuItem(
                "Regression Analysis",
                tabName = "regression",
                icon = icon("chart-line")
            ),
            # Create a Migration Menu
            menuItem(
                "Migration Analysis",
                tabName = "migration",
                icon = icon("exchange")
            ),
            # Create an Industry Skills Needs Menu
            menuItem(
                "Industry/Skills Needs Analysis",
                tabName = "industryskill",
                icon = icon("briefcase")
            ),
            # Create an Options Menu
            menuItem("Options", tabName = "options", icon = icon("tools"))
        )
    ),
    # Create a Dashboard Body
    dashboardBody(
        tabItems(
            # Create an Intro Tab
            tabItem(tabName = "intro"),
            # Create a Regression Tab
            tabItem(tabName = "regression",
                    # Load the Regression UI
                    regressionUI()),
            # Create a Migration Tab
            tabItem(tabName = "migration",
                    # Load the Migration UI
                    migrationUI()),
            # Create an Industry Skill Tab
            tabItem(tabName = "industryskill",
                    # Load the Industry Skill UI
                    industryskillUI()),
            # Create an Options Tab
            tabItem(
                tabName = "options",
                # Create a shiny theme selector
                themeSelector(),
                # Create a shiny dashboard theme selector
                selectInput(
                    inputId = "theme",
                    label = 'Dashboard Theme',
                    choices =  c(
                        'blue_gradient',
                        'flat_red',
                        'grey_light',
                        'grey_dark',
                        'onenote',
                        'poor_mans_flatly',
                        'purple_gradient'
                    ),
                    selected = "grey_light"
                )
            )
        ),
        # Create an output for the dashboard theme
        uiOutput("dashboardTheme")
    )
)

# Define the Server
server <- function(input, output) {
    # Set the theme
    output$dashboardTheme <-
        renderUI(shinyDashboardThemes(theme = input$theme))
    
    # Load the Regression Server function
    regressionServer()
    
    # Load the Migration Server function
    migrationServer()
    
    # Load the Industry Skill Server function
    industryskillServer()
}

# Run the Shiny App
shinyApp(ui, server)