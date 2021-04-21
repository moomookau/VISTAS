# Libraries and loading of data is defined in init.R
# Other code to run before the Shiny App is started should be done in the individual module's R file

# Define the UI
ui <- dashboardPage(
    # Set the Dashboard Header
    dashboardHeader(title = "VISTAS"),
    # Create a Dashboard Sidebar
    dashboardSidebar(
        # Create an output for the dashboard theme
        uiOutput("dashboardTheme"),
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
}

# Run the Shiny App
shinyApp(ui, server)