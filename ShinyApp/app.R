# Libraries and loading of data is defined in init.R
# Other code to run before the Shiny App is started should be done in the individual module's R file

# Define the UI
ui <- dashboardPage(
    # Set the Dashboard Header
    dashboardHeader(title = "VISTAS"),
    # Create a Dashboard Sidebar
    dashboardSidebar(sidebarMenu(
        id = "migration-sidebar",
        # Create an Intro Menu
        menuItem("Introduction", tabName = "intro", icon = icon("home")),
        # Create a Regression Menu
        menuItem(
            "Regression Analysis",
            tabName = "regression",
            icon = icon("chart-line"),
            menuSubItem("Usage Guide",
                        tabName = "regressionUsage",
                        icon = icon("info")),
            menuSubItem(
                "Regression",
                tabName = "regressionRegression",
                icon = icon("chart-line")
            ),
            menuSubItem(
                "Scatter Plot",
                tabName = "regressionScatter",
                icon = icon("braille")
            ),
            menuSubItem(
                "Correlation Matrix",
                tabName = "regressionCorrelation",
                icon = icon("border-all")
            )
        ),
        # Create a Migration Menu
        menuItem(
            "Migration Analysis",
            tabName = "migration",
            icon = icon("exchange"),
            menuSubItem("Usage Guide",
                        tabName = "migrationUsage",
                        icon = icon("info")),
            menuSubItem(
                "Choropleth",
                tabName = "migrationChoropleth",
                icon = icon("globe-asia")
            ),
            menuSubItem(
                "Chord Diagram",
                tabName = "migrationChord",
                icon = icon("exchange-alt")
            ),
            menuSubItem(
                "Slope Graph",
                tabName = "migrationSlope",
                icon = icon("chart-line")
            ),
            menuSubItem("Treemap",
                        tabName = "migrationTree",
                        icon = icon("th")),
            menuSubItem("Geofacet",
                        tabName = "migrationGeofacet",
                        icon = icon("map"))
        )
    )),
    # Create a Dashboard Body
    dashboardBody(
        useShinyjs(),
        # Change the theme
        shinyDashboardThemes(theme = "grey_light"),
        tabItems(
            # Create an Intro Tab
            tabItem(tabName = "intro"),
            
            # Create tabs for Regression Analysis
            tabItem(tabName = "regressionUsage",
                    regressionUsageUI()),
            tabItem(tabName = "regressionRegression",
                    regressionRegressionUI()),
            tabItem(tabName = "regressionScatter",
                    regressionScatterUI()),
            tabItem(tabName = "regressionCorrelation",
                    regressionCorrelationUI()),
            
            # Create tabs for Migration Analysis
            tabItem(tabName = "migrationUsage",
                    migrationUsageUI()),
            tabItem(tabName = "migrationChoropleth",
                    migrationChoroplethUI()),
            tabItem(tabName = "migrationChord",
                    migrationChordUI()),
            tabItem(tabName = "migrationSlope",
                    migrationSlopeUI()),
            tabItem(tabName = "migrationTree",
                    migrationTreeUI()),
            tabItem(tabName = "migrationGeofacet",
                    migrationGeofacetUI())
        )
    )
)

# Define the Server
server <- function(input, output) {
    # Load the Regression Server function
    regressionServer()
    
    # Load the Migration Server function
    migrationServer()
}

# Run the Shiny App
shinyApp(ui, server)