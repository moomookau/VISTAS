# Libraries and loading of data is defined in init.R
# Other code to run before the Shiny App is started should be done in the individual module's R file

# Define the UI
ui <- dashboardPage(
    # Set the Dashboard Header
    dashboardHeader(title = "VISTAS"),
    # Create a Dashboard Sidebar
    dashboardSidebar(
        sidebarMenu(
            id = "migration-sidebar",
            # Create an Intro Menu
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            # Create a Regression Menu
            menuItem(
                "Statistical Analysis",
                icon = icon("chart-line"),
                quickPop(
                    menuSubItem("Usage Guide",
                                tabName = "regressionUsage",
                                icon = icon("info")),
                    "Placeholder for tooltip"
                ),
                quickPop(
                    menuSubItem(
                        "Regression",
                        tabName = "regressionRegression",
                        icon = icon("chart-line")
                    ),
                    "Placeholder for tooltip"
                ),
                quickPop(
                    menuSubItem(
                        "Scatter Plot",
                        tabName = "regressionScatter",
                        icon = icon("braille")
                    ),
                    "Placeholder for tooltip"
                ),
                quickPop(
                    menuSubItem(
                        "Correlation Matrix",
                        tabName = "regressionCorrelation",
                        icon = icon("border-all")
                    ),
                    "Placeholder for tooltip"
                )
            ),
            # Create a Migration Menu
            menuItem(
                "Migration Analysis",
                icon = icon("exchange"),
                quickPop(
                    menuSubItem("Usage Guide",
                                tabName = "migrationUsage",
                                icon = icon("info")),
                    "Click here for a guide for migration analysis"
                ),
                quickPop(
                    menuSubItem(
                        "Choropleth Map",
                        tabName = "migrationChoropleth",
                        icon = icon("globe-asia")
                    ),
                    "Visualise migration using a thematic map where countries are coloured by migration values"
                ),
                quickPop(
                    menuSubItem(
                        "Chord Diagram",
                        tabName = "migrationChord",
                        icon = icon("exchange-alt")
                    ),
                    "Visualise country to country migration through a chord diagram"
                ),
                quickPop(
                    menuSubItem(
                        "Slope Graph",
                        tabName = "migrationSlope",
                        icon = icon("chart-line")
                    ),
                    "Visualise migration using a slope graph where lines representing each country are plotted to easily compare ranks at different points of time"
                ),
                quickPop(
                    menuSubItem("Treemap",
                                tabName = "migrationTree",
                                icon = icon("th")),
                    "Visualise migration using a treemap where hierarchical data is represented by nested rectangles"
                ),
                quickPop(
                    menuSubItem("Geofacet",
                                tabName = "migrationGeofacet",
                                icon = icon("map")),
                    "Visualise migration using sub-plots that are laid out in geographic topology"
                )
            )
        )
    ),
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