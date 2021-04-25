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
            # Create a Statistical Menu
            menuItem(
                "Statistical Analysis",
                icon = icon("chart-line"),
                quickPop(
                    menuSubItem("Usage Guide",
                                tabName = "statisticalUsage",
                                icon = icon("info")),
                    "Click here for a guide for statistical analysis"
                ),
                quickPop(
                    menuSubItem(
                        "Regression",
                        tabName = "statisticalRegression",
                        icon = icon("chart-line")
                    ),
                    "Visualise relationship between two variables using regression plot and visualise marginal distributions of variables using histogram"
                ),
                quickPop(
                    menuSubItem(
                        "Scatter Plot",
                        tabName = "statisticalScatter",
                        icon = icon("braille")
                    ),
                    "Visualise values for two variables using interactive scatter plot coloured by year"
                ),
                quickPop(
                    menuSubItem(
                        "Correlation Matrix",
                        tabName = "statisticalCorrelation",
                        icon = icon("border-all")
                    ),
                    "Visualise strength of relationship between pairs of variables using correlation matrix"
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
        # Change the theme
        shinyDashboardThemes(theme = "grey_light"),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "css/vistas.css")
        ),
        useShinyjs(),
        tags$script(
            HTML(
                "
        var openTab = function(parentTab, tabName) {
            $('ul[data-expanded=\"' + parentTab + '\"][style*=\"display: none\"]').prev().click();
            setTimeout(function() {
                $('a[data-value=\"' + tabName + '\"]').click();
            }, 1000);
          }
      "
            )
        ),
        tabItems(
            # Create an Intro Tab
            tabItem(tabName = "intro",
                    introUI()),
            
            # Create tabs for Statistical Analysis
            tabItem(tabName = "statisticalUsage",
                    statisticalUsageUI()),
            tabItem(tabName = "statisticalRegression",
                    statisticalRegressionUI()),
            tabItem(tabName = "statisticalScatter",
                    statisticalScatterUI()),
            tabItem(tabName = "statisticalCorrelation",
                    statisticalCorrelationUI()),
            
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
    # Load the Statistical Server function
    statisticalServer()
    
    # Load the Migration Server function
    migrationServer()
}

# Run the Shiny App
shinyApp(ui, server)