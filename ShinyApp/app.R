library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(dashboardthemes)

themeSelector <- function() {
    div(
        div(
            selectInput(
                "shinytheme-selector",
                "Choose a theme",
                c("default", shinythemes:::allThemes()),
                selectize = FALSE
            )
        ),
        tags$script(
            "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
        )
    )
}

ui <- dashboardPage(
    dashboardHeader(title="VISTAS"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon=icon("home")),
            menuItem("Regression and Correlation", tabName = "regcorr", icon=icon("chart-line")),
            menuItem("Migration Analysis", tabName = "migration", icon=icon("exchange")),
            menuItem("Industry/Skills Needs", tabName = "indskills", icon=icon("briefcase")),
            menuItem("Options", tabName = "options", icon=icon("tools"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "intro"),
            tabItem(tabName = "regcorr"),
            tabItem(tabName = "migration",
                    navbarPage("Migration Analysis",
                               tabPanel("Usage Guide"),
                               tabPanel("Flow Map",
                                        sidebarLayout(
                                            sidebarPanel(h3("Options"),
                                                         selectInput(
                                                             inputId = "flowMapType",
                                                             label = "Type Of Migration:",
                                                             choices = c("Country", "Industry", "Skills"),
                                                             selected = "Country"
                                                         ),
                                                         selectInput(
                                                             inputId = "flowMapCountry",
                                                             label = "Country:",
                                                             choices = c("Singapore", "India"),
                                                             selected = "Singapore"
                                                         ),
                                                         selectInput(
                                                             inputId =  "flowMapDate", 
                                                             label = "Select year", 
                                                             choices = 2015:2019
                                                         )),
                                            mainPanel("Flow Map")
                                        )),
                               tabPanel("Choropleth",
                                        sidebarLayout(
                                            sidebarPanel(h3("Options"),
                                                         selectInput(
                                                             inputId = "choroplethType",
                                                             label = "Type Of Migration:",
                                                             choices = c("Country", "Industry", "Skills"),
                                                             selected = "Country"
                                                         ),
                                                         selectInput(
                                                             inputId = "choroplethCountry",
                                                             label = "Country:",
                                                             choices = c("Singapore", "India"),
                                                             selected = "Singapore"
                                                         ),
                                                         selectInput(
                                                             inputId =  "choroplethDate", 
                                                             label = "Select year", 
                                                             choices = 2015:2019
                                                         )),
                                            mainPanel("Choropleth Map")
                                        )),
                               tabPanel("Sankey Diagram",
                                        sidebarLayout(
                                            sidebarPanel(h3("Options"),
                                                         selectInput(
                                                             inputId = "sankeyType",
                                                             label = "Type Of Migration:",
                                                             choices = c("Country", "Industry", "Skills"),
                                                             selected = "Country"
                                                         ),
                                                         selectInput(
                                                             inputId = "sankeyCountry",
                                                             label = "Country:",
                                                             choices = c("Singapore", "India"),
                                                             selected = "Singapore"
                                                         ),
                                                         selectInput(
                                                             inputId =  "sankeyDate", 
                                                             label = "Select year", 
                                                             choices = 2015:2019
                                                         ),
                                                         selectInput(
                                                             inputId = "sankeyColour",
                                                             label = "Colour by:",
                                                             choices = c("Region", "Income"),
                                                             selected = "Region"
                                                         )),
                                            mainPanel("Sankey Diagram")
                                        )),
                               tabPanel("Chord Diagram",
                                        sidebarLayout(
                                            sidebarPanel(h3("Options"),
                                                         selectInput(
                                                             inputId = "chordType",
                                                             label = "Type of Chord Diagram",
                                                             choices = c("Single Region", "Aggregate"),
                                                             selected = "Single Region"
                                                         ),
                                                         selectInput(
                                                             inputId = "chordRegion",
                                                             label = "Region:",
                                                             choices = c("East Asia & Pacific"),
                                                             selected = "East Asia & Pacific"
                                                         ),
                                                         selectInput(
                                                             inputId =  "chordDate", 
                                                             label = "Select year", 
                                                             choices = 2015:2019
                                                         )),
                                            mainPanel("Chord Diagram")
                                        ))
                    )
                ),
            tabItem(tabName = "indskills"),
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
}

shinyApp(ui, server)