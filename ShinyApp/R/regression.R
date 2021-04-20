##########################################################################
# This file contains the code for the regression module
##########################################################################

##########################################################################
# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R

##########################################################################
# End of pre load code
##########################################################################

country <- sort(unique(master$country_name))
year <- sort(unique(master$year))
region <- sort(unique(master$wb_region))
income <- sort(unique(master$wb_income))
industry <- sort(unique(master$isic_section_name))
skill <- sort(unique(master$skill_group_category))

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
regressionUI <- function(id = "regression") {
  ns <- NS(id)
  tagList(
    # Add your UI here
    navbarPage(
      tabPanel("Usage Guide"),
      tabPanel("Regression",
               sidebarLayout(
                 sidebarPanel(
                   h3("Variables"),
                   selectInput(
                     inputId = ns("yVar1"),
                     label = "Y Variable:",
                     choices = c("GDP Per Capita Growth" = "GDP_per_capita_growth",
                                 "Employment Growth" = "employment_growth",
                                 "Industry Migration" = "industry_migration",
                                 "Skill Migration" = "skill_migration"),
                     selected = "employment_growth"
                   ),
                   selectInput(
                     inputId = ns("xVar1"),
                     label = "X Variable:",
                     choices = c("GDP Per Capita Growth" = "GDP_per_capita_growth",
                                 "Employment Growth" = "employment_growth",
                                 "Industry Migration" = "industry_migration",
                                 "Skill Migration" = "skill_migration"),
                     selected = "industry_migration"
                   ),
                   tags$br(),
                   h3("Filters"),
                   selectInput(
                     inputId = ns("country1"),
                     label = "Country:",
                     choices = country,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("year1"),
                     label = "Year:",
                     choices = year,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("region1"),
                     label = "Region:",
                     choices = region,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("income1"),
                     label = "Income Level:",
                     choices = income,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("industry1"),
                     label = "Industry Section:",
                     choices = industry,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("skill1"),
                     label = "Skill Group:",
                     choices = skill,
                     multiple = TRUE
                   )
                 ),
                 mainPanel("Regression",
                           plotOutput(ns("RegressionOutput")))
               )),
      tabPanel("Scatter Plot",
               sidebarLayout(
                 sidebarPanel(
                   h3("Variables"),
                   selectInput(
                     inputId = ns("yVar2"),
                     label = "Y Variable:",
                     choices = c("GDP Per Capita Growth" = "GDP_per_capita_growth",
                                 "Employment Growth" = "employment_growth",
                                 "Industry Migration" = "industry_migration",
                                 "Skill Migration" = "skill_migration"),
                     selected = "employment_growth"
                   ),
                   selectInput(
                     inputId = ns("xVar2"),
                     label = "X Variable:",
                     choices = c("GDP Per Capita Growth" = "GDP_per_capita_growth",
                                 "Employment Growth" = "employment_growth",
                                 "Industry Migration" = "industry_migration",
                                 "Skill Migration" = "skill_migration"),
                     selected = "industry_migration"
                   ),
                   tags$br(),
                   h3("Filters"),
                   selectInput(
                     inputId = ns("country2"),
                     label = "Country:",
                     choices = country,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("year2"),
                     label = "Year:",
                     choices = year,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("region2"),
                     label = "Region:",
                     choices = region,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("income2"),
                     label = "Income Level:",
                     choices = income,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("industry2"),
                     label = "Industry Section:",
                     choices = industry,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("skill2"),
                     label = "Skill Group:",
                     choices = skill,
                     multiple = TRUE
                   )
               ),
               mainPanel("Scatter Plot",
                         plotlyOutput(ns("ScatterPlotOutput")))
               )),
      tabPanel("Correlation Matrix Analysis",
               sidebarLayout(
                 sidebarPanel(
                   h3("Variables"),
                   h5("- GDP per capita growth"),
                   h5("- Employment growth"),
                   h5("- Industry migration"),
                   h5("- Skill migration"),
                   tags$br(),
                   h3("Filters"),
                   selectInput(
                     inputId = ns("country3"),
                     label = "Country:",
                     choices = country,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("year3"),
                     label = "Year:",
                     choices = year,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("region3"),
                     label = "Region:",
                     choices = region,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("income3"),
                     label = "Income Level:",
                     choices = income,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("industry3"),
                     label = "Industry Section:",
                     choices = industry,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = ns("skill3"),
                     label = "Skill Group:",
                     choices = skill,
                     multiple = TRUE
                   )
                 ),
                 mainPanel("Correlation Matrix Analysis",
                           plotOutput(ns("CorrelationOutput")))
               ))
    
    
    # End UI here
  ))
}

regressionServer <- function(id = "regression") {
  moduleServer(id,
               function(input, output, session) {
                 # Server code should start from there
                 output$RegressionOutput <- renderPlot({
                   x <- unlist(master[,input$xVar1])
                   y <- unlist(master[,input$yVar1])
                   # (Add filter)

                   isolate({
                     ggscatterstats(master, input$xVar1, input$yVar1,
                                    xlab = NULL, ylab = NULL)
                     #ggplot(master, aes(x, y)) +
                      # geom_point(color="grey10",
                      #            size=1) +
                      # geom_smooth(method = "lm", se = FALSE)
                   })
                 })
                 
                 output$ScatterPlotOutput <- renderPlotly({
                   x <- unlist(master[,input$xVar2])
                   y <- unlist(master[,input$yVar2])
                   # (Add filter)

                   isolate({
                     p <- ggplot(master, aes(x, y)) +
                       labs(x = NULL, y = NULL) +
                       xlim(-2.00, 2.00) + ylim(-0.50, 0.50) +
                       geom_vline(xintercept = 0,
                                  linetype = "dashed",
                                  color = "grey60",
                                  size = 1) +
                       geom_hline(yintercept = 0,
                                  linetype = "dashed",
                                  color = "grey60",
                                  size = 1) +
                       geom_point(color=master$year,
                                  size=1,
                                  aes(textC = paste("Country: ", country_name),
                                      textY = paste("Year: ", year),
                                      textI = paste("Industry: ", industry_name),
                                      textS = paste("Skill: ", skill_group_name),
                                      textEG = paste("Employment Growth: ", employment_growth),
                                      textIM = paste("Industry Migration: ", industry_migration)))
                     # (Add colour legend)
                     ggplotly(p, tooltips = c("textC", "textY", "textI", "textS", "textEG", "textIM")) # (Edit tooltip)
                   })
                 })
                 
                 output$CorrelationOutput <- renderPlot(
                   # (Add filter)
                   
                   isolate({
                     ggcorrmat(master,
                               cor.vars = c(7, 10, 11, 12))
                   })
                 )
                 
                 # Server code should end above this line
               }
)}