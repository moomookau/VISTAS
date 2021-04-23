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
regressionUsageUI <- function(id = "regression") {
  ns <- NS(id)
  tagList()
}

regressionRegressionUI <- function(id = "regression") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      h4("Variables"),
      quickPop(
        selectInput(
          inputId = ns("yVar1"),
          label = "Y Variable:",
          choices = c(
            "GDP Per Capita Growth" = "GDP_per_capita_growth",
            "Employment Growth" = "employment_growth",
            "Industry Migration" = "industry_migration",
            "Skill Migration" = "skill_migration"
          ),
          selected = "employment_growth"
        ),
        "Select variable for y axis of regression plot"
      ),
      quickPop(
        selectInput(
          inputId = ns("xVar1"),
          label = "X Variable:",
          choices = c(
            "GDP Per Capita Growth" = "GDP_per_capita_growth",
            "Employment Growth" = "employment_growth",
            "Industry Migration" = "industry_migration",
            "Skill Migration" = "skill_migration"
          ),
          selected = "industry_migration"
        ),
        "Select variable for x axis of regression plot"
      ),
      h4("Filters"),
      quickPop(
        pickerInput(
          inputId = ns("year1"),
          label = "Year:",
          choices = year,
          selected = year,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select year to include in regression plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("country1"),
          label = "Country:",
          choices = country,
          selected = country,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country to include in regression plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("region1"),
          label = "Region:",
          choices = region,
          selected = region,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select region to include in regression plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("income1"),
          label = "Income Level:",
          choices = income,
          selected = income,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country income level to include in regression plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("industry1"),
          label = "Industry Section:",
          choices = industry,
          selected = industry,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select industry section to include in regression plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("skill1"),
          label = "Skill Group:",
          choices = skill,
          selected = skill,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select skill group to include in regression plot"
      ),
      quickPop(
        actionButton(inputId = ns("apply1"), "Apply changes"),
        "Click on button to generate regression plot and results"
      ),
      helpText(
        "Note: 'No results for selection' will be shown if selected filters produce no data point."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      plotOutput(ns("RegressionOutput")) %>% withSpinner(type =
                                                           8),
      verbatimTextOutput(ns("RegResOutput")) %>% withSpinner(type =
                                                               8)
    )
  ))
}

regressionScatterUI <- function(id = "regression") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      h4("Variables"),
      quickPop(
        selectInput(
          inputId = ns("yVar2"),
          label = "Y Variable:",
          choices = c(
            "GDP Per Capita Growth" = "GDP_per_capita_growth",
            "Employment Growth" = "employment_growth",
            "Industry Migration" = "industry_migration",
            "Skill Migration" = "skill_migration"
          ),
          selected = "employment_growth"
        ),
        "Select variable for y axis of interactive scatter plot"
      ),
      quickPop(
        selectInput(
          inputId = ns("xVar2"),
          label = "X Variable:",
          choices = c(
            "GDP Per Capita Growth" = "GDP_per_capita_growth",
            "Employment Growth" = "employment_growth",
            "Industry Migration" = "industry_migration",
            "Skill Migration" = "skill_migration"
          ),
          selected = "industry_migration"
        ),
        "Select variable for x axis of interactive scatter plot"
      ),
      h4("Filters"),
      quickPop(
        pickerInput(
          inputId = ns("year2"),
          label = "Year:",
          choices = year,
          selected = year,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select year to include in interactive scatter plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("country2"),
          label = "Country:",
          choices = country,
          selected = country,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country to include in interactive scatter plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("region2"),
          label = "Region:",
          choices = region,
          selected = region,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select region to include in interactive scatter plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("income2"),
          label = "Income Level:",
          choices = income,
          selected = income,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country income level to include in interactive scatter plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("industry2"),
          label = "Industry Section:",
          choices = industry,
          selected = industry,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select industry section to include in interactive scatter plot"
      ),
      quickPop(
        pickerInput(
          inputId = ns("skill2"),
          label = "Skill Group:",
          choices = skill,
          selected = skill,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select skill group to include in interactive scatter plot"
      ),
      quickPop(
        actionButton(inputId = ns("apply2"), "Apply changes"),
        "Click on button to generate interactive scatter plot"
      ),
      helpText(
        "Note: 'No results for selection' will be shown if selected filters produce no data point."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      plotlyOutput(ns("ScatterPlotOutput")) %>% withSpinner(type = 8)
    )
  ))
}

regressionCorrelationUI <- function(id = "regression") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      h4("Variables"),
      h5("- GDP per capita growth"),
      h5("- Employment growth"),
      h5("- Industry migration"),
      h5("- Skill migration"),
      h4("Filters"),
      quickPop(
        pickerInput(
          inputId = ns("year3"),
          label = "Year:",
          choices = year,
          selected = year,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select year to include in correlation matrix"
      ),
      quickPop(
        pickerInput(
          inputId = ns("country3"),
          label = "Country:",
          choices = country,
          selected = country,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country to include in correlation matrix"
      ),
      quickPop(
        pickerInput(
          inputId = ns("region3"),
          label = "Region:",
          choices = region,
          selected = region,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select region to include in correlation matrix"
      ),
      quickPop(
        pickerInput(
          inputId = ns("income3"),
          label = "Income Level:",
          choices = income,
          selected = income,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select country income level to include in correlation matrix"
      ),
      quickPop(
        pickerInput(
          inputId = ns("industry3"),
          label = "Industry Section:",
          choices = industry,
          selected = industry,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select industry section to include in correlation matrix"
      ),
      quickPop(
        pickerInput(
          inputId = ns("skill3"),
          label = "Skill Group:",
          choices = skill,
          selected = skill,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        "Select skill group to include in correlation matrix"
      ),
      quickPop(
        actionButton(inputId = ns("apply3"), "Apply changes"),
        "Click on button to generate correlation matrix"
      ),
      helpText(
        "Note: 'No results for selection' will be shown if selected filters produce no data point."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      plotOutput(ns("CorrelationOutput")) %>% withSpinner(type =
                                                            8)
    )
  ))
  
}

# End UI here

regressionServer <- function(id = "regression") {
  moduleServer(id,
               function(input, output, session) {
                 # Server code should start from there
                 output$RegressionOutput <- renderPlot({
                   req(input$apply1 > 0) # Check that greater than 0 to ensure user has clicked the button once
                   
                   isolate({
                     
                     filteredMaster1a <- master %>%
                       filter(
                         year %in% input$year1,
                         country_name %in% input$country1,
                         wb_region %in% input$region1,
                         wb_income %in% input$income1,
                         isic_section_name %in% input$industry1,
                         skill_group_category %in% input$skill1
                       )
                     
                     validate(need(nrow(filteredMaster1a) > 0,
                                   "No results for selection."))

#                     x <- unlist(filteredMaster1a[, input$xVar1])
#                     y <- unlist(filteredMaster1a[, input$yVar1])
                     
                     ggscatterstats(filteredMaster1a,
                                    !!input$xVar1,
                                    !!input$yVar1,
                                    ggplot.component = list(ggplot2::
                                                            xlim(-2.00, 2.00),
                                                            ylim(-0.50, 0.50),
                                                            labs(x = NULL, y = NULL),
                                                            geom_hline(yintercept = 0,
                                                                       linetype = "dashed",
                                                                       color = "grey60",
                                                                       size = 1),
                                                            geom_vline(xintercept = 0,
                                                                       linetype = "dashed",
                                                                       color = "grey60",
                                                                       size = 1)))

#                     p1 <- ggplot(filteredMaster1a, aes(x, y)) +
#                       labs(x = NULL, y = NULL) +
#                       xlim(-2.00, 2.00) + ylim(-0.50, 0.50) +
#                       geom_point(color = "grey10",
#                                  size = 1,
#                                  alpha = 0.5) +
#                       geom_vline(
#                         xintercept = 0,
#                         linetype = "dashed",
#                         color = "grey60",
#                         size = 1
#                       ) +
#                       geom_hline(
#                         yintercept = 0,
#                         linetype = "dashed",
#                         color = "grey60",
#                         size = 1
#                       ) +
#                       geom_smooth(method = "lm", color = "firebrick3") +
#                       stat_regline_equation(label.x = -2, label.y = 0.45) +
#                       stat_cor(label.x = -2, label.y = 0.4)
#                     ggMarginal(p1, type = "histogram", fill = "darkseagreen")
                     
                   })
                 })
                 
                 output$RegResOutput <- renderPrint({
                   req(input$apply1 > 0) # Check that greater than 0 to ensure user has clicked the button once
                   
                   isolate({
                     
                     filteredMaster1b <- master %>%
                       filter(
                         year %in% input$year1,
                         country_name %in% input$country1,
                         wb_region %in% input$region1,
                         wb_income %in% input$income1,
                         isic_section_name %in% input$industry1,
                         skill_group_category %in% input$skill1
                       )
                     
                     validate(need(nrow(filteredMaster1b) > 0,
                                   "No results for selection."))
                     
                     x <- unlist(filteredMaster1b[, input$xVar1])
                     y <- unlist(filteredMaster1b[, input$yVar1])
                     
                     lm(y ~ x, filteredMaster1b) %>%
                       model_parameters()

#                     ols_regress(y ~ x, filteredMaster1b)
                     
                   })
                 })
                 
                 output$ScatterPlotOutput <- renderPlotly({
                   req(input$apply2 > 0)
                   
                   isolate({
                     
                     filteredMaster2 <- master %>%
                       filter(
                         year %in% input$year2,
                         country_name %in% input$country2,
                         wb_region %in% input$region2,
                         wb_income %in% input$income2,
                         isic_section_name %in% input$industry2,
                         skill_group_category %in% input$skill2
                       )
                     
                     validate(need(nrow(filteredMaster2) > 0,
                                   "No results for selection."))
                     
                     x <- unlist(filteredMaster2[, input$xVar2])
                     y <- unlist(filteredMaster2[, input$yVar2])
                     
                     p2 <- ggplot(
                       filteredMaster2,
                       aes(
                         x,
                         y,
                         color = factor(year),
                         textC = country_name,
                         textI = industry_name,
                         textS = skill_group_name
                       )
                     ) +
                       labs(x = NULL, y = NULL) +
                       xlim(-2.00, 2.00) + ylim(-0.50, 0.50) +
                       geom_vline(
                         xintercept = 0,
                         linetype = "dashed",
                         color = "grey60",
                         size = 1
                       ) +
                       geom_hline(
                         yintercept = 0,
                         linetype = "dashed",
                         color = "grey60",
                         size = 1
                       ) +
                       geom_point(size = 1,
                                  alpha = 0.5)
                     ggplotly(p2, tooltips = c(textC, textI, textS))
                   })
                 })
                 
                 output$CorrelationOutput <-
                   renderPlot({
                     req(input$apply3 > 0)
                     
                     isolate({
                       
                       filteredMaster3 <- master %>%
                         filter(
                           year %in% input$year3,
                           country_name %in% input$country3,
                           wb_region %in% input$region3,
                           wb_income %in% input$income3,
                           isic_section_name %in% input$industry3,
                           skill_group_category %in% input$skill3
                         )
                       
                       validate(need(nrow(filteredMaster3) > 0,
                                     "No results for selection."))
                       
                       ggcorrmat(filteredMaster3,
                                 cor.vars = c(7, 10, 11, 12))
                     })
                   })
                 
                 # Server code should end above this line
               })
}