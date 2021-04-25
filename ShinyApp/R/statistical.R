##########################################################################
# This file contains the code for the statistical module
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
statisticalRegressionUI <- function(id = "statistical") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = div(
          "Regression Plot",
          actionButton(ns("regressionInfo"), "", icon = icon("info", class =
                                                               "fa-fw")),
          actionButton(ns("regressionHelp"), "", icon = icon("question", class = "fa-fw"))
        ),
        width = 12,
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
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(htmlOutput(
              ns("RegressionRowsOutput")
            )),
            div(
              quickPop(
                actionButton(inputId = ns("apply1"), "Apply changes"),
                "Click on button to generate regression plot and results"
              )
            ))
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        plotOutput(ns("RegressionOutput"), height = "calc(100vh - 280px)") %>% withSpinner(type =
                                                             8),
        verbatimTextOutput(ns("RegResOutput")) %>% withSpinner(type =
                                                                 8)
      )
    )
  ))
}

statisticalScatterUI <- function(id = "statistical") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = div(
          "Scatter Plot",
          actionButton(ns("scatterInfo"), "", icon = icon("info", class =
                                                               "fa-fw")),
          actionButton(ns("scatterHelp"), "", icon = icon("question", class = "fa-fw"))
        ),
        width = 12,
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
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(htmlOutput(
              ns("ScatterPlotRowsOutput")
            )),
            div(
              quickPop(
                actionButton(inputId = ns("apply2"), "Apply changes"),
                "Click on button to generate interactive scatter plot"
              )
            ))
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        plotlyOutput(ns("ScatterPlotOutput"), height = "calc(100vh - 180px)") %>% withSpinner(type = 8)
      )
    )
  ))
}

statisticalCorrelationUI <- function(id = "statistical") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = div(
          "Correlation Matrix",
          actionButton(ns("corrmatrixInfo"), "", icon = icon("info", class =
                                                               "fa-fw")),
          actionButton(ns("corrmatrixHelp"), "", icon = icon("question", class = "fa-fw"))
        ),
        width = 12,
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
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(htmlOutput(
              ns("CorrelationRowsOutput")
            )),
            div(
              quickPop(
                actionButton(inputId = ns("apply3"), "Apply changes"),
                "Click on button to generate correlation matrix"
              )
            ))
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        plotOutput(ns("CorrelationOutput"), height = "calc(100vh - 180px)") %>% withSpinner(type =
                                                              8)
      )
    )
  ))
  
}

# End UI here

statisticalServer <- function(id = "statistical") {
  moduleServer(id,
               function(input, output, session) {
                 # Server code should start from there
                 
                 observeEvent(input$regressionInfo, {
                   quickAlert("Regression Info",
                              div(
                                p(
                                  "The regression plot and histogram visualise the relationship between two variables and their marginal distributions. The visualisations and statistical results will be shown. You can select two different variables to be shown and apply filters to analyse subset of the dataset."
                                ),
                                p(
                                  "Note that if the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation."
                                )
                              ))
                 })
                 
                 observeEvent(input$regressionHelp, {
                   quickAlert("Regression Help",
                              withTags({
                                div(ol(
                                  li(
                                    "Select two different variables to be shown on the regression plot, one for y axis and one for x axis e.g. Employment Growth and Industry Migration."
                                  ),
                                  li(
                                    "Select filters to be applied on the dataset. The filters include year, country, region, income level, industry section and skill group."
                                  ),
                                  li(
                                    "By default, all options are selected for each filter. On each filter, there are buttons to select all, deselect all or select individual options. Selected options will have a tick next to it."
                                  ),
                                  li(
                                    "If the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation. Select the appropriate filters to ensure results."
                                  ),
                                  li(
                                    "Once the variables and filters are selected, click on 'Apply changes'."
                                  ),
                                  li(
                                    "The spinning wheel indicates that the regression plot and results are being generated. Once completed, the visualisations will be shown."
                                  )
                                ))
                              }))
                 })
                 
                 observeEvent(input$scatterInfo, {
                   quickAlert("Scatter Plot Info",
                              div(
                                p(
                                  "The interactive scatter plot visualises values for two variables and is coloured by year. You can select two different variables to be shown and apply filters to analyse subset of the dataset."
                                ),
                                p(
                                  "You can hover over each point to find out its values i.e. x variable, y variable, year, country, industry and skill. In addition, you may carry out actions such as download plot as png, zoom, select."
                                ),
                                p(
                                  "Note that if the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation."
                                )
                              ))
                 })
                 
                 observeEvent(input$scatterHelp, {
                   quickAlert("Scatter Plot Help",
                              withTags({
                                div(ol(
                                  li(
                                    "Select two different variables to be shown on the interactive scatter plot, one for y axis and one for x axis e.g. Employment Growth and Industry Migration."
                                  ),
                                  li(
                                    "Select filters to be applied on the dataset. The filters include year, country, region, income level, industry section and skill group."
                                  ),
                                  li(
                                    "By default, all options are selected for each filter. On each filter, there are buttons to select all, deselect all or select individual options. Selected options will have a tick next to it."
                                  ),
                                  li(
                                    "If the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation. Select the appropriate filters to ensure results."
                                  ),
                                  li(
                                    "Once the variables and filters are selected, click on 'Apply changes'."
                                  ),
                                  li(
                                    "The spinning wheel indicates that the interactive scatter plot is being generated. Once completed, the visualisation will be shown."
                                  ),
                                  li(
                                    "Hover over each point on the interactive scatter plot to find out its values i.e. x variable, y variable, year, country, industry and skill."
                                  ),
                                  li(
                                    "There are buttons on the interactive scatter plot to carry out actions e.g. download plot as png, zoom, select."
                                  )
                                ))
                              }))
                 })
                 
                 observeEvent(input$corrmatrixInfo, {
                   quickAlert("Correlation Matrix Info",
                              div(
                                p(
                                  "The correlation matrix visualises the strength of relationship between pairs of variables. Four variables are used i.e. GDP per capita growth, Employment growth, Industry migration and Skill migration. You can apply filters to analyse subset of the dataset."
                                ),
                                p(
                                  "Note that if the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation."
                                )
                              ))
                 })
                 
                 observeEvent(input$corrmatrixHelp, {
                   quickAlert("Correlation Matrix Help",
                              withTags({
                                div(ol(
                                  li(
                                    "Four variables will be used for the correlation matrix i.e. GDP per capita growth, Employment growth, Industry migration and Skill migration."
                                  ),
                                  li(
                                    "Select filters to be applied on the dataset. The filters include year, country, region, income level, industry section and skill group."
                                  ),
                                  li(
                                    "By default, all options are selected for each filter. On each filter, there are buttons to select all, deselect all or select individual options. Selected options will have a tick next to it."
                                  ),
                                  li(
                                    "If the selected filters produce no data point, '0 rows selected' will be reflected and you will not be able to apply changes to generate the visualisation. Select the appropriate filters to ensure results."
                                  ),
                                  li(
                                    "Once the variables and filters are selected, click on 'Apply changes'."
                                  ),
                                  li(
                                    "The spinning wheel indicates that the correlation matrix is being generated. Once completed, the visualisation will be shown."
                                  )
                                ))
                              }))
                 })
                 
                 output$RegressionRowsOutput <- renderText({
                   filteredMaster1r <- master %>%
                     filter(
                       year %in% input$year1,
                       country_name %in% input$country1,
                       wb_region %in% input$region1,
                       wb_income %in% input$income1,
                       isic_section_name %in% input$industry1,
                       skill_group_category %in% input$skill1
                     )
                   
                   if (nrow(filteredMaster1r) > 0)
                   {
                     shinyjs::enable("apply1")
                     HTML(paste(nrow(filteredMaster1r), "rows selected."))
                   }
                   else {
                     shinyjs::disable("apply1")
                     HTML(paste0(
                       "<span style='color: red'>",
                       nrow(filteredMaster1r),
                       "</span>",
                       " rows selected."
                     ))
                   }
                 })
                 
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
                     
                     ggscatterstats(
                       filteredMaster1a,!!input$xVar1,!!input$yVar1,
                       ggplot.component = list(
                         ggplot2::xlim(-2.00, 2.00),
                         ylim(-0.50, 0.50),
                         labs(x = NULL, y = NULL),
                         geom_hline(
                           yintercept = 0,
                           linetype = "dashed",
                           color = "grey60",
                           size = 1
                         ),
                         geom_vline(
                           xintercept = 0,
                           linetype = "dashed",
                           color = "grey60",
                           size = 1
                         )
                       )
                     )
                     
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
                 
                 output$ScatterPlotRowsOutput <- renderText({
                   filteredMaster2r <- master %>%
                     filter(
                       year %in% input$year2,
                       country_name %in% input$country2,
                       wb_region %in% input$region2,
                       wb_income %in% input$income2,
                       isic_section_name %in% input$industry2,
                       skill_group_category %in% input$skill2
                     )
                   
                   if (nrow(filteredMaster2r) > 0)
                   {
                     shinyjs::enable("apply2")
                     HTML(paste(nrow(filteredMaster2r), "rows selected."))
                   }
                   else {
                     shinyjs::disable("apply2")
                     HTML(paste0(
                       "<span style='color: red'>",
                       nrow(filteredMaster2r),
                       "</span>",
                       " rows selected."
                     ))
                   }
                   
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
                 
                 output$CorrelationRowsOutput <- renderText({
                   filteredMaster3r <- master %>%
                     filter(
                       year %in% input$year3,
                       country_name %in% input$country3,
                       wb_region %in% input$region3,
                       wb_income %in% input$income3,
                       isic_section_name %in% input$industry3,
                       skill_group_category %in% input$skill3
                     )
                   
                   if (nrow(filteredMaster3r) > 0)
                   {
                     shinyjs::enable("apply3")
                     HTML(paste(nrow(filteredMaster3r), "rows selected."))
                   }
                   else {
                     shinyjs::disable("apply3")
                     HTML(paste0(
                       "<span style='color: red'>",
                       nrow(filteredMaster3r),
                       "</span>",
                       " rows selected."
                     ))
                   }
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
