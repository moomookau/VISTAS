###########################################################
# This file contains the code for the migration module
###########################################################

##########################################################################
# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R

# We prepare some variables to be used for the inputs
yearMin <- as.integer(min(countryMigrationPivot$year))
yearMax <- as.integer(max(countryMigrationPivot$year))

##########################################################################
# End of pre load code
##########################################################################

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
migrationUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(
    navbarPage(
      "Migration Analysis",
      tabPanel("Usage Guide"),
      tabPanel("Choropleth",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("choroplethCountry"),
                     label = "Country:",
                     choices = countriesGrouped
                   ),
                   sliderInput(
                     inputId = ns("choroplethYear"),
                     label = "Select year",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   )
                 ),
                 mainPanel(leafletOutput(ns(
                   "choroplethOutput"
                 ), height = "calc(100vh - 160px)"))
               )),
      tabPanel("Chord Diagram",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("chordType"),
                     label = "Type of Chord Diagram",
                     choices = c("Single Region", "Aggregate"),
                     selected = "Single Region"
                   ),
                   selectInput(
                     inputId = ns("chordRegion"),
                     label = "Region:",
                     choices = c("East Asia & Pacific"),
                     selected = "East Asia & Pacific"
                   ),
                   sliderInput(
                     inputId = ns("chordYear"),
                     label = "Select year",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   )
                 ),
                 mainPanel("Chord Diagram")
               )),
      tabPanel("Slope Graph",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = ns("slopeType"),
                     label = "Type of Migration to View:",
                     choices = c("Country", "Industry", "Skill")
                   ),
                   uiOutput(outputId = ns("slopeSelectedOutput")),
                   sliderInput(
                     inputId = ns("slopeTop"),
                     label = "Show Top N Countries:",
                     min = 10,
                     max = nrow(countriesUnique),
                     value = 30
                   )
                 ),
                 mainPanel(
                   plotOutput(ns("slopeOutput"), height = "calc(100vh - 160px)") %>% withSpinner(type =
                                                                                                   8)
                 )
               )),
      tabPanel("Treemap",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = ns("treeType"),
                     label = "Type of Migration to View:",
                     choices = c("Country", "Industry", "Skill")
                   ),
                   uiOutput(outputId = ns("treeSelectedOutput")),
                   sliderInput(
                     inputId = ns("treeYear"),
                     label = "Select year:",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   ),
                   selectInput(
                     inputId = ns("treeGroup"),
                     label = "Group By:",
                     choices = c("Income Group" = "wb_income", "Region" = "wb_region"),
                     selected = "wb_region"
                   ),
                   selectInput(
                     inputId = ns("treeSize"),
                     label = "Size By:",
                     choices = c("Population", "GDP Per Capita (2010 US$)")
                   ),
                 ),
                 mainPanel(
                   d3tree2Output(ns("treeOutput"), height = "calc(100vh - 160px)") %>% withSpinner(type =
                                                                                                     8)
                 )
               )),
      tabPanel("Geofacet Map")
    )
  )
}

migrationServer <- function(id = "migration") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 #######################################
                 # Choropleth Map - Start
                 #######################################
                 output$choroplethOutput <- renderLeaflet({
                   # We set the variables based on the UI inputs
                   filterCountry <- input$choroplethCountry
                   filterYear <- input$choroplethYear
                   
                   # We create the choropleth data using the variables above
                   migrationChoropleth <- countryMigrationPivot %>%
                     filter(base_country_name == filterCountry, year == filterYear) %>%
                     select(target_country_name, net_per_10K) %>%
                     mutate(label = lapply(
                       paste0(
                         "Country: ",
                         target_country_name,
                         "<br/>Net migration: ",
                         net_per_10K
                       ),
                       htmltools::HTML
                     ))
                   
                   # We use joinCountryData2Map to join the data with the spatial data
                   migrationChoroplethMap <-
                     joinCountryData2Map(migrationChoropleth,
                                         joinCode = "NAME",
                                         nameJoinColumn = "target_country_name") %>%
                     spatialEco::sp.na.omit(col.name = "net_per_10K")
                   
                   # We compute the max migration, rounding up to the nearest 5
                   maxMigration = plyr::round_any(max(abs(migrationChoropleth$net_per_10K)), 5, f =
                                                    ceiling)
                   
                   # We create the bins and a diverging palette
                   bins <- seq(-maxMigration, maxMigration, 5)
                   pal <- colorBin("RdBu", bins = bins)
                   
                   # We plot the migration choropleth map
                   migrationChoroplethMap %>%
                     leaflet() %>%
                     addTiles() %>%
                     addPolygons(
                       fillColor = ~ pal(net_per_10K),
                       label = ~ label,
                       weight = 2,
                       opacity = 1,
                       color = "white",
                       dashArray = "3",
                       fillOpacity = 0.7
                     ) %>%
                     addLegend(
                       pal = pal,
                       values = ~ net_per_10K,
                       opacity = 0.7,
                       title = NULL,
                       position = "bottomright"
                     )
                 })
                 #######################################
                 # Choropleth Map - End
                 #######################################
                 
                 #######################################
                 # Chord Diagram - Start
                 #######################################
                 
                 #######################################
                 # Chord Diagram - End
                 #######################################
                 
                 #######################################
                 # Slope Graph - Start
                 #######################################
                 
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$slopeType), {
                   if (input$slopeType == "Country") {
                     output$slopeSelectedOutput <-
                       renderUI(
                         selectInput(
                           inputId = ns("slopeCountry"),
                           label = "Choose the Base Country:",
                           choices = countriesGrouped
                         )
                       )
                   }
                   else if (input$slopeType == 'Industry') {
                     output$slopeSelectedOutput <-
                       renderUI(
                         selectInput(
                           inputId = ns("slopeIndustry"),
                           label = "Choose the Industry:",
                           choices = industriesGrouped
                         )
                       )
                   }
                   else {
                     output$slopeSelectedOutput <-
                       renderUI(selectInput(
                         inputId = ns("slopeSkill"),
                         label = "Choose the Skill:",
                         choices = skillsGrouped
                       ))
                   }
                 })
                 
                 # Plotting of the Slope Graph
                 output$slopeOutput <- renderPlot({
                   # We create a dataframe to be used for the plotting of the slope graph
                   slopeDf <- {
                     if (input$slopeType == "Country") {
                       req(input$slopeCountry)
                       
                       topNCountries <- countryMigrationPivot %>%
                         filter(base_country_name == input$slopeCountry, year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(target_country_name)
                       
                       countryMigrationPivot %>%
                         filter(
                           base_country_name == input$slopeCountry,
                           target_country_name %in% topNCountries
                         )
                     }
                     else if (input$slopeType == "Industry")
                     {
                       req(input$slopeIndustry)
                       
                       topNCountries <- industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry, year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(country_name)
                       
                       industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry,
                                country_name %in% topNCountries)
                     }
                     else {
                       req(input$slopeSkill)
                       
                       topNCountries <- skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill, year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(country_name)
                       
                       skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill,
                                country_name %in% topNCountries)
                     }
                   }
                   
                   # We plot the slope graph
                   if (input$slopeType == "Country") {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = target_country_name,
                       Title = paste("Net Country Migration for", input$slopeCountry),
                       SubTitle = NULL,
                       Caption = NULL,
                       WiderLabels = TRUE,
                       XTextSize = 8
                     )
                   }
                   else if (input$slopeType == "Industry")
                   {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = country_name,
                       Title = paste("Net Industry Migration for", input$slopeIndustry),
                       SubTitle = NULL,
                       Caption = NULL,
                       WiderLabels = TRUE,
                       XTextSize = 8
                     )
                   }
                   else {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = country_name,
                       Title = paste("Net Skill Migration for", input$slopeSkill),
                       SubTitle = NULL,
                       Caption = NULL,
                       WiderLabels = TRUE,
                       XTextSize = 8
                     )
                   }
                   
                 })
                 
                 #######################################
                 # Slope Graph - End
                 #######################################
                 
                 #######################################
                 # Treemap - Start
                 #######################################
                 
                 # Using d3treeR
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$treeType), {
                   if (input$treeType == "Country") {
                     output$treeSelectedOutput <-
                       renderUI(
                         selectInput(
                           inputId = ns("treeCountry"),
                           label = "Choose the Base Country:",
                           choices = countriesGrouped
                         )
                       )
                   }
                   else if (input$treeType == 'Industry') {
                     output$treeSelectedOutput <-
                       renderUI(
                         selectInput(
                           inputId = ns("treeIndustry"),
                           label = "Choose the Industry:",
                           choices = industriesGrouped
                         )
                       )
                   }
                   else {
                     output$treeSelectedOutput <-
                       renderUI(selectInput(
                         inputId = ns("treeSkill"),
                         label = "Choose the Skill:",
                         choices = skillsGrouped
                       ))
                   }
                 })
                 
                 output$treeOutput <- renderD3tree2({
                   if (input$treeSize == "Population") {
                     sizeDf <- populationDf %>%
                       filter(year == input$treeYear)
                   }
                   else {
                     sizeDf <- gdpPerCapitaDf %>%
                       filter(year == input$treeYear)
                   }
                   
                   if (input$treeType == "Country") {
                     req(input$treeCountry)
                     
                     treemapDf <- countryMigrationPivot %>%
                       filter(year == input$treeYear,
                              base_country_name == input$treeCountry) %>%
                       left_join(sizeDf,
                                 by = c("target_country_name" = "Country Name")) %>%
                       rename(
                         country_name = target_country_name,
                         wb_region = target_country_wb_region,
                         wb_income = target_country_wb_income
                       )
                   }
                   else if (input$treeType == "Industry") {
                     req(input$treeIndustry)
                     
                     treemapDf <- industryMigrationPivot %>%
                       filter(year == input$treeYear,
                              industry_name == input$treeIndustry) %>%
                       left_join(sizeDf,
                                 by = c("country_name" = "Country Name"))
                   }
                   else {
                     req(input$treeSkill)
                     
                     treemapDf <- skillMigrationPivot %>%
                       filter(year == input$treeYear,
                              skill_group_name == input$treeSkill) %>%
                       left_join(sizeDf,
                                 by = c("country_name" = "Country Name"))
                   }
                   
                   treemapDf <-
                     treemapDf %>% mutate(wb_region = str_replace_all(wb_region, "&", "and"))
                   
                   tm <- treemap(
                     treemapDf,
                     index = c(input$treeGroup, "country_name"),
                     vSize = "value",
                     vColor = "net_per_10K",
                     type = "value"
                   )
                   d3tree2(tm, rootname = "World")
                   
                 })
                 
                 #######################################
                 # Treemap - End
                 #######################################
                 
               })
}