###########################################################
# This file contains the code for the migration module
###########################################################

##########################################################################
# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R

# We pivot the 3 migration files to transpose the year and values into 2 columns
countryMigrationPivot <- countryMigration %>%
  pivot_longer(
    col = starts_with("net_per_10K_"),
    names_to = "year",
    names_prefix = "net_per_10K_",
    values_to = "net_per_10K"
  )

industryMigrationPivot <- industryMigration %>%
  pivot_longer(
    col = starts_with("net_per_10K_"),
    names_to = "year",
    names_prefix = "net_per_10K_",
    values_to = "net_per_10K"
  )

skillMigrationPivot <- skillMigration %>%
  pivot_longer(
    col = starts_with("net_per_10K_"),
    names_to = "year",
    names_prefix = "net_per_10K_",
    values_to = "net_per_10K"
  )

# We prepare some variables to be used for the inputs
countryVector <-
  sort(unique(countryMigrationPivot$base_country_name))
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
                     choices = countryVector
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
                   h3("Options"),
                   selectInput(
                     inputId = ns("slopeCountry"),
                     label = "Country:",
                     choices = countryVector
                   ),
                   selectInput(
                     inputId = ns("slopeType"),
                     label = "Type of Migration:",
                     choices = c("Country", "Industry", "Skill"),
                     selected = "Country"
                   ),
                   uiOutput(outputId = ns("slopeCategoryOutput"))
                 ),
                 mainPanel(
                   plotOutput(ns("slopeOutput"), height = "calc(100vh - 160px)") %>% withSpinner(type =
                                                                                                   8)
                 )
               )),
      tabPanel("Treemap",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("treeIndustry"),
                     label = "Industry",
                     choices = sort(unique(industryMigrationPivot$industry_name))
                   ),
                   sliderInput(
                     inputId = ns("treeYear"),
                     label = "Select year",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   ),
                   uiOutput(outputId = ns("treeCategoryOutput"))
                 ),
                 mainPanel(
                   d3tree2Output(ns("treeOutput"), height = "calc(100vh - 160px)"))
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
                 observeEvent(c(input$slopeType, input$slopeCountry), {
                   if (input$slopeType == 'Industry') {
                     countryIndustryMigration <- industryMigrationPivot %>%
                       filter(country_name == input$slopeCountry)
                     
                     industrySectionVector <-
                       sort(unique(countryIndustryMigration$isic_section_name))
                     
                     output$slopeCategoryOutput <-
                       renderUI(
                         selectInput(
                           inputId = ns('slopeGroup'),
                           label = 'Industry:',
                           choices = industrySectionVector
                         )
                       )
                   }
                   else if (input$slopeType == 'Skill') {
                     countrySkillMigration <- skillMigrationPivot %>%
                       filter(country_name == input$slopeCountry)
                     
                     skillGroupVector <-
                       sort(unique(countrySkillMigration$skill_group_category))
                     
                     output$slopeCategoryOutput <-
                       renderUI(selectInput(
                         inputId = ns('slopeGroup'),
                         label = 'Skill:',
                         choices = skillGroupVector
                       ))
                   }
                   # We hide the category input if "Country" is selected
                   else {
                     output$slopeCategoryOutput <-
                       renderUI(NULL)
                   }
                 })
                 
                 # Plotting of the Slope Graph
                 output$slopeOutput <- renderPlot({
                   # We get the variables from the various inputs
                   slopeCountry <- input$slopeCountry
                   slopeType <- input$slopeType
                   slopeGroup <- input$slopeGroup
                   
                   # We check that the slopeGroup has been provided
                   # If not, it may return an error
                   if (slopeType != "Country") {
                     req(slopeGroup)
                   }
                   
                   # We create a dataframe to be used for the plotting of the slope graph
                   slopeDf <- {
                     if (slopeType == "Country") {
                       countryMigrationPivot %>%
                         filter(base_country_name == slopeCountry)
                     }
                     else if (slopeType == "Skill")
                     {
                       skillMigrationPivot %>%
                         filter(country_name == slopeCountry,
                                skill_group_category == slopeGroup)
                     }
                     else {
                       industryMigrationPivot %>%
                         filter(country_name == slopeCountry,
                                isic_section_name == slopeGroup)
                     }
                   }
                   
                   # We check that there are results for the dataframe
                   req(nrow(slopeDf) > 0)
                   
                   # We plot the slope graph
                   if (slopeType == "Country") {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = target_country_name,
                       Title = paste("Net Country Migration for", slopeCountry),
                       SubTitle = NULL,
                       Caption = NULL,
                       WiderLabels = TRUE,
                       XTextSize = 8
                     )
                   }
                   else if (slopeType == "Industry")
                   {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = industry_name,
                       Title = paste(
                         "Net Industry Migration for",
                         slopeCountry,
                         "for",
                         slopeGroup,
                         "Industry Section"
                       ),
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
                       Grouping = skill_group_name,
                       Title = paste(
                         "Net Skill Migration for",
                         slopeCountry,
                         "for",
                         slopeGroup,
                         "Skill Group"
                       ),
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
                 output$treeOutput <- renderD3tree2({
                   
                   treemapDf <- industryMigrationPivot %>%
                     filter(year == input$treeYear,
                            industry_name == input$treeIndustry)
                   
                   treemapDf <- treemapDf %>% mutate(wb_region = str_replace_all(wb_region, "&", "and"))
                   treemapDf$abs_net_per_10K <- abs(treemapDf$net_per_10K)
                   
                   tm <- treemap(
                     treemapDf,
                     index=c("wb_region","country_name"),
                     vSize="abs_net_per_10K",
                     vColor="net_per_10K",
                     type="value"
                     )
                   d3tree2(tm, rootname="World")
                   
                 })
                 
                 #######################################
                 # Treemap - End
                 #######################################
                 
               })
}