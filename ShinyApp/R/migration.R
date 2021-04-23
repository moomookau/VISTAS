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

divergingPalettesChoices <- list(
  "Colour Blind Accessible" = list(
    "Red Yellow Blue" = "RdYlBu",
    "Red Blue" = "RdBu",
    "Purple Orange" = "PuOr",
    "Purple Green" = "PRGn",
    "Pink Green" = "PiYG",
    "Brown Green" = "BrBG"
  ),
  "Others" = list(
    "Spectral",
    "Red Yellow Green" = "RdYlGn",
    "Red Grey" = "RdGy"
  )
)

##########################################################################
# End of pre load code
##########################################################################

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
migrationUsageUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    box(plotOutput("plot1"), width = 8, height = "100%"),
    
    box(
      width = 4,
      "Box content here",
      br(),
      "More box content",
      sliderInput("slider", "Slider input:", 1, 100, 50),
      textInput("text", "Text input:")
    )
  ))
}

migrationChoroplethUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      quickPop(
        selectInput(
          inputId = ns("choroplethType"),
          label = "Type of Migration to View:",
          choices = c("Country", "Industry", "Skill")
        ),
        "Select the type of migration to view."
      ),
      uiOutput(outputId = ns("choroplethSelectedOutput")),
      quickPop(
        sliderInput(
          inputId = ns("choroplethYear"),
          label = "Year:",
          min = yearMin,
          max = yearMax,
          value = yearMax,
          step = 1,
          ticks = FALSE
        ),
        "Slide to choose the year to view."
      ),
      quickPop(
        sliderInput(
          inputId = ns("choroplethBins"),
          label = "Number of Bins:",
          min = 2,
          max = 10,
          value = 4,
          step = 2,
          ticks = FALSE
        ),
        "Slide to choose the number of bins to colour by. The number of bins need to be even to have equal number of bins for positive and negative values."
      ),
      quickPop(
        selectInput(
          inputId = ns("choroplethPalette"),
          label = "Colour Palette:",
          choices = divergingPalettesChoices,
          selected = "RdBu"
        ),
        "Choose the colour palette for the choropleth."
      ),
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      leafletOutput(ns("choroplethOutput"), height = "calc(100vh - 120px)")
    )
  ))
}

migrationChordUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      quickPop(
        pickerInput(
          inputId = ns("chordCountries"),
          label = "Countries:",
          choices = countriesGrouped,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        ),
        "Select the countries to display the Chord Diagram for. Chord Diagram will only be displayed when 3 or more countries are selected."
      ),
      quickPop(
        sliderInput(
          inputId = ns("chordYear"),
          label = "Year:",
          min = yearMin,
          max = yearMax,
          value = yearMax,
          step = 1,
          ticks = FALSE
        ),
        "Slide to choose the year to view."
      ),
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      chorddiagOutput(ns("chorddiagOutput"), height = "calc(100vh - 120px)") %>% withSpinner(type = 8)
    )
  ))
}

migrationSlopeUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      quickPop(
        selectInput(
          inputId = ns("slopeType"),
          label = "Type of Migration to View:",
          choices = c("Country", "Industry", "Skill")
        ),
        "Select the type of migration to view."
      ),
      uiOutput(outputId = ns("slopeSelectedOutput")),
      quickPop(
        sliderInput(
          inputId = ns("slopeTop"),
          label = "Show Top N Countries:",
          min = 10,
          max = nrow(countriesUnique),
          value = 30
        ),
        "Slide to choose number of countries to show. Countries will be chosen by the top N net migration values of 2019."
      ),
      quickPop(
        selectInput(
          inputId = ns("slopeColour"),
          label = "Color By:",
          choices = c("Income Group" = "wb_income", "Region" = "wb_region"),
          selected = "wb_region"
        ),
        "Select the variable to colour the lines by."
      ),
      quickPop(
        checkboxInput(inputId = ns("slopeLog"),
                      label = "Pseudolog Y-Axis?"),
        "Check to apply pseudolog transformation on the Y-Axis. Pseudolog allows for a log-like transformation for both positive and negative values."
      ),
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      plotOutput(ns("slopeOutput"), height = "calc(100vh - 120px)") %>% withSpinner(type =
                                                                                      8)
    )
  ))
}

migrationTreeUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    box(
      title = "Inputs",
      width = 3,
      solidHeader = TRUE,
      quickPop(
        selectInput(
          inputId = ns("treeType"),
          label = "Type of Migration to View:",
          choices = c("Country", "Industry", "Skill")
        ),
        "Select the type of migration to view."
      ),
      uiOutput(outputId = ns("treeSelectedOutput")),
      quickPop(
        sliderInput(
          inputId = ns("treeYear"),
          label = "Year:",
          min = yearMin,
          max = yearMax,
          value = yearMax,
          step = 1,
          ticks = FALSE
        ),
        "Slide to choose the year to view."
      ),
      quickPop(
        selectInput(
          inputId = ns("treeGroup"),
          label = "Group By:",
          choices = c("Income Group" = "wb_income", "Region" = "wb_region"),
          selected = "wb_region"
        ),
        "Select the variable to group the treemap by."
      ),
      quickPop(
        selectInput(
          inputId = ns("treeSize"),
          label = "Size By:",
          choices = c("Population", "GDP Per Capita (2010 US$)")
        ),
        "Select the variable to size the treemap by."
      ),
      quickPop(
        selectInput(
          inputId = ns("treePalette"),
          label = "Colour Palette:",
          choices = divergingPalettesChoices,
          selected = "RdBu"
        ),
        "Choose the colour palette for the choropleth."
      ),
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 9,
      solidHeader = TRUE,
      d3tree2Output(ns("treeOutput"), height = "calc(100vh - 120px)") %>% withSpinner(type =
                                                                                        8)
    )
  ))
}

migrationGeofacetUI <- function(id = "migration") {
  ns <- NS(id)
  tagList()
}

migrationServer <- function(id = "migration") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 
                 #######################################
                 # Choropleth Map - Start
                 #######################################
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$choroplethType), {
                   clearPops()
                   if (input$choroplethType == "Country") {
                     output$choroplethSelectedOutput <-
                       renderUI(
                         quickPop(
                           selectInput(
                             inputId = ns("choroplethCountry"),
                             label = "Base Country:",
                             choices = countriesGrouped
                           ),
                           "Select the base country to view the migration for. Migration will be shown with respect to the chosen base country."
                         )
                       )
                   }
                   else if (input$choroplethType == 'Industry') {
                     output$choroplethSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("choroplethIndustry"),
                           label = "Choose the Industry:",
                           choices = industriesGrouped
                         ),
                         "Select the industry to view the migration for."
                       ))
                   }
                   else {
                     output$choroplethSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("choroplethSkill"),
                           label = "Choose the Skill:",
                           choices = skillsGrouped
                         ),
                         "Select the skill to view the migration for."
                       ))
                   }
                 })
                 
                 output$choroplethOutput <- renderLeaflet({
                   leaflet(options = leafletOptions(zoomControl = FALSE,
                                                    dragging = FALSE)) %>%
                     addTiles() %>%
                     fitBounds(-170, 85, 170, -75) %>%
                     htmlwidgets::onRender(
                       "
    function(el, x) {
      var map = $('#migration-choroplethOutput').data('leaflet-map');
        function disableZoom(e) {map.scrollWheelZoom.disable();}

        $(document).on('mousemove', '*', disableZoom);

        map.on('click', function() {
          $(document).off('mousemove', '*', disableZoom);
          map.scrollWheelZoom.enable();
        });
    }
  "
                     )
                 })
                 
                 observeEvent(
                   c(
                     input$choroplethType,
                     input$choroplethCountry,
                     input$choroplethIndustry,
                     input$choroplethSkill,
                     input$choroplethYear,
                     input$sidebar,
                     input$choroplethBins,
                     input$choroplethPalette
                   ),
                   {
                     req(input$sidebar == "migrationChoropleth")
                     
                     if (input$choroplethType == "Country")
                     {
                       req(input$choroplethCountry)
                       
                       migrationChoropleth <-
                         countryMigrationPivot %>%
                         filter(base_country_name == input$choroplethCountry,
                                year == input$choroplethYear) %>%
                         select(target_country_name,
                                target_country_code,
                                net_per_10K) %>%
                         mutate(label = lapply(
                           paste0(
                             "Country: ",
                             target_country_name,
                             "<br/>Net migration: ",
                             net_per_10K
                           ),
                           htmltools::HTML
                         )) %>%
                         rename(country_name = target_country_name,
                                country_code = target_country_code)
                     }
                     else if (input$choroplethType == "Industry") {
                       req(input$choroplethIndustry)
                       
                       migrationChoropleth <-
                         industryMigrationPivot %>%
                         filter(industry_name == input$choroplethIndustry,
                                year == input$choroplethYear) %>%
                         select(country_name, country_code, net_per_10K) %>%
                         mutate(label = lapply(
                           paste0(
                             "Country: ",
                             country_name,
                             "<br/>Net Industry migration: ",
                             net_per_10K
                           ),
                           htmltools::HTML
                         ))
                     }
                     else {
                       req(input$choroplethSkill)
                       
                       migrationChoropleth <-
                         skillMigrationPivot %>%
                         filter(skill_group_name == input$choroplethSkill,
                                year == input$choroplethYear) %>%
                         select(country_name, country_code, net_per_10K) %>%
                         mutate(label = lapply(
                           paste0(
                             "Country: ",
                             country_name,
                             "<br/>Net Skill migration: ",
                             net_per_10K
                           ),
                           htmltools::HTML
                         ))
                     }
                     
                     migrationChoropleth$country_code = toupper(migrationChoropleth$country_code)
                     
                     # We use joinCountryData2Map to join the data with the spatial data
                     migrationChoroplethMap <-
                       joinCountryData2Map(migrationChoropleth,
                                           joinCode = "ISO2",
                                           nameJoinColumn = "country_code") %>%
                       spatialEco::sp.na.omit(col.name = "net_per_10K")
                     
                     # We compute the max migration, rounding up to the nearest 5
                     maxMigration = plyr::round_any(max(abs(migrationChoropleth$net_per_10K)), 5, f =
                                                      ceiling)
                     
                     # We create the bins and a diverging palette
                     bins <-
                       seq(-maxMigration,
                           maxMigration,
                           maxMigration / input$choroplethBins * 2)
                     pal <-
                       colorBin(input$choroplethPalette, bins = bins)
                     
                     # We plot the migration choropleth map
                     leafletProxy("choroplethOutput", data = migrationChoroplethMap) %>%
                       clearShapes() %>%
                       clearControls() %>%
                       addPolygons(
                         fillColor = ~ pal(net_per_10K),
                         label = ~ label,
                         weight = 2,
                         opacity = 1,
                         color = "white",
                         dashArray = "3",
                         fillOpacity = 0.7
                       ) %>%
                       addLegend_decreasing(
                         pal = pal,
                         values = ~ net_per_10K,
                         opacity = 0.7,
                         title = NULL,
                         position = "bottomleft",
                         decreasing = TRUE
                       ) %>%
                       fitBounds(-170, 85, 170, -75)
                   }
                 )
                 #######################################
                 # Choropleth Map - End
                 #######################################
                 
                 #######################################
                 # Chord Diagram - Start
                 #######################################
                 
                 output$chorddiagOutput <- renderChorddiag({
                   topCountries <- countryMigrationPivot %>%
                     filter(year == input$chordYear) %>%
                     mutate(abs_net_per_10K = abs(net_per_10K)) %>%
                     group_by(base_country_name) %>%
                     summarise(groupsum = sum(abs_net_per_10K)) %>%
                     slice_max(n = 20, order_by = groupsum)
                   
                   validate(need(
                     length(input$chordCountries) > 2,
                     'Please choose at least 3 countries to plot Chord Diagram'
                   ))
                   
                   # In order to use the chorddiag package, we create a matrix of the countries
                   chordDf <- countryMigrationPivot %>%
                     filter(
                       year == input$chordYear,
                       base_country_name %in% input$chordCountries,
                       target_country_name %in% input$chordCountries
                     ) %>%
                     select(base_country_name,
                            target_country_name,
                            net_per_10K) %>%
                     pivot_wider(
                       names_from = target_country_name,
                       values_from = net_per_10K,
                       values_fill = 0,
                       names_sort = TRUE
                     ) %>%
                     arrange(base_country_name) %>%
                     column_to_rownames("base_country_name")
                   
                   countryNames <- row.names(chordDf)
                   
                   # We create separate dataframes with positive and negative values
                   chordPosDf <- chordDf
                   chordPosDf[chordPosDf < 0] <- 0.0
                   chordNegDf <- chordDf
                   chordNegDf[chordNegDf >= 0] <- 0.0
                   chordNegDf <- abs(chordNegDf)
                   
                   # We convert the dataframes into matrices to be used with the chorddiag function
                   chordPosMatrix <- as.matrix(chordPosDf)
                   chordNegMatrix <- as.matrix(chordNegDf)
                   
                   # We plot the chord diagram
                   p <-
                     chorddiag(
                       chordPosMatrix,
                       #height = 1000,
                       #width = 1000,
                       groupnamePadding = 20,
                       groupnameFontsize = 10,
                       showTicks = FALSE,
                       groupThickness = 0.01,
                       palette = "Set3",
                       precision = 3,
                       showZeroTooltips = FALSE
                     )
                   p
                 })
                 
                 #######################################
                 # Chord Diagram - End
                 #######################################
                 
                 #######################################
                 # Slope Graph - Start
                 #######################################
                 
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$slopeType), {
                   clearPops()
                   if (input$slopeType == "Country") {
                     output$slopeSelectedOutput <-
                       renderUI(
                         quickPop(
                           selectInput(
                             inputId = ns("slopeCountry"),
                             label = "Base Country:",
                             choices = countriesGrouped
                           ),
                           "Select the base country to view the migration for. Migration will be shown with respect to the chosen base country."
                         )
                       )
                   }
                   else if (input$slopeType == 'Industry') {
                     output$slopeSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("slopeIndustry"),
                           label = "Industry:",
                           choices = industriesGrouped
                         ),
                         "Select the industry to view the migration for."
                       ))
                   }
                   else {
                     output$slopeSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("slopeSkill"),
                           label = "Skill:",
                           choices = skillsGrouped
                         ),
                         "Select the skill to view the migration for."
                       ))
                   }
                 })
                 
                 # Plotting of the Slope Graph
                 output$slopeOutput <- renderPlot({
                   # We create a dataframe to be used for the plotting of the slope graph
                   slopeDf <- {
                     if (input$slopeType == "Country") {
                       req(input$slopeCountry)
                       
                       topNCountries <-
                         countryMigrationPivot %>%
                         filter(base_country_name == input$slopeCountry,
                                year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(target_country_name)
                       
                       countryMigrationPivot %>%
                         filter(
                           base_country_name == input$slopeCountry,
                           target_country_name %in% topNCountries
                         ) %>%
                         rename(wb_region = target_country_wb_region,
                                wb_income = target_country_wb_income) %>%
                         rename(country_name = target_country_name,
                                colour_group = input$slopeColour)
                     }
                     else if (input$slopeType == "Industry")
                     {
                       req(input$slopeIndustry)
                       
                       topNCountries <-
                         industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry,
                                year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(country_name)
                       
                       industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry,
                                country_name %in% topNCountries) %>%
                         rename(colour_group = input$slopeColour)
                     }
                     else {
                       req(input$slopeSkill)
                       
                       topNCountries <- skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill,
                                year == 2019) %>%
                         slice_max(net_per_10K, n = input$slopeTop) %>%
                         pull(country_name)
                       
                       skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill,
                                country_name %in% topNCountries) %>%
                         rename(colour_group = input$slopeColour)
                     }
                   }
                   
                   # We plot the slope graph
                   if (input$slopeType == "Country") {
                     newggslopegraph(
                       dataframe = slopeDf,
                       Times = year,
                       Measurement = net_per_10K,
                       Grouping = country_name,
                       Title = paste("Net Country Migration for", input$slopeCountry),
                       SubTitle = NULL,
                       Caption = NULL,
                       WiderLabels = TRUE,
                       XTextSize = 8,
                       ColorGroup = colour_group
                     ) +
                       theme(legend.position = "bottom") +
                       scale_alpha(guide = 'none') +
                       scale_color_discrete(name = ifelse(
                         input$slopeColour == "wb_region",
                         "Region",
                         "Income Group"
                       )) +
                       {
                         if (input$slopeLog)
                           scale_y_continuous(trans = pseudolog10_trans)
                       }
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
                       XTextSize = 8,
                       ColorGroup = colour_group
                     ) +
                       theme(legend.position = "bottom") +
                       scale_alpha(guide = 'none') +
                       scale_color_discrete(name = ifelse(
                         input$slopeColour == "wb_region",
                         "Region",
                         "Income Group"
                       )) +
                       {
                         if (input$slopeLog)
                           scale_y_continuous(trans = pseudolog10_trans)
                       }
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
                       XTextSize = 8,
                       ColorGroup = colour_group
                     ) +
                       theme(legend.position = "bottom") +
                       scale_alpha(guide = 'none') +
                       scale_color_discrete(name = ifelse(
                         input$slopeColour == "wb_region",
                         "Region",
                         "Income Group"
                       )) +
                       {
                         if (input$slopeLog)
                           scale_y_continuous(trans = pseudolog10_trans)
                       }
                   }
                   
                 })
                 
                 #######################################
                 # Slope Graph - End
                 #######################################
                 
                 #######################################
                 # Treemap - Start
                 #######################################
                 
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$treeType), {
                   clearPops()
                   if (input$treeType == "Country") {
                     output$treeSelectedOutput <-
                       renderUI(
                         quickPop(
                           selectInput(
                             inputId = ns("treeCountry"),
                             label = "Base Country:",
                             choices = countriesGrouped
                           ),
                           "Select the base country to view the migration for. Migration will be shown with respect to the chosen base country."
                         )
                       )
                   }
                   else if (input$treeType == 'Industry') {
                     output$treeSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("treeIndustry"),
                           label = "Choose the Industry:",
                           choices = industriesGrouped
                         ),
                         "Select the industry to view the migration for."
                       ))
                   }
                   else {
                     output$treeSelectedOutput <-
                       renderUI(quickPop(
                         selectInput(
                           inputId = ns("treeSkill"),
                           label = "Choose the Skill:",
                           choices = skillsGrouped
                         ),
                         "Select the skill to view the migration for."
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
                     type = "value",
                     palette = input$treePalette,
                     title = "Hello",
                     title.legend = "Net Migration Per 10K LinkedIn users of Country"
                   )
                   d3tree2(tm, rootname = "World")
                   
                 })
                 
                 #######################################
                 # Treemap - End
                 #######################################
                 
               })
}