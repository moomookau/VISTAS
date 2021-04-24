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

sequentialPalettesChoices <- list(
  "Yellow Orange Red" = "YlOrRd",
  "Yellow Orange Blue" = "YlOrBr",
  "Yellow Green Blue" = "YlGnBu",
  "Yellow Green" = "YlGn",
  "Reds" = "Reds",
  "Red Purple" = "RdPu",
  "Purples" = "Purples",
  "Purple Red" = "PuRd",
  "Purple Blue Green" = "PuBuGn",
  "Purple Blue" = "PuBu",
  "Orange Red" = "OrRd",
  "Oranges" = "Oranges",
  "Greys" = "Greys",
  "Greens" = "Greens",
  "Green Blue" = "GnBu",
  "Blue Purple" = "BuPu",
  "Blue Green" = "BuGn",
  "Blues" = "Blues"
)

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
    box(
      width = 4,
      title = "Introduction",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 4,
      title = "Choropleth",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 4,
      title = "Chord Diagram",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    )
  ),
  fluidRow(
    box(
      width = 4,
      title = "Slope Graph",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 4,
      title = "Treemap",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    box(
      width = 4,
      title = "Geofacet",
      helpText(
        "Add help text or description here. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    )
  ),
  fluidRow(box(
    width = 12,
    title = "Maybe",
    helpText(
      "How should we do this usage guide? Maybe we can use tab panel instead."
    )
  )))
}

migrationChoroplethUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        title = "Choropleth Map",
        collapsible = TRUE,
        helpText(
          p(
            "The choropleth visualisation is a thematic map where countries are coloured by migration values. You can view migration by Country, Industry or Skill and select a country, industry or skill to view. You may also select the year you want to view."
          ),
          p(
            "The visualisation can be coloured by Bins, Numeric or Quantile, and additional options such as Number of Bins or Quantiles will be displayed accordingly."
          ),
          p(
            "Finally, you can choose the colour palette for colouring the choropleth map. You may choose from diverging colour palettes when colouring by Bins and Numeric, and sequential colour palettes when colouring by Quantiles."
          )
        )
      ),
      box(
        title = "Inputs",
        width = 12,
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
            ticks = FALSE,
            sep = ""
          ),
          "Slide to choose the year to view."
        ),
        quickPop(
          selectInput(
            inputId = ns("choroplethColourType"),
            label = "Colour By:",
            choices = c("Bins", "Numeric", "Quantile"),
            selected = "Bins"
          ),
          "Slide to choose the number of bins to colour by. The number of bins need to be even to have equal number of bins for positive and negative values."
        ),
        uiOutput(outputId = ns("choroplethColourOptionsOutput")),
        uiOutput(outputId = ns("choroplethColourPaletteOutput"))
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        title = textOutput(ns('choroplethTitle')),
        leafletOutput(ns("choroplethOutput"), height = "calc(100vh - 160px)")
      )
    )
  ))
}

migrationChordUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = "Chord Diagram",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        helpText(
          p(
            "A chord diagram is a graphical method of displaying the inter-relationships between data in a matrix. The data are arranged radially around a circle with the relationships between the data points drawn as arcs connecting the data."
          ),
          p(
            "This visualisation shows the positive or negative net migration based on the perspective selected for a selected year. The chord diagram can be ordered by either income group or region."
          ),
          p(
            "Positive net migration refers to more LinkedIn users migrating from the target country to the selected base country, while negative net migration refers to more LinkedIn users migrating from the selected base country to the target country."
          )
        )
      ),
      box(
        title = "Inputs",
        width = 12,
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
            ticks = FALSE,
            sep = ""
          ),
          "Slide to choose the year to view."
        ),
        quickPop(
          radioButtons(
            inputId = ns("chordOrder"),
            label = "Order:",
            choices = c("Income Group" = "base_country_wb_income", "Region" = "base_country_wb_region"),
            selected = "base_country_wb_region"
          ),
          "Choose how to order the chord diagram."
        ),
        quickPop(
          radioButtons(
            inputId = ns("chordInOut"),
            label = "Migration Perspective:",
            choices = c("Inwards (Positive)", "Outwards (Negative)"),
            selected = "Inwards (Positive)"
          ),
          "Choose the net migration perspective to view. Inwards refer to positive net migration with respect to the chosen country while Outwards refer to negative net migration with respect to the chosen country."
        )
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        title = textOutput(ns('chordTitle')),
        chorddiagOutput(ns("chorddiagOutput"), height = "calc(100vh - 160px)") %>% withSpinner(type = 8)
      )
    )
  ))
}

migrationSlopeUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = "Slope Graph",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        helpText(
          p(
            "Slope graphs are visualisations which allow a user to compare changes usually over time for a list of categorical variables."
          ),
          p(
            "This visualisation shows the net migration with respect to a base country, industry or skill over time."
          ),
          p(
            "As the slope graph may be too cluttered if there are too many lines on the plot, there is an option to limit the number of lines to the Top, Bottom or Top & Bottom N Countries. The lines can be coloured by either Region or Income Group. The Y-Axis can be transformed using the pseudolog function if the lines are too cluttered at certain values."
          )
        )
      ),
      box(
        title = "Inputs",
        width = 12,
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
          selectInput(
            inputId = ns("slopeTopBottom"),
            label = "Show Top or Bottom N (based on 2019 values):",
            choices = c("Top", "Bottom", "Top/Bottom"),
            selected = "Top"
          ),
          "Select to show Top, Bottom or both Top and Bottom N countries. Countries will be chosen by the net migration values of 2019."
        ),
        quickPop(
          sliderInput(
            inputId = ns("slopeN"),
            label = "Top/Bottom N:",
            min = 10,
            max = nrow(countriesUnique),
            value = 20
          ),
          "Slide to choose number of countries to show."
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
        )
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        plotOutput(ns("slopeOutput"), height = "calc(100vh - 120px)") %>% withSpinner(type =
                                                                                        8)
      )
    )
  ))
}

migrationTreeUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 3,
      style = 'padding: 0px;',
      box(
        title = "Treemap",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        helpText(
          p(
            "A Treemap displays hierarchical data as a set of nested rectangles. Each group is represented by a rectangle, which area is proportional to its value."
          ),
          p(
            "This visualisation shows the net country, industry or skill migration by the colour of the rectangles, and either the population or GDP per Capita by the size of the rectangles."
          ),
          p(
            "The rectangles can be nested by Region or Income Group of the countries, and clicking on a rectangle will show the individual countries within that rectangle."
          ),
          p("Clicking on the top of the plot allows going up the hierarchy.")
        )
      ),
      box(
        title = "Inputs",
        width = 12,
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
            ticks = FALSE,
            sep = ""
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
        )
      )
    ),
    column(
      width = 9,
      style = 'padding: 0px;',
      box(
        width = 12,
        solidHeader = TRUE,
        title = textOutput(ns('treeTitle')),
        d3tree2Output(ns("treeOutput"), height = "calc(100vh - 160px)") %>% withSpinner(type =
                                                                                          8)
      )
    )
  ))
}

migrationGeofacetUI <- function(id = "migration") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 6,
      style = 'padding: 0px;',
      box(
        title = "Geofacet",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        helpText(
          p(
            "To geofacet is to take data representing different geographic entities and apply a visualisation method to the data for each entity, with the resulting set of visualisations being laid out in a grid that mimics the original geographic topology as closely as possible."
          ),
          p(
            "This visualisation shows the net migration with respect to a base country, industry or skill on a geofacet plot. The visualisation will show multiple line graphs by default, but if the migration type selected is industry or skill, you may select multiple industries or skills. Selecting multiple industries or skills will change the graphs to a bar chart."
          ),
          p(
            "As a geofacet plot takes some time to render, a render button is provided for the user to click on the necessary options have been selected."
          )
        )
      )
    ),
    column(
      width = 6,
      style = 'padding: 0px;',
      box(
        title = "Inputs",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(
            width = 4,
            quickPop(
              selectInput(
                inputId = ns("geofacetType"),
                label = "Type of Migration to View:",
                choices = c("Country", "Industry", "Skill")
              ),
              "Select the type of migration to view."
            )
          ),
          column(width = 4,
                 uiOutput(outputId = ns(
                   "geofacetSelectedOutput"
                 )),
                 uiOutput(outputId = ns(
                   "geofacetYearOutput"
                 ))),
          column(
            width = 4,
            quickPop(
              pickerInput(
                inputId = ns("geofacetRegions"),
                label = "Regions:",
                choices = regionsUnique,
                selected = regionsUnique,
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 5"
                ),
                multiple = TRUE
              ),
              "Select the regions to display the geofacet for.",
              placement = "left"
            ),
            quickPop(
              actionButton(inputId = ns("geofacetApply"),
                           label = "Render!"),
              "Click on button to render plot.",
              placement = "left"
            )
          )
        )
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      plotOutput(ns("geofacetOutput"), height = "calc(100vh - 200px)") %>% withSpinner(type = 8)
    )
  ))
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
                 
                 # Render the Colour Options and Colour Palette Inputs
                 observeEvent(c(input$choroplethColourType), {
                   output$choroplethColourOptionsOutput <- renderUI({
                     clearPops()
                     if (input$choroplethColourType == "Bins") {
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
                       )
                     }
                     else if (input$choroplethColourType == "Quantile") {
                       quickPop(
                         sliderInput(
                           inputId = ns("choroplethQuantiles"),
                           label = "Number of Quantiles:",
                           min = 2,
                           max = 10,
                           value = 4,
                           step = 1,
                           ticks = FALSE
                         ),
                         "Slide to choose the number of quantiles to colour by."
                       )
                     }
                     else {
                       # No options required for "Numeric"
                       NULL
                     }
                   })
                   
                   output$choroplethColourPaletteOutput <-
                     renderUI({
                       clearPops()
                       if (input$choroplethColourType == "Quantile") {
                         paletteChoices <- sequentialPalettesChoices
                         selectedChoice <- "Blues"
                       }
                       else {
                         paletteChoices <- divergingPalettesChoices
                         selectedChoice <- "RdBu"
                       }
                       
                       quickPop(
                         selectInput(
                           inputId = ns("choroplethPalette"),
                           label = "Colour Palette:",
                           choices = paletteChoices,
                           selected = selectedChoice
                         ),
                         "Choose the colour palette for the choropleth. Sequential palette choices are provided for Quantile type and Diverging palette choices are provided for Bin and Numeric type."
                       )
                     })
                   
                 })
                 
                 output$choroplethOutput <- renderLeaflet({
                   leaflet(options = leafletOptions(zoomControl = FALSE,
                                                    dragging = FALSE)) %>%
                     addTiles() %>%
                     fitBounds(-170, 85, 170,-75) %>%
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
                     input$choroplethColourType,
                     input$choroplethBins,
                     input$choroplethQuantiles,
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
                     
                     migrationChoroplethMap <- worldPolygons50 %>%
                       right_join(migrationChoropleth, by = c("ISO_A2" = "country_code"))
                     
                     maxMigration = max(abs(migrationChoropleth$net_per_10K))
                     
                     pal <- {
                       if (input$choroplethColourType == "Bins") {
                         req(input$choroplethBins)
                         req(input$choroplethPalette %in% unlist(divergingPalettesChoices))
                         colorBin(
                           input$choroplethPalette,
                           domain = c(-maxMigration, maxMigration) ,
                           bins = input$choroplethBins,
                           pretty = FALSE
                         )
                       }
                       else if (input$choroplethColourType == "Numeric") {
                         req(input$choroplethPalette %in% unlist(divergingPalettesChoices))
                         colorNumeric(input$choroplethPalette,
                                      domain = c(-maxMigration, maxMigration))
                       }
                       else {
                         req(input$choroplethQuantiles)
                         req(input$choroplethPalette %in% sequentialPalettesChoices)
                         colorQuantile(
                           input$choroplethPalette,
                           domain = migrationChoropleth$net_per_10K,
                           n = input$choroplethQuantiles
                         )
                       }
                     }
                     
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
                         #title = NULL,
                         position = "bottomleft",
                         decreasing = TRUE,
                         title = "Net migration<br>(per 10K LinkedIn users of country)"
                       ) %>%
                       fitBounds(-170, 85, 170,-75)
                     
                     output$choroplethTitle <- renderText({
                       paste(
                         "Choropleth Map for Net",
                         input$choroplethType,
                         "Migration for",
                         {
                           if (input$choroplethType == "Country") {
                             input$choroplethCountry
                           }
                           else if (input$choroplethType == "Industry") {
                             input$choroplethIndustry
                           }
                           else {
                             input$choroplethSkill
                           }
                         },
                         "by Country for",
                         input$choroplethYear
                       )
                     })
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
                   
                   countriesOrder <- countryMigrationPivot %>%
                     filter(
                       base_country_name %in% input$chordCountries,
                       target_country_name %in% input$chordCountries
                     ) %>%
                     distinct(base_country_name,!!sym(input$chordOrder)) %>%
                     select(base_country_name,!!sym(input$chordOrder)) %>%
                     arrange(!!sym(input$chordOrder), base_country_name) %>%
                     mutate(tooltip = paste0(base_country_name, " (", !!sym(input$chordOrder), ")"))
                   
                   tooltipNames <- countriesOrder$tooltip
                   
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
                     arrange(match(base_country_name, countriesOrder$base_country_name)) %>%
                     column_to_rownames("base_country_name")
                   
                   # We reorder the columns as well
                   chordDf <-
                     chordDf[, countriesOrder$base_country_name]
                   
                   # We create the matrix depending on the view
                   if (input$chordInOut == "Inwards (Positive)")
                   {
                     chordPosDf <- chordDf
                     chordPosDf[chordPosDf < 0] <- 0.0
                     chordMatrix <- as.matrix(chordPosDf)
                     connectorSymbol = " &#8592; "
                   }
                   else {
                     chordNegDf <- chordDf
                     chordNegDf[chordNegDf >= 0] <- 0.0
                     chordNegDf <- abs(chordNegDf)
                     chordMatrix <- as.matrix(chordNegDf)
                     connectorSymbol = " &#8594; "
                   }
                   
                   # We render the title of the plot
                   output$chordTitle <- renderText({
                     paste(
                       input$chordInOut,
                       "Net Migration (per 10K LinkedIn users of country) for",
                       input$chordYear
                     )
                   })
                   
                   # We plot the chord diagram
                   chorddiag(
                     chordMatrix,
                     groupnameFontsize = 10,
                     tooltipNames = tooltipNames,
                     showTicks = FALSE,
                     groupThickness = 0.01,
                     palette = "Set3",
                     precision = 3,
                     showZeroTooltips = FALSE,
                     tooltipGroupConnector = connectorSymbol
                   )
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
                   
                   if (grepl("Top", input$slopeTopBottom))
                   {
                     if (input$slopeType == "Country") {
                       req(input$slopeCountry)
                       
                       topNCountries <-
                         countryMigrationPivot %>%
                         filter(base_country_name == input$slopeCountry,
                                year == 2019) %>%
                         slice_max(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(target_country_name)
                     }
                     else if (input$slopeType == "Industry") {
                       req(input$slopeIndustry)
                       
                       topNCountries <-
                         industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry,
                                year == 2019) %>%
                         slice_max(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(country_name)
                     }
                     else {
                       req(input$slopeSkill)
                       
                       topNCountries <- skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill,
                                year == 2019) %>%
                         slice_max(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(country_name)
                     }
                   }
                   if (grepl("Bottom", input$slopeTopBottom))
                   {
                     if (input$slopeType == "Country") {
                       req(input$slopeCountry)
                       
                       bottomNCountries <-
                         countryMigrationPivot %>%
                         filter(base_country_name == input$slopeCountry,
                                year == 2019) %>%
                         slice_min(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(target_country_name)
                     }
                     else if (input$slopeType == "Industry") {
                       req(input$slopeIndustry)
                       
                       bottomNCountries <-
                         industryMigrationPivot %>%
                         filter(industry_name == input$slopeIndustry,
                                year == 2019) %>%
                         slice_min(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(country_name)
                     }
                     else {
                       req(input$slopeSkill)
                       
                       topNCountries <- skillMigrationPivot %>%
                         filter(skill_group_name == input$slopeSkill,
                                year == 2019) %>%
                         slice_min(net_per_10K,
                                   n = input$slopeN,
                                   with_ties = FALSE) %>%
                         pull(country_name)
                     }
                   }
                   
                   if (input$slopeTopBottom == "Top/Bottom") {
                     filterCountries <- c(topNCountries, bottomNCountries)
                     print(filterCountries)
                   } else if (input$slopeTopBottom == "Top") {
                     filterCountries <- topNCountries
                   } else {
                     filterCountries <- bottomNCountries
                   }
                   
                   if (input$slopeType == "Country") {
                     req(input$slopeCountry)
                     
                     slopeDf <- countryMigrationPivot %>%
                       filter(
                         base_country_name == input$slopeCountry,
                         target_country_name %in% filterCountries
                       ) %>%
                       rename(wb_region = target_country_wb_region,
                              wb_income = target_country_wb_income) %>%
                       rename(country_name = target_country_name,
                              colour_group = input$slopeColour)
                   }
                   else if (input$slopeType == "Industry")
                   {
                     req(input$slopeIndustry)
                     
                     slopeDf <- industryMigrationPivot %>%
                       filter(industry_name == input$slopeIndustry,
                              country_name %in% filterCountries) %>%
                       rename(colour_group = input$slopeColour)
                   }
                   else {
                     req(input$slopeSkill)
                     
                     slopeDf <- skillMigrationPivot %>%
                       filter(skill_group_name == input$slopeSkill,
                              country_name %in% filterCountries) %>%
                       rename(colour_group = input$slopeColour)
                   }
                   
                   # We plot the slope graph
                   newggslopegraph(
                     dataframe = slopeDf,
                     Times = year,
                     Measurement = net_per_10K,
                     Grouping = country_name,
                     Title = {
                       if (input$slopeType == "Country") {
                         paste("Net Country Migration for", input$slopeCountry)
                       }
                       else if (input$slopeType == "Industry") {
                         paste("Net Industry Migration for", input$slopeIndustry)
                       }
                       else{
                         paste("Net Skill Migration for", input$slopeSkill)
                       }
                     },
                     SubTitle = NULL,
                     Caption = NULL,
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
                   
                   output$treeTitle <- renderText({
                     paste(
                       input$treeType,
                       "Net Migration for",
                       {
                         if (input$treeType == "Country") {
                           input$treeCountry
                         }
                         else if (input$treeType == "Industry") {
                           input$treeIndustry
                         }
                         else {
                           input$treeSkill
                         }
                       },
                       "by",
                       ifelse(
                         input$treeGroup == "wb_region",
                         "Region",
                         "Income Group"
                       ),
                       "for",
                       input$treeYear,
                       "(Sized By",
                       paste0(input$treeSize,
                              ")")
                     )
                   })
                   
                   treemapDf <-
                     treemapDf %>% mutate(wb_region = str_replace_all(wb_region, "&", "and"))
                   
                   tm <- treemap(
                     treemapDf,
                     index = c(input$treeGroup, "country_name"),
                     vSize = "value",
                     vColor = "net_per_10K",
                     type = "value",
                     palette = input$treePalette,
                     title.legend = "Net Migration (per 10K LinkedIn users of country)",
                     fontsize.legend = 6
                   )
                   d3tree2(tm, rootname = "World")
                   
                 })
                 
                 #######################################
                 # Treemap - End
                 #######################################
                 
                 #######################################
                 # Geofacet - Start
                 #######################################
                 # We create an observeEvent to have dynamic UI inputs
                 observeEvent(c(input$geofacetType), {
                   clearPops()
                   if (input$geofacetType == "Country") {
                     output$geofacetSelectedOutput <-
                       renderUI(
                         quickPop(
                           selectInput(
                             inputId = ns("geofacetCountry"),
                             label = "Base Country:",
                             choices = countriesGrouped
                           ),
                           "Select the base country to view the migration for. Migration will be shown with respect to the chosen base country."
                         )
                       )
                   }
                   else if (input$geofacetType == 'Industry') {
                     output$geofacetSelectedOutput <-
                       renderUI(
                         quickPop(
                           pickerInput(
                             inputId = ns("geofacetIndustries"),
                             label = "Industries:",
                             choices = industriesGrouped,
                             options = list(
                               "max-options" = 5,
                               "max-options-text" = "Maximum number of industries have been selected.",
                               size = 10
                             ),
                             multiple = TRUE
                           ),
                           "Select the industry to view the migration for. If one industry is selected, a line chart will show the migration trend over the years. If more than one industry is selected, a bar chart will display the migration for the industries for a selected year."
                         )
                       )
                   }
                   else {
                     output$geofacetSelectedOutput <-
                       renderUI(
                         quickPop(
                           pickerInput(
                             inputId = ns("geofacetSkills"),
                             label = "Skills:",
                             choices = skillsGrouped,
                             options = list(
                               "max-options" = 5,
                               "max-options-text" = "Maximum number of skills have been selected.",
                               size = 10
                             ),
                             multiple = TRUE
                           ),
                           "Select the skill to view the migration for. If one skill is selected, a line chart will show the migration trend over the years. If more than one skill is selected, a bar chart will display the migration for the skills for a selected year."
                         )
                       )
                   }
                 })
                 
                 output$geofacetYearOutput <- renderUI({
                   req((
                     input$geofacetType == "Industry" &&
                       length(input$geofacetIndustries) > 1
                   ) ||
                     (
                       input$geofacetType == "Skill" && length(input$geofacetSkills) > 1
                     )
                   )
                   
                   quickPop(
                     sliderInput(
                       inputId = ns("geofacetYear"),
                       label = NULL,
                       min = yearMin,
                       max = yearMax,
                       value = yearMax,
                       step = 1,
                       ticks = FALSE,
                       sep = ""
                     ),
                     "Slide to choose the year to view."
                   )
                 })
                 
                 output$geofacetOutput <- renderPlot({
                   req(input$geofacetApply > 0)
                   
                   isolate({
                     validate(need(
                       length(input$geofacetRegions) > 0,
                       "Please select at least one region."
                     ))
                     
                     geofacetBar <- FALSE
                     
                     if (input$geofacetType == "Country")
                     {
                       migrationGeofacet <-
                         countryMigrationPivot %>%
                         filter(
                           base_country_name == input$geofacetCountry,
                           target_country_wb_region %in% input$geofacetRegions
                         ) %>%
                         select(target_country_name,
                                target_country_code,
                                net_per_10K,
                                year) %>%
                         rename(country_name = target_country_name,
                                country_code = target_country_code)
                     }
                     else if (input$geofacetType == "Industry") {
                       validate(need(
                         length(input$geofacetIndustries) > 0,
                         "Please choose at least one industry"
                       ))
                       if (length(input$geofacetIndustries) > 1)
                       {
                         req(input$geofacetYear)
                         geofacetBar <- TRUE
                         
                         migrationGeofacet <-
                           industryMigrationPivot %>%
                           filter(
                             industry_name %in% input$geofacetIndustries,
                             wb_region %in% input$geofacetRegions,
                             year == input$geofacetYear
                           ) %>%
                           select(industry_name,
                                  country_name,
                                  country_code,
                                  net_per_10K) %>%
                           rename(bars = industry_name)
                       }
                       else {
                         migrationGeofacet <-
                           industryMigrationPivot %>%
                           filter(
                             industry_name == input$geofacetIndustries,
                             wb_region %in% input$geofacetRegions
                           ) %>%
                           select(country_name,
                                  country_code,
                                  net_per_10K,
                                  year)
                       }
                     }
                     else {
                       validate(need(
                         length(input$geofacetSkills) > 0,
                         "Please choose at least one skill"
                       ))
                       if (length(input$geofacetSkills) > 1)
                       {
                         req(input$geofacetYear)
                         geofacetBar <- TRUE
                         
                         migrationGeofacet <-
                           skillMigrationPivot %>%
                           filter(
                             skill_group_name %in% input$geofacetSkills,
                             wb_region %in% input$geofacetRegions,
                             year == input$geofacetYear
                           ) %>%
                           select(skill_group_name,
                                  country_name,
                                  country_code,
                                  net_per_10K) %>%
                           rename(bars = skill_group_name)
                       }
                       else {
                         migrationGeofacet <-
                           skillMigrationPivot %>%
                           filter(
                             skill_group_name == input$geofacetSkills,
                             wb_region %in% input$geofacetRegions
                           ) %>%
                           select(country_name,
                                  country_code,
                                  net_per_10K,
                                  year)
                       }
                     }
                     
                     validate(need(nrow(migrationGeofacet) > 0,
                                   "No results for selection."))
                     
                     migrationGeofacet$country_code = toupper(migrationGeofacet$country_code)
                     
                     
                     migrationGeofacetCountries <-
                       migrationGeofacet %>%
                       distinct(country_code, .keep_all = TRUE)
                     
                     geofacetMap <- worldPolygons110 %>%
                       right_join(migrationGeofacetCountries,
                                  by = c("ISO_A2" = "country_code"))
                     
                     geofacetGrid <- NULL
                     
                     while (is.null(geofacetGrid)) {
                       tryCatch(
                         geofacetGrid <- grid_auto(geofacetMap,
                                                   names = "country_name") %>%
                           distinct(name_country_name, .keep_all = TRUE),
                         error = function(e) {
                           print(e)
                           print("retrying...")
                         }
                       )
                     }
                     
                     if (geofacetBar)
                     {
                       ggplot(
                         migrationGeofacet,
                         aes(
                           bars,
                           net_per_10K,
                           fill = bars,
                           alpha = net_per_10K >= 0
                         )
                       ) +
                         geom_col() +
                         geom_hline(yintercept = 0,
                                    linetype = "dashed",
                                    color = "black") +
                         scale_alpha_discrete(range = c(0.5, 1.0)) +
                         facet_geo( ~ country_name, grid = geofacetGrid) +
                         coord_flip() +
                         theme(legend.position = "none")
                     }
                     else {
                       migrationGeofacet$year = as.numeric(migrationGeofacet$year)
                       ggplot(migrationGeofacet,
                              aes(year, net_per_10K)) +
                         geom_line() +
                         geom_point() +
                         geom_hline(yintercept = 0,
                                    linetype = "dashed",
                                    color = "black") +
                         theme_bw() +
                         theme(axis.text.x = element_blank()) +
                         facet_geo( ~ country_name, grid = geofacetGrid)
                     }
                   })
                 })
                 
                 #######################################
                 # Geofacet - End
                 #######################################
                 
               })
}
