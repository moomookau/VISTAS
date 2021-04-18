###########################################################
# This file contains the code for the migration module
###########################################################

##########################################################################
# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R

countryMigrationPivot <- countryMigration %>%
  pivot_longer(col = starts_with("net_per_10K_"),
               names_to = "year",
               names_prefix = "net_per_10K_",
               values_to = "net_per_10K")

industryMigrationPivot <- industryMigration %>%
  pivot_longer(col = starts_with("net_per_10K_"),
               names_to = "year",
               names_prefix = "net_per_10K_",
               values_to = "net_per_10K")

skillMigrationPivot <- skillMigration %>%
  pivot_longer(col = starts_with("net_per_10K_"),
               names_to = "year",
               names_prefix = "net_per_10K_",
               values_to = "net_per_10K")

countryVector <- sort(unique(countryMigrationPivot$base_country_name))
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
      tabPanel("Flow Map",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("flowMapType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
                   selectInput(
                     inputId = ns("flowMapCountry"),
                     label = "Country:",
                     choices = countryVector
                   ),
                   sliderInput(
                     inputId = ns("flowMapYear"),
                     label = "Select year",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   )
                 ),
                 mainPanel("Flow Map",
                           leafletOutput(ns("flowMapOutput")))
               )),
      tabPanel("Choropleth",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("choroplethType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
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
                 mainPanel("Choropleth",
                           leafletOutput(ns("choroplethOutput")))
               )),
      tabPanel("Sankey Diagram",
               sidebarLayout(
                 sidebarPanel(
                   h3("Options"),
                   selectInput(
                     inputId = ns("sankeyType"),
                     label = "Type Of Migration:",
                     choices = c("Country", "Industry", "Skills"),
                     selected = "Country"
                   ),
                   selectInput(
                     inputId = ns("sankeyCountry"),
                     label = "Country:",
                     choices = countryVector
                   ),
                   sliderInput(
                     inputId = ns("sankeyYear"),
                     label = "Select year",
                     min = yearMin,
                     max = yearMax,
                     value = yearMax,
                     step = 1,
                     ticks = FALSE
                   ),
                   selectInput(
                     inputId = ns("sankeyColour"),
                     label = "Colour by:",
                     choices = c("Region", "Income"),
                     selected = "Region"
                   )
                 ),
                 mainPanel("Sankey Diagram")
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
               ))
    )
  )
}

migrationServer <- function(id = "migration") {
  moduleServer(id,
               function(input, output, session) {
                 output$test <- renderText({
                   input$choroplethDate
                 })
                 
                 # Render Flow Map
                 output$flowMapOutput <- renderLeaflet({
                   
                   # We read the filter variables from the shiny inputs
                   filterCountry <- input$flowMapCountry
                   filterYear <- input$flowMapYear
                   
                   # We filter the migration data using the variables above
                   flowMapSf <- countryMigrationPivot %>%
                     filter(base_country_name == filterCountry, year == filterYear)
                   
                   flowMapSfg <- vector(mode = "list", length = nrow(flowMapSf))
                   
                   for (j in 1:nrow(flowMapSf)) {
                     flowMapSfg[[j]] <- st_linestring(rbind(c(flowMapSf[j,]$base_long,flowMapSf[j,]$base_lat),c(flowMapSf[j,]$target_long,flowMapSf[j,]$target_lat)))
                   }
                   
                   maxMigration = max(abs(flowMapSf$net_per_10K))
                   
                   flowMapSf <- flowMapSfg %>%
                     st_sfc(crs = 4326) %>%
                     st_sf(geometry = .) %>%
                     cbind(flowMapSf) %>%
                     mutate(colour = ifelse(net_per_10K > 0, "green", "red"), value=log(abs(net_per_10K))*5) %>%
                     mutate(label = lapply(paste0("Country: ",target_country_name,"<br/>Net migration: ",net_per_10K), htmltools::HTML)) %>%
                     filter(value != 0)
                   
                   flowMapSf %>%
                     st_segmentize(units::set_units(100, km)) %>%
                     mutate(geometry = (geometry + c(180,90)) %% c(360) - c(180,90)) %>%
                     st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = TRUE) %>% 
                     leaflet() %>%
                     addTiles() %>%
                     addCircleMarkers(
                       lng = ~target_long,
                       lat = ~target_lat,
                       label = ~label,
                       color = ~colour,
                       radius = ~value,
                       stroke = FALSE,
                       fillOpacity = 0.5
                     ) %>%
                     addPolylines(label = ~label,
                                  color = ~colour,
                                  weight = ~value)
                 })
                 
                 output$choroplethOutput <- renderLeaflet({
                   # We set the variables we want to try out
                   # In this case, we filter by Singapore and 2019
                   filterCountry <- input$choroplethCountry
                   filterYear <- input$choroplethYear
                   
                   # We create the choropleth data using the variables above
                   migrationChoropleth <- countryMigrationPivot %>%
                     filter(base_country_name == filterCountry, year == filterYear) %>%
                     select(target_country_name, net_per_10K) %>%
                     mutate(label = lapply(paste0("Country: ",target_country_name,"<br/>Net migration: ",net_per_10K), htmltools::HTML))
                   
                   # We use joinCountryData2Map to join the data with the spatial data
                   migrationChoroplethMap <- joinCountryData2Map(migrationChoropleth, joinCode = "NAME", nameJoinColumn = "target_country_name") %>%
                     spatialEco::sp.na.omit(col.name = "net_per_10K")
                   
                   # We compute the max migration, rounding up to the nearest 5
                   maxMigration = plyr::round_any(max(abs(migrationChoropleth$net_per_10K)),5, f=ceiling)
                   
                   # We create the bins and a diverging palette
                   bins <- seq(-maxMigration, maxMigration, 5)
                   pal <- colorBin("RdBu", bins = bins)
                   
                   # We plot the migration choropleth map
                   migrationChoroplethMap %>%
                     leaflet() %>%
                     addTiles() %>%
                     addPolygons(fillColor = ~pal(net_per_10K),
                                 label = ~label,
                                 weight = 2,
                                 opacity = 1,
                                 color = "white",
                                 dashArray = "3",
                                 fillOpacity = 0.7) %>%
                     addLegend(pal = pal, values = ~net_per_10K, opacity = 0.7, title = NULL,
                               position = "bottomright")
                 })
                 
                 
               })
}