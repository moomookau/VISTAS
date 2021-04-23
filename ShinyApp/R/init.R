# We load the libraries
library(shiny) # Base Shiny library
library(shinydashboard) # Used for Shiny Dashboard
library(dashboardthemes) # Used for Shiny Dashboard Themes
library(shinyWidgets) # Used for advanced Shiny Widgets
library(shinyjs) # Used for jQuery manipulation of dom objects
library(tidyverse) # Tidy universe of libraries
library(readxl) # Used to read excel files
library(leaflet) # Used to draw interactive maps
library(rworldmap) # Get world map geospatial data and functions
####################################################################################################################
library(CGPfunctions) # For plotting of slope graph
# Note: We used a forked version @ github.com/moomookau/CGPfunctions
# The forked version allows customisation to colour by another column of the dataframe
# To Install - devtools::install_github("moomookau/CGPfunctions")
####################################################################################################################
library(shinycssloaders) # For loading animation of plots
library(plotly) # Interactive scatter plot
library(ggExtra) # Marginal distribution on regression plot
library(ggpubr) # Regression formula
library(olsrr) # Regression results
library(parameters) # Regression results
library(ggstatsplot) # Correlation matrix analysis
library(treemap) # For plotting of treemap
####################################################################################################################
library(d3treeR) # For interactive treemaps
# Note: We use a forked version of d3treeR @ github.com/moomookau/d3treeR
# The forked version renames the d3 object to allow compatibility with other packages which use a diff version of d3
# To Install - devtools::install_github("moomookau/d3treeR")
####################################################################################################################
library(chorddiag) # For plotting of chord diagrams
library(ggallin) # For pseudolog transformation
library(geofacet) # For geofacet map
library(shinyBS) # For popover tooltips

# Load of data files
industryEmploymentGrowth <-
  read_excel("data/public_use-industry-employment-growth.xlsx", sheet = "Growth from Industry Transition")
industrySkillsNeeds <-
  read_excel("data/public_use-industry-skills-needs.xlsx", sheet = "Industry Skills Needs")
skillPenetration <-
  read_excel("data/public_use-skill-penetration.xlsx", sheet = "Skill Penetration")
countryMigration <-
  read_excel("data/public_use-talent-migration.xlsx", sheet = "Country Migration")
industryMigration <-
  read_excel("data/public_use-talent-migration.xlsx", sheet = "Industry Migration")
skillMigration <-
  read_excel("data/public_use-talent-migration.xlsx", sheet = "Skill Migration")
popAndGdpData <-
  read_excel("data/Population-GDPPerCapita-2015-2019.xlsx")
master <- read.csv("data/master.csv")

# We change Taiwan and Gaza to be recognisable by RWorldMap
industryEmploymentGrowth <- industryEmploymentGrowth %>%
  mutate(country_name = str_replace(country_name, "Taiwan, China", "Taiwan")) %>%
  mutate(country_name = str_replace(country_name, "West Bank and Gaza", "West Bank"))
countryMigration <- countryMigration %>%
  mutate(base_country_name = str_replace(base_country_name, "Taiwan, China", "Taiwan")) %>%
  mutate(base_country_name = str_replace(base_country_name, "West Bank and Gaza", "West Bank")) %>%
  mutate(target_country_name = str_replace(target_country_name, "Taiwan, China", "Taiwan")) %>%
  mutate(target_country_name = str_replace(target_country_name, "West Bank and Gaza", "West Bank"))
industryMigration <- industryMigration %>%
  mutate(country_name = str_replace(country_name, "Taiwan, China", "Taiwan")) %>%
  mutate(country_name = str_replace(country_name, "West Bank and Gaza", "West Bank"))
skillMigration <- skillMigration %>%
  mutate(country_name = str_replace(country_name, "Taiwan, China", "Taiwan")) %>%
  mutate(country_name = str_replace(country_name, "West Bank and Gaza", "West Bank"))
popAndGdpData <- popAndGdpData %>%
  mutate(`Country Name` = str_replace(`Country Name`, "Taiwan, China", "Taiwan")) %>%
  mutate(`Country Name` = str_replace(`Country Name`, "West Bank and Gaza", "West Bank"))

# Pivoting of data files
industryEmploymentGrowthPivot <- industryEmploymentGrowth %>%
  pivot_longer(
    col = starts_with("growth_rate_"),
    names_to = "year",
    names_prefix = "growth_rate_",
    values_to = "growth_rate"
  )

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

populationDf <- popAndGdpData %>%
  na_if("..") %>%
  filter(`Series Code` == "SP.POP.TOTL") %>%
  pivot_longer(col = starts_with("201"),
               names_to = "year_text",
               values_to = "value") %>%
  mutate(year = substr(year_text, 1, 4)) %>%
  select(`Country Name`, year, value) %>%
  mutate(value = as.numeric(value))

gdpPerCapitaDf <- popAndGdpData %>%
  na_if("..") %>%
  filter(`Series Code` == "NY.GDP.PCAP.KD") %>%
  pivot_longer(col = starts_with("201"),
               names_to = "year_text",
               values_to = "value") %>%
  mutate(year = substr(year_text, 1, 4)) %>%
  select(`Country Name`, year, value) %>%
  mutate(value = as.numeric(value))

countriesUnique <- countryMigrationPivot %>%
  select(base_country_name, base_country_wb_region) %>%
  unique()

countriesGrouped <-
  split(countriesUnique$base_country_name,
        countriesUnique$base_country_wb_region)

regionsUnique <-
  sort(unique(countryMigrationPivot$base_country_wb_region))

industriesUnique <- industryMigrationPivot %>%
  select(isic_section_name, industry_name) %>%
  unique()

industriesGrouped <-
  split(industriesUnique$industry_name,
        industriesUnique$isic_section_name)

skillsUnique <- skillMigrationPivot %>%
  select(skill_group_name, skill_group_category) %>%
  unique()

skillsGrouped <-
  split(skillsUnique$skill_group_name,
        skillsUnique$skill_group_category)