# We load the libraries
library(shiny) # Base Shiny library
library(shinydashboard) # Used for Shiny Dashboard
library(shinyWidgets) # Used for Shiny Widgets
library(shinythemes) # Used for Shiny Themes
library(dashboardthemes) # Used for Shiny Dashboard Themes
library(tidyverse) # Tidy universe of libraries
library(readxl) # Used to read excel files
library(leaflet) # Used to draw interactive maps
library(sf) # Load the simple feature package
library(rworldmap) # Get world map geospatial data and functions
library(CGPfunctions) # For plotting of slope graph
library(shinycssloaders) # For loading animation of plots
library(plotly)
library(ggstatsplot)
library(parameters)
library(treemap) # For plotting of treemap
library(d3treeR) # For interactive treemaps

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