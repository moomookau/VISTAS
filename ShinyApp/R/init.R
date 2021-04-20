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
library(CGPfunctions)
library(plotly)

# Load of data files
industryEmploymentGrowth <- read_excel("../data/public_use-industry-employment-growth.xlsx", sheet="Growth from Industry Transition")
industrySkillsNeeds <- read_excel("../data/public_use-industry-skills-needs.xlsx", sheet="Industry Skills Needs")
skillPenetration <- read_excel("../data/public_use-skill-penetration.xlsx", sheet="Skill Penetration")
countryMigration <- read_excel("../data/public_use-talent-migration.xlsx", sheet="Country Migration")
industryMigration <- read_excel("../data/public_use-talent-migration.xlsx", sheet="Industry Migration")
skillMigration <- read_excel("../data/public_use-talent-migration.xlsx", sheet="Skill Migration")
master <- read.csv("../data/master.csv")