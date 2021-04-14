##########################################################################
# This file contains the code for the industry skill needs module
#########################################################################

# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R
industryskillPreLoad <- function() {
  
}

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
industryskillUI <- function(id = "industryskill") {
  ns <- NS(id)
  tagList(
    # Add your UI here
    
    
    
    # End UI here
  )
}

industryskillServer <- function(id = "industryskill") {
  moduleServer(id,
               function(input, output, session) {
                 # Server code should start from there
                 
                 
                 
                 # Server code should end above this line
               })
}