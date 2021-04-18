##########################################################################
# This file contains the code for the regression module
##########################################################################

##########################################################################
# Function to run for preload
# Insert any code that will be called before Shiny App is loaded specific to this module e.g. Data Wrangling
# Data has already been loaded in dataload.R

##########################################################################
# End of pre load code
##########################################################################

# Function for UI
# Make sure to wrap all input and output ids with the ns() function
regressionUI <- function(id = "regression") {
  ns <- NS(id)
  tagList(
    # Add your UI here
    
    
    
    # End UI here
  )
}

regressionServer <- function(id = "regression") {
  moduleServer(id,
               function(input, output, session) {
                 # Server code should start from there
                 
                 
                 
                 # Server code should end above this line
               })
}