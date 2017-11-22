# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tibble)
library(stringr)
library(DT)
library(ggplot2) # install with devtools::install_github('hadley/ggplot2') for compatibility with plotly

# Load required data
load("data/nutriPT.RData")
load("data/nutri_wide.RData")
load("data/choiceFoods.RData")
load("data/choiceNutrients.RData")


# Run shiny App
# shinyApp(
#   ui = dashboardPage(header, sidebar, body),
#   server = function(input, output) { }
# )
