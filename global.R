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


# Setting a default nutrient choice based on filtering in case there are updates to the Nutrients
# in the original data, i.e., this prevents hard coding a NutrientID.
# Default for Nutrient Ranges
defRangeNutrient <-  choiceNutrients %>% 
  filter(str_detect(Nutrient, "ENERCC")) %>% 
  select(NutrientID) %>% 
  as_vector()

# Default for Recipes
defRecipeNutrients <-  choiceNutrients %>% 
  filter(str_detect(Nutrient, "ENERCC|\\(CHO\\)|PROT|\\(FAT\\)")) %>% 
  select(NutrientID) %>% 
  as_vector()

# total foodID - used to add a total for nutritients at the end
totalFoodID <- (max(choiceFoods$foodID) * 200)

  
# Run shiny App
# shinyApp(
#   ui = dashboardPage(header, sidebar, body),
#   server = function(input, output) { }
# )
