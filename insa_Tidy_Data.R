library(tidyverse)
library(readxl)
library(stringr)

# Import and tidy the INSA Food Composition Data --------------------------------------------

## The source data file can be downloaded from: http://portfir.insa.pt/
##     >> Composição de alimentos >> Pesquisa >> Download da TCA

## Due to copyright and rights issues it has intentionally not been included in this github 
## repository. To use this program please down the file "insa_tca.xlsx" and save it in a
## subdirectory "data" in the working directory.

# If "data/insa_tca.xlsx" does not exist the program will not work so check for it first.
insaURL <- "http://portfir.insa.pt/"
insaData <- "data/insa_tca.xlsx"

if(!file.exists(insaData)){
  stop(paste("Error: Data file'", insaData, "' does not exist!\n  Download it from", insaURL))
} else {

nutri_orig <- read_xlsx(insaData) 

# Correcting Variable Names ------------------------------------------------------------------

## The original data file groups nutrients into three columns (value, unit and quantity)
## where the three column name is the "Nutrient (nutrient code)".
##  - The first of the three columns is named as the "Nutrient (nutrient code)".
##  - R automatically names some columns with the prefix "X__" followed by a number (e.g. 
##    X__4, X__7, etc).
##  - For variables named "X__??": when ?? is even the variable is "Unit" and when it is 
##    odd the variable is "Quantity".

## Creating a dataset that will be used to "correct" the variables names.
ordNames <- as_tibble(list(varName = names(nutri_orig)[-c(1, 2, 3)])) %>%
  mutate(grpNtr = ceiling(row_number()/3)) %>% 
  mutate(typeVars = parse_number(varName) %% 2) %>% 
  mutate(xVars = str_detect(varName, "^X__")) %>% 
  mutate(grpType = if_else(typeVars == 1, "Quantity", "Unit")) %>% 
  mutate(grpType = if_else(xVars == FALSE, "Nutrient", grpType)) %>% 
  select(-xVars, -typeVars)

## For the name correction to work the original dataset has to be converted to long format
## based on the groups of three columns per nutrient.
nutri_long <- nutri_orig %>% 
  rename(foodID = X__1, 
         foodItem = `Nome do alimento`, 
         foodGroup = Grupo) %>% 
  gather(key = "keyVars", value = "keyVals", -starts_with("food")) %>% 
  select(foodID, foodGroup, foodItem, keyVars, keyVals) %>% 
  left_join(ordNames, by= c("keyVars" = "varName")) 

# Create clean and tidy dataset --------------------------------------------------------------

## Prepare the Units and Quantity so that they can be merged to the Nutrient data as using
## tidyr::spread will not give the result that we need for a tidy dataset.
nUnit <- nutri_long %>% 
  filter(grpType == "Unit") %>% 
  select(foodID, grpNtr, Unit = keyVals) 

nQty <-  nutri_long %>% 
  filter(grpType == "Quantity") %>% 
  select(foodID, grpNtr, Quantity = keyVals) 

nutri_tidy <- nutri_long %>% 
  filter(grpType == "Nutrient") %>% 
  rename(Value = keyVals,
         Nutrient = keyVars) %>% 
  left_join(nUnit, by = c("foodID", "grpNtr")) %>% 
  left_join(nQty, by = c("foodID", "grpNtr")) %>% 
  mutate(NutrientCode = gsub(".*\\((.*)\\).*", "\\1", Nutrient)) %>%
  ### To make filtering easier later, remove the square brackets in Nutrient 
  ### Remove the "+" in "Mono+dissacáridos (SUGAR)"  -- DATA MISTAKE
  mutate(Nutrient = str_replace_all(Nutrient, "\\s[\\[\\]]", "_"),
         Nutrient = str_replace_all(Nutrient, "[\\]]", ""),
         Nutrient = str_replace_all(Nutrient, "\\+", "")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(foodID, foodGroup, foodItem, Nutrient, NutrientCode, Value, Unit, Quantity) %>% 
  arrange(foodID, Nutrient)


# Create a wide dataset with Nutrients in rows -----------------------------------------
## Useful for the Shiny App later
nutri_wide <- nutri_tidy %>%
  select(foodID, foodItem, Nutrient, Value, Unit, -Quantity) %>%
  group_by(foodItem) %>%
  select(-foodID) %>%
  spread(foodItem, Value) 


# MIGHT NEED THIS CODE AGAIN -----------------------------------------------------------
# nutri_new <- nutri_tidy %>%
#         mutate(Quantity_unit = str_detect(Quantity, "g"),
#                Quantity_unit = ifelse(Quantity_unit == TRUE, "g", "mL"),
#                Quantity = str_replace_all(Quantity, "[^[\\d]]", ""),
#                Value = sprintf("%6.f", Value),
#                Value = as.numeric(Value),
#                Quantity = as.numeric(Quantity)) %>%
#         group_by(foodItem) %>%
#         unite(Food, foodItem, Quantity_unit, sep = " (") %>%
#         mutate(Food = str_c(Food, ")")) %>%
#         select(FoodID = foodID, Food, FoodGroup = foodGroup, Quantity, Nutrient, Unit, Value) 
# head(nutri_new)
#         

## Saving R datasets ------------------------------------------------------------------
save(nutri_wide, file = "data/nutri_wide.RData")
save(nutri_tidy, file = "data/nutri_tidy.RData")

rm(nQty, nUnit, nutri_long, ordNames, insaData, insaURL)
## Close the "stop" check for data file at the start.
}
