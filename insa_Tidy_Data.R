library(tidyverse)
library(readxl)
library(stringr)

## ----Import INSA Food Composition Data ----
## The source data file can be downloaded from:
##   
## It has intentionally not been included in this github repository.

## Check that the data file exists
insaURL <- "https://"
insaData <- "data/insa_tca.xlsx"

if(!file.exists(insaData)){
  stop(paste("Error: Data file'", insaData, "' does not exist!\n  Download it from", insaURL))
} else {

# Read in the source data to create a tidy dataset.  
nutri_orig <- read_xlsx(insaData) 

## Given the structure of the original data the variable names batched in groups of
## three and hence need to some reconisable format to be understood.

## Creating a datset that will be used to "correct" the variables names.
ordNames <- as_tibble(list(varName = names(nutri_orig)[-c(1, 2, 3)])) %>%
  mutate(grpNtr = ceiling(row_number()/3)) %>% 
  mutate(typeVars = parse_number(varName) %% 2) %>% 
  mutate(xVars = str_detect(varName, "^X__")) %>% 
  mutate(grpType = if_else(typeVars == 1, "Quantity", "Unit")) %>% 
  mutate(grpType = if_else(xVars == FALSE, "Nutrient", grpType)) %>% 
  select(-xVars, -typeVars)
  
nutri_long <- nutri_orig %>% 
  rename(foodID = X__1, 
         foodItem = `Nome do alimento`, 
         foodGroup = Grupo) %>% 
  gather(key = "keyVars", value = "keyVals", -starts_with("food")) %>% 
  select(foodID, foodGroup, foodItem, keyVars, keyVals) %>% 
  left_join(ordNames, by= c("keyVars" = "varName")) 

# Prepare the Units and Quantity so that they can be merged to the Nutrient data.
nUnit <- nutri_long %>% 
  filter(grpType == "Unit") %>% 
  select(foodID, grpNtr, Unit = keyVals) 

head(nUnit)

nQty <-  nutri_long %>% 
  filter(grpType == "Quantity") %>% 
  select(foodID, grpNtr, Quantity = keyVals) 

# Prepare the base for the Nutrition data by keeping only the nutrient and values
nutri_tidy <- nutri_long %>% 
  filter(grpType == "Nutrient") %>% 
  rename(Value = keyVals,
         Nutrient = keyVars) %>% 
  left_join(nUnit, by = c("foodID", "grpNtr")) %>% 
  left_join(nQty, by = c("foodID", "grpNtr")) %>% 
  mutate(NutrientCode = gsub(".*\\((.*)\\).*", "\\1", Nutrient)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  select(foodID, foodGroup, foodItem, Nutrient, NutrientCode, Value, Unit, Quantity) %>% 
  arrange(foodID, Nutrient)

  glimpse(nutri_tidy)

  nutri_tidy %>% arrange(foodGroup) %>% select(foodGroup)

## ------------------------------------------------------------------------
nutri_clean <- nutri_tidy %>%
        # clean observations
        mutate(foodItem = str_replace_all(
                foodItem, "\\p{quotation mark}", ""),
               Nutrient = str_replace_all(
                       Nutrient, "\\+", ""),
               # remove square brackets to facilitate filtering detection later
               Nutrient = str_replace_all(
                       Nutrient, "\\s[\\[\\]]", "_"),
               Nutrient = str_replace_all(
                       Nutrient, "[\\]]", "")
               ) %>% 
        arrange(foodGroup, foodItem)
# DT::datatable(nutri_clean)

## ---- eval = FALSE-------------------------------------------------------
## #save(nutri_clean, file = "data/nutri_clean.RData")

## ------------------------------------------------------------------------
#load("nutri_clean.RData")

## ------------------------------------------------------------------------
nutri_wide <- nutri_clean %>%
        select(foodID, foodItem, Nutrient, Value, Unit, -Quantity) %>%
        group_by(foodItem) %>%
        select(-foodID) %>%
        spread(foodItem, Value) 

head(nutri_wide)

## ---- eval = FALSE-------------------------------------------------------
## #save(nutri_wide, file = "data/nutri_wide.RData")

## ------------------------------------------------------------------------
x <- c("Abacate", "Abóbora", "Açorda")

nutri_wide %>% select(Nutrient, str_subset(names(.), x)) %>% head()

## ------------------------------------------------------------------------
nutri_new <- nutri_clean %>%
        mutate(Quantity_unit = str_detect(Quantity, "g"),
               Quantity_unit = ifelse(Quantity_unit == TRUE, "g", "mL"),
               Quantity = str_replace_all(Quantity, "[^[\\d]]", ""),
               Value = sprintf("%6.f", Value),
               Value = as.numeric(Value),
               Quantity = as.numeric(Quantity)) %>%
        group_by(foodItem) %>%
        unite(Food, foodItem, Quantity_unit, sep = " (") %>%
        mutate(Food = str_c(Food, ")")) %>%
        select(FoodID = foodID, Food, FoodGroup = foodGroup, Quantity, Nutrient, Unit, Value) 
head(nutri_new)
        
#save(nutri_new, file = "data/nutri_new.RData", envir = .GlobalEnv)


## Close the "stop" check for data file at the start.
}