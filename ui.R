

# Dashboard Header ----------------------------------------------------
header <- dashboardHeader(title = "PT Food Composition")

# Dashboard Sidebar ---------------------------------------------------

sidebar <- dashboardSidebar(disable = TRUE)

# Dashboard Body--- ---------------------------------------------------

body <- dashboardBody(
  fluidPage(
    # Compare Nutritional Composition of Food Items ===============
    tabsetPanel(
      tabPanel("Foods", 
               h3("Compare Nutritional Composition of Food Items", align = "center"), br(),
               box(tags$b("Select one or more food items to compare"),
                   status = "primary",
                   selectizeInput("iFoodID", "",
                                  choices = set_names(choiceFoods$foodID, choiceFoods$foodItem),
                                  multiple = TRUE, 
                                  selected = NULL,
                                  options = list(placeholder = "Select Food Items"))
               ),
               box(tags$b("Select one or more nutrients"),
                   status = "primary",
                   selectizeInput("iNutrientID", "",
                                  choices = set_names(choiceNutrients$NutrientID, choiceNutrients$Nutrient),
                                  multiple = TRUE, 
                                  selected = NULL,
                                  options = list(placeholder = "Select Nutrients"))
               ),
               mainPanel(br(), 
                 DT::dataTableOutput("CompareFood")
               )
      ),
    # Select Food Items Using Nutrient Ranges ===============
    tabPanel("Nutrients",
             h3("Select Food Items Using Nutrient Ranges", align = "center"), br(),
             box(selectizeInput("nutChoice1", 
                                "Select nutritional component",
                                choices = set_names(choiceNutrients$NutrientID, choiceNutrients$NutrientFull), 
                                multiple = FALSE),
                 uiOutput("nutInterval1")
                 ),
             
             box(selectizeInput("nutChoice2",
                                "Select nutritional component",
                                choices = set_names(choiceNutrients$NutrientID, choiceNutrients$NutrientFull),
                                multiple = FALSE),
                 uiOutput("nutInterval2")
                 ),
             
             mainPanel(br(), 
                       h1("----- THIS IS NOT WORKING YET! -----", align = "center"),
                       h2(":: Intentionall broken ::", align = "center"),
                       h2(":: Need data clarification from expert ::", align = "center"),
                       br(),
                       DT::dataTableOutput("NutriRanges")
                       )
             ),
    
    # Create Recipes
    tabPanel("Recipes",
             h3("Create Recipes from Food Items", align = "center"), br()
             ),
    
    tabPanel("About",
             h3("About Nutrition Data", align = "center"), br(),
             # Create a new row for the table.
             box(p("Test About")),
             mainPanel(
               p("http://portfir.insa.pt/")
               ))
    )
  )
)



# Shiny User Interface
shinyUI(dashboardPage(header, sidebar, body))
