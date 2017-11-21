

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
             box(status = "primary",
                 selectInput("nutChoice1", 
                                "Select first nutrient and range:",
                                choices = set_names(choiceNutrients$NutrientID, choiceNutrients$NutrientUnit), 
                                selected = 15, # Hard coding to Energy (ENERCC) Kcal
                                selectize = FALSE,
                                multiple = FALSE),
                 uiOutput("nutInterval1")
                 ),
             
             box(status = "primary",
                 selectizeInput("nutChoice2",
                                "Select second nutrient and range:",
                                choices = set_names(choiceNutrients$NutrientID, choiceNutrients$NutrientUnit),
                                multiple = FALSE,
                                selected = character(0),
                                options = list(placeholder = "Select second nutrient")
                             ),
                 uiOutput("nutInterval2")
                 ),
             
             mainPanel({
                       DT::dataTableOutput("NutriRanges")
             })
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
