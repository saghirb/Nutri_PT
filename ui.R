

# Dashboard Header ----------------------------------------------------
header <- dashboardHeader(title = "PT Food Composition")

# Dashboard Sidebar ---------------------------------------------------

sidebar <- dashboardSidebar(disable = TRUE)

# Dashboard Body--- ---------------------------------------------------

body <- dashboardBody(
  fluidPage(
    # Compare Nutritional Composition of Food Items
    tabsetPanel(
      tabPanel("Foods", 
               h3("Compare Nutritional Composition of Food Items", align = "center"), br(),
               box(tags$b("Select one or more food items to compare"),
                   selectizeInput("iFoodID", "",
                                  choices = levels(nutriPT$foodID),
                                  multiple = TRUE, 
                                  selected = NULL,
                                  options = list(placeholder = "Select Food Items"))
               ),
               box(tags$b("Select one or more nutrients"),
                   selectizeInput("iNutrientID", "",
                                  choices = levels(nutriPT$NutrientID),
                                  multiple = TRUE, 
                                  selected = NULL,
                                  options = list(placeholder = "Select Nutrients"))
               ),
               mainPanel(br(), 
                 DT::dataTableOutput("CompareFood")
               )
      ),
    # Select Food Items Using Nutrient Ranges
    tabPanel("Nutrients",
             h3("Select Food Items Using Nutrient Ranges", align = "center"), br()
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