

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
               box("Select one or more food items to compare",
                   selectizeInput("ifoodItem", "",
                                  choices = unique(nutri_tidy$foodItem),
                                  multiple = TRUE, selected = NULL)
               ),
               box("Select one or more nutrient",
                   selectizeInput("iNutrient", "",
                                  choices = unique(nutri_tidy$Nutrient),
                                  multiple = TRUE, selected = NULL)
               ),
               mainPanel(
                 br(), br(),
                 h1("Comparison table here", align = "center"),
                 br(), br()
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
             mainPanel(
               p("http://portfir.insa.pt/")
               ))
    )
  )
)



# Shiny User Interface
shinyUI(dashboardPage(header, sidebar, body))