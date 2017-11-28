
shinyServer(function(input, output, session) {

  
  # Compare Nutritional Composition of Food Items ===============
  output$CompareFood <- DT::renderDataTable(DT::datatable({
    
    if(!is.null(input$iFoodID) && !is.null(input$iNutrientID)){
      compFood <- nutriPT %>% 
        select(foodID, foodItem, NutrientID, Nutrient, Value, Unit, Quantity) %>% 
        filter(foodID %in% input$iFoodID & NutrientID %in% input$iNutrientID) %>% 
        select(-foodID, -NutrientID) %>% 
        spread(foodItem, Value)
    }
    
  }))
 
  # Select Food Items Using Nutrient Ranges ===============
  observe({
    nutSelected <- input$nutChoice1
    
    updatedNutrients <- choiceNutrients %>% 
      filter(NutrientID != nutSelected) %>% 
      arrange(NutrientID)
    
    # Can also set the label and select items
    updateSelectizeInput(session, "nutChoice2",
                      label = paste("Select second nutrient and range:"),
                      choices = set_names(choiceNutrients$NutrientID, choiceNutrients$NutrientUnit),
                      selected = character(0)
    )
  })
  
  output$nutInterval1 <- renderUI({
    nutRng1 = choiceNutrients %>% 
      filter(NutrientID %in% input$nutChoice1) %>%
      select(min, max) %>% 
      unlist()
      
    sliderInput("nutRange1", NULL,
                min = nutRng1[1], 
                max = nutRng1[2],
                value = nutRng1)
  })
  
  output$nutInterval2 <- renderUI({
    if (isTruthy(input$nutChoice2)){
      nutRng2 = choiceNutrients %>% 
        filter(NutrientID %in% input$nutChoice2) %>%
        select(min, max) %>% 
        unlist()
    
    sliderInput("nutRange2", NULL,
                min = nutRng2[1], 
                max = nutRng2[2],
                value = nutRng2)
    } else {
      return(NULL)
    }
  })
  

  output$NutriRanges <- renderDataTable({
    
      compFood <- reactive({
        req(input$nutChoice1, input$nutRange1)
        
      if (isTruthy(input$nutChoice1) & !isTruthy(input$nutChoice2)) {
        nutriPT %>%
          select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>%
          filter((NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2])))
        
      } else {
        req(input$nutRange2)
        # req(input$nutChoice2, input$nutRange2)
        
        nutriPT %>%
          select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>%
          filter(NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2]) |
                   (NutrientID == input$nutChoice2 & between(Value, input$nutRange2[1], input$nutRange2[2]))) %>%
          group_by(foodID) %>%
          filter(n() == 2) %>%
          ungroup() %>%
          select(-foodID, -NutrientID)
        }
  })
      
    if(nrow(compFood()) == 0){
      compFood()
    } else {
        DT::datatable({
          compFood() %>%
            spread(NutrientUnit, Value)
        })
    }
  })
  
  # Create recipes and calculate nutritional breakdown ===============
  output$QuantitySelection <- renderUI({
    numericInput("QuantityID",
                 "Quantity for ingredient?", "",
                 min = 0.1, max = 1000, width = '50%')
    })

  input_current <- reactiveValues(ingredients = NULL, nutrients = defRecipeNutrients)
  
  # Adding ingredients
  observeEvent({input$addIngredient}, {
    req(input$addIngredient)
               
    input_prev <- session$userData$saveIng
    input_cur <- tibble(foodID = as.numeric(input$ingredientID), 
                        Portion = as.numeric(input$QuantityID))

    input_cur <- bind_rows(input_prev, input_cur) %>% 
      distinct(foodID, Portion)
    
    session$userData$saveIng <- input_cur
    input_current$ingredients <- input_cur
  })
  
  # Nutrient updates
  observeEvent({input$updateNutrients}, {
    if(!isTruthy(input$selectedNutrients)) {
      input_current$nutrients <- defRecipeNutrients
    } else {
      input_current$nutrients <- input$selectedNutrients
    }
    })
  
  nutri_recipe <- reactive({
    if(!isTruthy(input_current$ingredients)) {
      return(NULL)
    } else {
    
    recipe <- nutriPT %>%
      right_join(input_current$ingredients, by = "foodID") %>%
      filter(NutrientID %in% input_current$nutrients) %>%
      mutate(Quantity = Portion,
             Value = (Portion * Value)/100) %>%
      select(foodID, foodItem, Quantity, NutrientCodeUnit, Value)
    
    if (nrow(recipe) >0){
      total <- recipe %>% 
        filter(!(foodID == totalFoodID)) %>% 
        group_by(NutrientCodeUnit) %>% 
        summarise(sum = sum(Value)) %>% 
        ungroup() %>% 
        mutate(foodItem = "Recipe Total",
               foodID = totalFoodID) %>% 
        rename(Value = sum)
    
    recipeWide <- recipe %>%
      bind_rows(total) %>% 
      # select(-foodID) %>% 
      spread(NutrientCodeUnit, Value)

    return(recipeWide)
    } else {
      return(NULL)
    }
    }
  })
  
  # Remove ingredients
  observeEvent(input$removeRows, {

    input_prev <- session$userData$saveIng
    nutri_cur <- nutri_recipe()

    input_cur <- nutri_cur %>%
      filter(!(row_number() %in% input$RecipeTable_rows_selected)) %>% 
      rename(Portion = Quantity) %>% 
      distinct(foodID, Portion) 
    
    session$userData$saveIng <- input_cur
    input_current$ingredients <- input_cur
  })
  
  # Only display the "Delete Rows" button if there is data to be deleted.
  output$removeRowsUI <- renderUI({
    if (isTruthy(nrow(nutri_recipe()))){
      actionButton("removeRows", "Delete Rows", icon("erase", lib = "glyphicon"))
      } else {
        return(NULL)
      }
  })

  output$RecipeTable <- DT::renderDataTable({
    d <- nutri_recipe()
    DT::datatable(d[, colnames(d)!="foodID"], options = list(orderClasses = TRUE))
    })

  # When the browser (tab) is closed end the session
  session$onSessionEnded(stopApp) 
  
})

