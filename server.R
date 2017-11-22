
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
    if (!is.na(as.numeric(input$nutChoice2))){
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
        req(input$nutChoice1)
        
      if (isTruthy(input$nutChoice1) & !isTruthy(input$nutChoice2)) {
        nutriPT %>%
          select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>%
          filter((NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2])))
        
      } else {
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

  input_current <- reactiveValues(ingredients = NULL)
  
  observeEvent(input$AddIngredient, {
    print('Update occured')
    input_prev <- session$userData$saveIng
    input_cur <- tibble(foodID = as.numeric(input$ingredientID), 
                            Portion = as.numeric(input$QuantityID))
    # NOTE: instead of a bind rows, should join and update the value instead. So that we dont get duplicates
    input_cur <- bind_rows(input_prev, input_cur) %>% 
      distinct(foodID, foodItem, Portion)
    
    session$userData$saveIng <- input_cur
    input_current$ingredients <- input_cur
  })
  
  nutri_filtered <- reactive({
    if(is.null(input_current$ingredients)) 
      return(NULL)
    
    nutriPT %>%
      right_join(input_current$ingredients, by = "foodID") %>%
       filter(NutrientID %in% as.numeric(isolate(input$NutrientSub))) %>%
      mutate(Quantity = Portion,
             Value = (Portion * Value)/100) %>%
      select(foodID, foodItem, Quantity, Nutrient, Unit, Value)
  })
  
  observe(cat("Filtered:" , nrow(nutri_filtered()), "\n"))
  
  nutri_sum <- reactive({
    if(is.null(nutri_filtered())) 
      return(NULL)
    
    nutri_filtered() %>%
      group_by(Nutrient, Unit) %>%
      summarise(Total = sum(Value)) %>%
      right_join(nutri_filtered(), by = c("Nutrient", "Unit")) %>%
      select(foodID, foodItem, Quantity, Nutrient, Unit, Value, Total)
  })
  
  observeEvent(input$RemoveIngredient, {
    print('Delete occured')
    input_prev <- session$userData$saveIng
    sel_rows <- input_prev[input$RecipeTable_rows_selected,]
    #input_cur <- setdiff(input_prev, sel_rows) 
    input_cur <- input_prev %>%
      distinct(foodID, foodItem, Portion) %>%
      anti_join(sel_rows)
    
    session$userData$saveIng <- input_cur
    
    input_current$ingredients <- input_cur
  })
  
  
  
  output$RecipeTable <- DT::renderDataTable({
    DT::datatable(nutri_sum(), options = list(orderClasses = TRUE))
  })

  session$onSessionEnded(stopApp) 
  
})
