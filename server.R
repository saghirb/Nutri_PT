
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
                       selected = character(0),
                      options = list(placeholder = "Select Second Nutrient")                      
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
      
      nutriPT %>% 
         select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>% 
         filter(NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2])) 
      
      
      # if (isTruthy(input$nutChoice1) & isTruthy(input$nutChoice2)) {
      #     # req(input$nutChoice1, input$nutChoice2)
      #   nutriPT %>% 
      #     select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>% 
      #     filter((NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2])) &
      #     (NutrientID ==input$nutChoice2 & between(Value,  input$nutRange2[1], input$nutRange2[2]))) %>%
      #     group_by(foodID) %>%
      #     filter(n() == 2) %>%
      #     ungroup() %>%
      #     select(-foodID, -NutrientID)
      #     } else if(isTruthy(input$nutChoice1 & !isTruthy(input$nutChoice2))){
      #       # req(input$nutChoice1)
      #       nutriPT %>% 
      #         select(foodID, foodItem, NutrientID, NutrientUnit, Value, Quantity) %>% 
      #         filter(NutrientID == input$nutChoice1 & between(Value, input$nutRange1[1], input$nutRange1[2])) 
      #     }  
      #       
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
  
  session$onSessionEnded(stopApp) 
  
})
