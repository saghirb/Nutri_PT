
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
    updateSelectInput(session, "nutChoice2",
                      label = paste("Select nutritional component"),
                      choices = set_names(updatedNutrients$NutrientID, updatedNutrients$NutrientFull)
                      )
  })
  
  output$nutInterval1 <- renderUI({
    nutRng1 = choiceNutrients %>% 
      filter(NutrientID %in% input$nutChoice1) %>%
      select(min, max) %>% 
      unlist()
      
    sliderInput("nutRange1", h4("Select range"),
                min = nutRng1[1], 
                max = nutRng1[2],
                value = nutRng1)
  })
  
  output$nutInterval2 <- renderUI({
    nutRng2 = choiceNutrients %>% 
      filter(NutrientID %in% input$nutChoice2) %>%
      select(min, max) %>% 
      unlist()
    
    sliderInput("nutRange2", h4("Select range"),
                min = nutRng2[1], 
                max = nutRng2[2],
                value = nutRng2)
  })
  
  output$NutriRanges <- DT::renderDataTable(DT::datatable({
    
      compFood <- nutriPT %>% 
        select(foodID, foodItem, NutrientID, NutrientCode, Value, Unit, Quantity) %>% 
        filter(NutrientID %in% c(input$nutChoice1, input$nutChoice2)) %>% 
        filter((input$nutChoice1 %in% NutrientID & between(Value,  input$nutRange1[1], input$nutRange1[2])) & 
               (input$nutChoice2 %in% NutrientID & between(Value,  input$nutRange2[1], input$nutRange2[2]))) %>%
        group_by(foodID) %>% 
        filter(n() == 2) %>% 
        ungroup() %>% 
        select(-foodID, -NutrientID) %>%
        spread(NutrientCode, Value)
      
      if(nrow(compFood) == 0){
        return("No data in the selected ranges.")
      } else {
        compFood
      }
    
  }))
  
  
  
})
