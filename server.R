
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
        select(-foodID, -NutrientID) %>% 
        spread(NutrientCode, Value)
      
    
  }))
  
  
  
})
