
shinyServer(function(input, output, session) {

  
  
  output$CompareFood <- DT::renderDataTable(DT::datatable({
    
    if(!is.null(input$iFoodID) && !is.null(input$iNutrientID)){
      compFood <- nutriPT %>% 
        select(foodID, foodItem, NutrientID, Nutrient, Value, Unit, Quantity) %>% 
        filter(foodID %in% input$iFoodID & NutrientID %in% input$iNutrientID) %>% 
        select(-foodID, -NutrientID) %>% 
        spread(foodItem, Value)
    }
    
  }))
  
})
