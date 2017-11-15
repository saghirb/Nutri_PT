
shinyServer(function(input, output, session) {

  
  
  output$CompareFood <- DT::renderDataTable(DT::datatable({
    
    if(!is.null(input$iFoodID) && !is.null(input$iNutrientID)){
      compFood <- nutriPT %>% 
          select(foodID, NutrientID, Value, Unit, Quantity) %>% 
          # filter(as.numeric(foodID) %in% input$iFoodID &&  as.numeric(NutrientID) %in% input$iNutrientID)
          filter((foodID %in% input$iFoodID)) # && (NutrientID %in% input$iNutrientID))
        
      
    # } else if (!is.null(input$ifoodID) && !is.null(input$iNutrientID)) {
    #   nutri_compare() %>%
    #     filter(str_detect(Food, str_c(str_match(input$inputID, "^[\\w-\\w+\\s\\[\\w\\]]+"),
    #                                   collapse = "|")))
    #   
    # } else if (!is.null(input$nutrientID) && is.null(input$inputID)){
    #   nutri_compare()
      
    } else {
      NULL
    }
   
  }))
})
