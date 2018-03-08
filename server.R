function(input, output, session){
  
  # on-load modal ========================================================================
  # showModal(modalDialog(title = "About this app", size = "m",
  #                       renderUI(includeMarkdown("www/about_small.md")),
  #                       easyClose = TRUE))
  
  output$out_net <- renderVisNetwork({
      CASP_net %>% 
      filter(edge_type %in% str_to_lower(input$in_edge_type)) %>%
      visualize_graph()
  })
  
  session$onSessionEnded(stopApp)
}
