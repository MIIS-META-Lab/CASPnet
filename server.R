function(input, output, session){
  
  # on-load modal ========================================================================
  showModal(modalDialog(title = "About", size = "l",
                        renderUI(includeMarkdown("www/about.md")),
                        easyClose = TRUE))

  net_title <- reactive({
    case_when(input$in_edge_type == "All" ~ "All",
                         input$in_edge_type == "Any" ~ "Any",
                         TRUE ~ paste0("\"", input$in_edge_type, "\""))
  })
  
  output$out_net <- renderVisNetwork({
    if(input$in_edge_type != "All"){
      CASP_net %E>%
        filter(edge_type %in% input$in_edge_type) %>%
        visualize_graph(net_title())
    } else {
      CASP_net %E>%
        filter(edge_type != "Any") %>% 
        visualize_graph(net_title())
    }
  })
  
    output$distances <- renderHighchart({
    if(input$in_edge_type != "All"){
      CASP_net %E>%
        filter(edge_type %in% input$in_edge_type) %>%
        plot_distances(net_title())
    } else {
      CASP_net %E>%
        filter(edge_type != "Any") %>% 
        plot_distances(net_title())
    }
  })
    
  output$btwn <- renderHighchart({
    if(input$in_edge_type != "All"){
      CASP_net %E>%
        filter(edge_type %in% input$in_edge_type) %>%
        plot_betweenness(net_title())
    } else {
      CASP_net %E>%
        filter(edge_type != "Any") %>% 
        plot_betweenness(net_title())
    }
  })
  
  session$onSessionEnded(stopApp)
}
