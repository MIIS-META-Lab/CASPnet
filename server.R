function(input, output, session){
  # on-load modal ========================================================================
  showModal(modalDialog(title = "About", size = "l",
                        renderUI(includeMarkdown("www/about.md")),
                        easyClose = TRUE))

  # filter network, prep title ===========================================================
  net_selected <- reactive({
    if(input$in_edge_type != "All"){
      g <- CASP_net %E>% filter(edge_type %in% input$in_edge_type)
    } else g <- CASP_net %E>% filter(edge_type != "Any")
    
    title <- case_when(input$in_edge_type == "All" ~ "All",
                       input$in_edge_type == "Any" ~ "Any",
                       TRUE ~ paste0("\"", input$in_edge_type, "\""))
    
    list(g = g, title = title)
  })
  
  # sociogram  ===========================================================================
  output$out_net <- renderVisNetwork({
    visualize_graph(net_selected()$g, net_selected()$title)
  })

  # path distance histogram ==============================================================
  output$distances <- renderHighchart({
    plot_distances(net_selected()$g, net_selected()$title)
  })

  # liaison/betweenness barchart =========================================================
  output$btwn <- renderHighchart({
    plot_betweenness(net_selected()$g, net_selected()$title)
  })
  
  session$onSessionEnded(stopApp)
}
