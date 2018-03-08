function(input, output, session){
  
  # on-load modal ========================================================================
  # showModal(modalDialog(title = "About this app", size = "m",
  #                       renderUI(includeMarkdown("www/about_small.md")),
  #                       easyClose = TRUE))
  
  output$out_net <- renderVisNetwork({
    CASP_net %>%
      visIgraph(physics = TRUE, layout = "layout.norm",
                layoutMatrix = layout_with_fr(g)) %>%
      visPhysics(repulsion = list(springlength = 100),
                 maxVelocity = 1,
                 solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -500)) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
      visEvents(type = "on",
                startStabilizing = "function() {this.moveTo({scale:0.0001})}")
  })
  
  session$onSessionEnded(stopApp)
}
