function(input, output, session){
  
  # on-load modal ========================================================================
  # showModal(modalDialog(title = "About this app", size = "m",
  #                       renderUI(includeMarkdown("www/about_small.md")),
  #                       easyClose = TRUE))
  
  session$onSessionEnded(stopApp)
}
