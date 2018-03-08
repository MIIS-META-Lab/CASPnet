# HEADER =================================================================================
header <- dashboardHeader(title = "CASP Network",
                          tags$li(a(href = "https://www.facebook.com/future.futuro/",
                                    target = "_blank",
                                    img(src = "facebook_logo_transparent.png",
                                        title = "Visit CASP on Facebook",
                                        height = "50px",
                                        align= "0,0,0,0,0"),
                                    style = "padding-top:0px;
                                             padding-bottom:0px;"
                                    ),
                                  class = "dropdown"),
                          tags$li(a(href = "http://future-futuro.org/",
                                    target = "_blank",
                                    img(src = "CASP_logo_transparent.png",
                                        title = "Visit the CASP Home Page",
                                        height = "50px"),
                                    style = "padding-top:0; 
                                             padding-bottom:0;
                                             background-color:white"
                                    ),
                                  class = "dropdown")
                          
)

# SIDEBAR ================================================================================
sidebar <- 
  dashboardSidebar(
    # width = 250,
    sidebarMenu(
      id = "tabs",
      #* network ====
      menuItem("Network",
               tabName = "network",
               icon = icon("connectdevelop", lib = "font-awesome")),
      #* about ====
      menuItem("About", tabName = "about",
               icon = icon("info-circle", lib="font-awesome"))
      )
)

# BODY ====
body <- dashboardBody(
  tabItems(
    #* network ====
    tabItem(tabName = "network",
            # fluidRow(column(width = 8,
            #                 radioButtons(inputId = "in_edge_type",
            #                              label = "Select Connection Type",
            #                              choices = c("Drivers",
            #                                          "Works with",
            #                                          "Knows",
            #                                          "Info")))),
            fluidRow(column(width = 3,
                            radioButtons(inputId = "in_edge_type",
                                         label = "Select Connection Type",
                                         choices = c("Drivers",
                                                     "Works with",
                                                     "Knows",
                                                     "Info"))),
                     column(width = 8,
                            visNetworkOutput("out_net", height = 700) %>%
                              withSpinner(6)
                            )
                     )
            ),
    tabItem(tabName = "about",
            fluidRow(column(width = 12
                            # ,
                            # includeMarkdown("www/about.md")
                            )
                     )
            )
    )
  )


# PAGE ====
dashboardPage(
  header,
  sidebar,
  body,
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)
