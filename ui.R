# HEADER =================================================================================
header <- dashboardHeader(
  title = "CASP Network",
  tags$li(a(href = "http://future-futuro.org/",
            target = "_blank",
            img(src = "CASP_logo_transparent.png",
                title = "Visit the CASP Home Page",
                height = "50px"),
            style = "padding-top:0; 
                     padding-bottom:0;
                     padding-right:0;
                     padding-left:0;
                     background-color:white"
            ),
          class = "dropdown"),
  tags$li(a(href = "https://www.facebook.com/future.futuro/",
            target = "_blank",
            img(src = "facebook_logo_transparent.png",
                title = "Visit CASP on Facebook",
                height = "50px",
                align= "0,0,0,0,0"),
            style = "padding-top:0px;
                     padding-bottom:0px;
                     padding-right:0;
                     padding-left:0;"
            ),
          class = "dropdown"),
  tags$li(a(href = "http://sites.miis.edu/metalab/",
            target = "_blank",
            img(src = "meta_logo.png",
                title = "Visit the META Lab Home Page",
                height = "50px"),
            style = "padding-top:0; 
                     padding-bottom:0;
                     padding-right:1;
                     padding-left:0;
                     background-color:black"
            ),
          class = "dropdown")
)

# SIDEBAR ================================================================================
sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      #* network ====
      menuItem("Network", startExpanded = TRUE,
               tabName = "network",
               icon = icon("connectdevelop", lib = "font-awesome")),
      selectInput(inputId = "in_edge_type",
                  label = "Connection Type",
                  choices = c("All", "Any", "Drivers",
                              "Works With", "Knows", "Info"),
                  selected = "All"
                         ),
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
            fluidRow(column(width = 7,
                            visNetworkOutput("out_net", height = 900) %>%
                              withSpinner()
                            ),
                     column(width = 5,
                            highchartOutput("distances", height = 400) %>% 
                              withSpinner(),
                            
                              highchartOutput("btwn", height = 500) %>% 
                                withSpinner()
                              )
                     )
            ),
    tabItem(tabName = "about",
            fluidRow(column(width = 12,
                            includeMarkdown("www/about.md")
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
