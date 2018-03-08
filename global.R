library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(igraph)
library(tidygraph)
library(visNetwork)

CASP_net <- read_rds("data/CASP_net.rds")

coords <- read_rds("data/coords.rds")

dropdown_values <- CASP_net %>%
  activate(nodes) %>%
  distinct(name) %>%
  arrange(name) %>%
  pull(name)

visualize_graph <- function(g){
  g %>%
    visIgraph(physics = FALSE, type = "full", 
              layout = "layout.norm", 
              layoutMatrix = coords) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
               nodesIdSelection = list(enabled = TRUE, values = dropdown_values)) 
}
