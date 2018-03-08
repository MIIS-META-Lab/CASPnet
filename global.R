library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(igraph)
library(tidygraph)
library(visNetwork)

# load graph =============================================================================
CASP_net <- read_rds("data/CASP_net.rds")
