library(shinydashboard)
# library(shiny)
library(shinycssloaders)
library(tidyverse)
library(igraph)
library(tidygraph)
library(visNetwork)
library(highcharter)

# vars ===================================================================================
# save to .rds once data is confirmed for performance ====
CASP_simple <- read_rds("data/CASP_net.rds") %>% 
  as.igraph() %>% 
  simplify() %>% 
  as_tbl_graph() %N>% 
  mutate(name = str_replace_all(name, "�", "'")) %E>%
  mutate(edge_type = "Any", color = "lightgray") %>% 
  as_tibble(which = "edges")

CASP_net <- read_rds("data/CASP_net.rds") %>% 
  bind_edges(CASP_simple) %E>% 
  mutate(edge_type = str_to_title(edge_type)) %>% 
  mutate(color = "#85bedb") %N>% 
  mutate(title = name) %>%
  mutate(name = str_replace_all(name, "�", "'")) %>% 
  filter(centrality_degree() > 0)

full_coords <- CASP_net %E>%
  filter(edge_type == "Any") %>% 
  as.igraph() %>% 
  layout_with_lgl(area = vcount(.)^4)

CASP_net <- CASP_net %>% 
  mutate(x = full_coords[,1], y = full_coords[,2])

# opts ===================================================================================
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# foos ===================================================================================
cust_pal <- colorRampPalette(c("#00c3ff", "#2A4C6E"))

visualize_graph <- function(g, title){
  g <- g %N>% 
    filter(centrality_degree() > 0)
    
  coords <- g %N>% 
    as_tibble() %>% 
    select(x, y) %>% 
    as.matrix()
  
  vis_g <- toVisNetworkData(as.igraph(g))
  
  vis_g <- visNetwork(vis_g$nodes, vis_g$edges, main = paste(title, "Connections")) %>% 
    visIgraphLayout(physics = FALSE, type = "full", randomSeed = 1234,
                    layout = "layout.norm", layoutMatrix = coords) %>%
    visNodes(size = 30, 
             shadow = list(enabled = TRUE, size = 10)) %>%
    visEdges(selectionWidth = 15) %>% 
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = FALSE,
                                       hover = TRUE),
               nodesIdSelection  = list(enabled = TRUE))
  # vis_g$nodes$
}

get_distances <- function(g){
  ig <- as.igraph(g)
  1:seq_along(vcount(ig)) %>% 
    map(~ distances(g, .x)) %>% 
    as_vector()
}

plot_distances <- function(g, title){
  g_distances <- g %N>%
    filter(centrality_degree() > 0) %>% 
    get_distances() %>%
    as_tibble() %>%
    filter(value != 0) %>% 
    arrange(value) %>% 
    mutate(value = paste(value, "Step(s)")) %>% 
    group_by(value) %>%
    summarise(n = n()) %>%
    mutate(color = cust_pal(nrow(.)))
  
  highchart() %>% 
    hc_xAxis(title = list(text = "Number of Steps"),
             categories = g_distances$value) %>% 
    hc_yAxis(title = list(text = "Count")) %>% 
    hc_legend(NULL) %>% 
    hc_title(text = paste0("Degrees of Separation-", title, " Connections")) %>%
    hc_add_series(g_distances, "column", name = "Count",
             hcaes(x = value, y = n, color = color)) %>%
    hc_chart(options3d = list(enabled = TRUE,
                              beta = 15, alpha = 10))
}

plot_betweenness <- function(g, title){
  g_btwn <- g %N>%
    mutate(btwn = centrality_betweenness() %>% 
             round(digits = 0)) %>% 
    as_tibble() %>% 
    arrange(desc(btwn)) %>% 
    select(short_names, name, btwn) %>% 
    head(20) %>%
    mutate(color = colors(nrow(.)))

  highchart() %>%
    hc_xAxis(title = list(text = NULL),
             labels = list(text = NULL),
             categories = g_btwn$short_names) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
    hc_legend(NULL) %>%
    hc_title(text = paste0("Liasions-", title, " Connections: Top 20")) %>%
    hc_add_series(g_btwn, "bar", name = "Liaison Score",
             hcaes(x = name, y = btwn, color = color)) %>%
    hc_chart(options3d = list(enabled = TRUE,
                              beta = 15, alpha = 10)) %>% 
    hc_tooltip(options = list(decimalPoint = ","))
}
