library(tidyverse)
library(igraph)
library(tidygraph)
library(visNetwork)

# set paths to flat files ================================================================
paths <- list(attrs = "data/flat_files/CASPAttributes.csv",
              el_drivers = "data/flat_files/Driver Final Connections.csv",
              el_works_with = "data/flat_files/WorkWith Final Connections.csv",
              el_knows = "data/flat_files/Know Final Connections.csv",
              el_info = "data/flat_files/Info-Transposed Final Connections.csv")

# read flat files ========================================================================
attrs <- paths$attrs  %>%
  read_csv() %>% 
  rename_all(str_replace_all, "([a-z])([A-Z])", "\\1_\\2") %>%
  rename_all(str_replace_all, "-|\\s", "_") %>%
  rename_all(str_to_lower) %>%
  rename(name = names) %>%
  mutate_if(is.character, funs(str_replace_all(., "\\&", ""))) %>%
  mutate(color = if_else(color == "sky blue", "blue", color)) %>%
  mutate(years_in_casp = case_when(zero_years_in_casp ~ 0L,
                                   one_years_in_casp ~ 1L,
                                   two_years_in_casp ~ 2L,
                                   three_years_in_casp ~ 3L,
                                   four_years_in_casp ~ 4L,
                                   five_years_in_casp ~ 5L,
                                   six_years_in_casp ~ 6L,
                                   seven_years_in_casp ~ 7L,
                                   no_response_years_in_casp ~ NA_integer_)) %>%
  select(-contains("_years_in_casp"))
  

el_drivers <- paths$el_drivers %>%
  read_csv() %>%
  mutate(edge_type = "drivers")

el_work_with <- paths$el_works_with %>%
  read_csv() %>%
  mutate(edge_type = "works with")

el_knows <- paths$el_knows %>%
  read_csv() %>%
  mutate(edge_type = "knows")

el_info <- paths$el_knows %>%
  read_csv() %>%
  mutate(edge_type = "info")

# build combo edge list ==================================================================
combo_el <- bind_rows(el_drivers, el_work_with, el_knows, el_info)

# build graph ============================================================================
g <- graph_from_data_frame(combo_el, vertices = attrs, directed = FALSE) %>%
  simplify(remove.multiple = FALSE, remove.loops = TRUE) %>%
  as_tbl_graph() %E>%
  mutate(color = case_when(edge_type == "info" ~ "salmon",
                           edge_type == "know" ~ "lightblue",
                           edge_type == "works with" ~ "green",
                           edge_type == "drivers" ~ "yellow"))

# write graph as .rds ====================================================================
# write_rds(g, "data/CASP_net.rds")

# calculate and write coords =============================================================
# coords <- layout_with_fr(g, niter = 100000, 
#                          start.temp = 100000,
#                          weights = rep(1, length(E(g) * 0.25)))

options(viewer = NULL)
g %>%
  # filter(edge_type == "works with") %>%
  # as.igraph() %>%
  # simplify() %>%
  visIgraph(physics = TRUE, layout = "layout.norm", 
            layoutMatrix = layout_with_fr(g)) %>%
  visPhysics(repulsion = list(springlength = 100),
             maxVelocity = 1,
             solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -500)) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
  visEvents(type = "on", startStabilizing = "function() {
            this.moveTo({scale:0.0001})}")

  # visPhysics(solver = "repulsion")
  visPhysics(solver = "forceAtlas2Based", 
             # stabilization = list(enabled = TRUE, iterations = 10000),
             forceAtlas2Based = list(gravitationalConstant = -10000))

  # visIgraphLayout(layout = "layout.norm",
                  # layoutMatrix = layout_with_fr(g))

# library(networkD3)
networkD3::
