library(igraph)
library(tidygraph)

source("./functions.R")

graph <- readRDS(gzcon(url("https://github.com/schnee/tangled/blob/master/data/graph.RDS?raw=true")))

edge_pal <- c("#C0C0C0", "#FFA500", "#00B300", "#FF0000")
node_pal <- get_palette(graph)

the_edge_types <- graph %>% activate(edges) %>% 
  pull(d_type) %>% factor() %>% levels()

the_clusters <- graph %>% activate(nodes) %>% 
  pull(group) %>% 
  factor() %>%
  levels()

the_cluster_lab <- graph %>% activate(nodes) %>% 
  pull(group_label) %>% factor() %>% levels()


g <- graph %>% activate(edges) %>%
  mutate(color = edge_pal[match(d_type, the_edge_types)],
         arrow.size = 1,
         width = 5) %>%
  activate(nodes) %>%
  mutate(color = node_pal[match(group_label, the_cluster_lab)],
         alpha_hex = if_else(n_tri > 0, "aa", "88"),
         label.color = if_else(n_tri > 0, "#000000FF", "#00000022"),
         color = paste0(color, alpha_hex),
         size = 3,
         frame.color = "NA",
         label.cex = 2) %>%
  weight_graph(1, 0.4)

title_txt <- paste0(graph %>% activate(nodes) %>% 
                      as_tibble %>% arrange(desc(centrality)) %>% 
                      pull(name) %>% first(),"'s Tangled Web")

png(filename = "tangled-base.png", width = 4000, height = 3000)
par(ps = 12, cex = 1, cex.main = 8)
plot(g, layout = layout_with_drl,  options = igraph::drl_defaults$final)
title(title_txt, line = -3)
legend("topright", legend = the_edge_types, 
       col = edge_pal, lty = 1, cex = 4,
       lwd = 8, title = "Relationship")
legend("bottomright", legend = the_cluster_lab, fill = node_pal, cex =4,
       title = "Group")
dev.off()
