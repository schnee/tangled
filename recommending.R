library(recommenderlab)
library(dplyr)

graph <- readRDS(gzcon(url("https://github.com/schnee/tangled/blob/master/data/graph.RDS?raw=true")))

fed_ct <- graph %>% 
  activate(nodes) %>% 
  as_tibble %>% 
  mutate(row_num = row_number()) %>%
  filter(name == "Federal Court") %>% 
  pull(row_num)

# just chaining everything together...
b_adj <- igraph::as_adj(graph) %>% 
  as("realRatingMatrix") %>% 
  binarize(minRating=1)
topReqs <- b_adj %>%
  Recommender(method = "ALS") %>% 
  predict( b_adj[fed_ct,], n=50) %>% 
  bestN(n=30) 


topN_tib <- tibble(
  nodes = topReqs %>% 
    as("list") %>%
    unlist,
  ratings = topReqs %>%
    getRatings() %>%
    unlist
)

topN_tib
# 
# recommenderRegistry$get_entry_names()
# 
# p <- ggraph(graph ) +
#   geom_edge_fan(aes(edge_linetype=e_type, edge_color = e_type, 
#                     label=note), edge_width=.5,
#                 end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
#                 label_dodge = unit(2,"mm"), label_size = 2,
#                 arrow = arrow(type="closed", length = unit(0.05, "inches"))) +
#   annotate("text", x=25, y=-20, label = paste(knitr::kable(top30, colnames = c("Most likely to\nbe friends with the Court")), collapse='\n'),
#            family="Courier", size =0.5)
# p
