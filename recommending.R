library(recommenderlab)

# get the adjacency matrix
adj <- igraph::as_adj(graph)
# and treat it as a rating matrix
rrm_adj <- as(adj, "realRatingMatrix")
# but only have a 1 if there's a link and a zero if not (ignore multiple links)
b_adj <- binarize(rrm_adj, minRating=1)
# build a recommender
r <- Recommender(b_adj, method = "ALS")
recom <- predict(r, b_adj, n=50)
as(recom, "list")$`Federal Court`
top30 <- as(bestN(recom, n=30), "list")$`Federal Court`

recommenderRegistry$get_entry_names()

p <- ggraph(graph ) +
  geom_edge_fan(aes(edge_linetype=e_type, edge_color = e_type, 
                    label=note), edge_width=.5,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 2,
                arrow = arrow(type="closed", length = unit(0.05, "inches"))) +
  annotate("text", x=25, y=-20, label = paste(knitr::kable(top30, colnames = c("Most likely to\nbe friends with the Court")), collapse='\n'),
           family="Courier", size =0.5)
p
