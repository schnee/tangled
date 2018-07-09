# prototyping on how to "center" the top of a list and surround it by nearest
# descencents fanning out

df <- tibble(
  index = c(1,2,3,4,5,6),
  text = c("one", "two", "three", "four", "five", "six")
)

arrange_centered <- function(df) {
  odd_half <- df %>% filter((row_number() + 1)  %% 2 == 0)
  even_half <- df %>% filter(row_number()  %% 2 == 0) %>% arrange(-row_number())
  
  bind_rows(even_half, odd_half)
}
df
df %>% arrange_centered()

# determine the "centered" order of the groups based on their max centrality
# score
group_order <- max_cent %>% arrange(desc(centrality)) %>% arrange_centered()

# extract all the nodes, and factorize the "group", ordering by the centered
# order
node_df <- g %>% activate(nodes) %>% as_tibble()
node_df$group <- factor(x = node_df$group,levels = group_order$group )

# group the nodes by group, and arrange the /grouped/ centralities. THEN create
# a bunch of centered group data frames, which will be bind_rowed coming up
centered_groups <- node_df %>% group_by(group) %>% arrange(desc(centrality), .by_group=TRUE) %>% do(
  centered_group = arrange_centered(.)
)

centered_groups[2,]$centered_group

# this has all the nodes in a by-group centering, with the most central group in
# the middle, and the most central node in the middle of that. This should form
# the basis of the bump chart
the_ordered_nodes <- bind_rows(centered_groups$centered_group) %>% mutate(the_centered_order = row_number())

g2<-g %>% activate(nodes) %>% inner_join(the_ordered_nodes)

ggraph(g2, layout="linear", sort.by = "centrality") + geom_edge_arc(aes(colour=type), arrow = arrow(type="closed", length = unit(0.5, "mm")))

