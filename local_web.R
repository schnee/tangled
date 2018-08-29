source("./functions.R")


tangled <- read_csv("./data/tangled.csv")
graph <- make_graph(tangled)
my_pal <- get_palette(graph)


node_name <- 'AMI'
node_id <- graph %>% activate(nodes) %>% mutate(node_id = row_number()) %>%
  filter(name == node_name) %>% pull(node_id)

local_graph <- graph %>% to_local_neighborhood(node=node_id, order=2)


ggraph(local_graph$neighborhood, layout = "auto" ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(guide = "none", values=c(5,1,1,1,1)) +
  scale_edge_color_brewer(name="Relationship", type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group_label),size = 6) + geom_node_point(color = "white",size = 2)+
  geom_node_label(aes(label=name), size=4, repel = TRUE, alpha=0.75, show.legend = FALSE) + 
  scale_color_manual(name = "Community", values = my_pal) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
    caption = paste(node_name,now("UTC"),sep='\n')
  )

ggsave("./docs/AMI.png", height=8, width = 12, dpi=100)
