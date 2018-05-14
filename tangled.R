library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)

tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

tangled <- tangled %>% mutate(note = if_else(is.na(note), "",note))

graph <- as_tbl_graph(tangled)

ggraph(graph, layout = 'igraph', algorithm="fr" ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(2,"mm"), spread = 15, start_cap = circle(2,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(values=c(5,1)) +
  scale_edge_color_brewer(type="qual", palette = "Dark2") +
  geom_node_point(size = 2) + geom_node_label(aes(label=name), size=3, repel = TRUE, alpha=0.75) + 
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("tangled.png", height=9, width = 15, dpi=150)
