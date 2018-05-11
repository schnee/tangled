library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)

tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

graph <- as_tbl_graph(tangled)

ggraph(graph, layout = 'igraph', algorithm="kk" ) +
  geom_edge_fan(aes(linetype=type, color = type, label=ifelse(is.na(amount), "", amount)),
                spread = 15, arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(values=c(2,1)) +
  scale_edge_color_brewer(type="qual", palette = "Dark2") +
  geom_node_point() + geom_node_label(aes(label=name), size=3, repel = TRUE) + 
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("tangled.png", height=6, width = 10, dpi=100)
