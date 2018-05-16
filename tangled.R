library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)
library(networkD3)


tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

tangled <- tangled %>% mutate(note = if_else(is.na(note), "",note))

graph <- as_tbl_graph(tangled) %>% mutate(group = as.character(group_walktrap()))

ggraph(graph, layout = 'igraph', algorithm="nicely" ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(2,"mm"), spread = 15, start_cap = circle(2,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(values=c(5,1)) +
  scale_edge_color_brewer(type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group),size = 2) + geom_node_label(aes(label=name), size=3, repel = TRUE, alpha=0.75) + 
  scale_color_discrete(guide=FALSE) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("tangled.png", height=9, width = 15, dpi=150)

links <- graph %>% activate(edges) %>% as_tibble() %>% 
  mutate(from = from -1, to = to -1) %>% 
  mutate(lc = if_else(type == "payment", "red", "blue"))
nodes <- graph %>% activate(nodes) %>% as_tibble() 

gd3 <- list(links = links, nodes = nodes)

forceNetwork(Links = gd3$links,
             Nodes = gd3$nodes,
             NodeID = 'name',
             Group = 'group',
             Source = 'from',
             Target = 'to',
             opacityNoHover = 1,
             opacity = 1,
             fontSize = 20,
             linkColour = gd3$links$lc,
             charge = -100)  %>% 
  saveNetwork(file=paste0(normalizePath("./docs"),"/tangled.html"), selfcontained = TRUE)
