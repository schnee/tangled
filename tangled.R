library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)
library(networkD3)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(lubridate)

source("./functions.R")



tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

# check the version of the data found on disk and compare it to the
# docs.google.com version if they are identical, then there's nothing to do, so
# bail out. If they are identical and maybe you feel that they should not be, it
# could be because the Google CDN hasn't updated.
#
# you can skip over this code in interactive mode
old_state_fn <- "./data/tangled.csv"
if(file.exists(old_state_fn)){
  old_state <- read_csv(old_state_fn)
  if(identical(tangled, old_state)){
    # nothing to do; bail out
    stop("Old state == current state, bailing out")
  }
}

# continue on, update the old state.
tangled %>% write_csv(old_state_fn)

graph <- make_graph(tangled) %>% weight_graph(.4, 0.05) %>% 
  activate(nodes) %>% 
  mutate(n_tri = local_triangles())

my_pal <- get_palette(graph)

num_nodes <- graph %>% activate(nodes) %>% as_tibble() %>% summarize(n=n()) %>% pull(n)
#the_layout <- create_layout(graph, layout = "igraph", algorithm="lgl", maxiter = 200*num_nodes)

the_layout <- create_layout(graph, layout = "igraph", algorithm = "drl", options = igraph::drl_defaults$final)#, maxiter = 200*num_nodes)

the_edge_types <- graph %>% activate(edges) %>% pull(type) %>% factor() %>% levels()

ggraph(the_layout ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.5,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 2,
                arrow = arrow(type="closed", length = unit(0.05, "inches"))) +
  scale_edge_linetype_manual(guide = "none", values=c(5, rep(1, length(the_edge_types) -1))) +
  scale_edge_color_brewer(name="Relationship", type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group_label),size = 4) + geom_node_point(color = "white",size = 1)+
  geom_node_label(aes(label=name), size=2, repel = TRUE, alpha=0.75) + 
  scale_color_manual(name = "Community", values = my_pal) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
    title = paste0(graph %>% activate(nodes) %>% as_tibble %>% arrange(desc(centrality)) %>% pull(name) %>% first(),"'s Tangled Web"),
    caption = paste(now("UTC"))
  )

ggsave("./docs/tangled.png", height=15, width = 20, dpi=200)

links <- graph %>% activate(edges) %>% as_tibble() %>% 
  mutate(from = from -1, to = to -1) %>% 
  mutate(lc = if_else(type == "payment", "red", "blue"))
nodes <- graph %>% activate(nodes) %>% as_tibble() 

gd3 <- list(links = links, nodes = nodes)

forceNetwork(Links = links,
             Nodes = nodes,
             NodeID = 'name',
             Group = 'group',
             Source = 'from',
             Target = 'to',
             opacityNoHover = 1,
             opacity = 1,
             fontSize = 20,
             linkColour = links$lc,
             zoom = TRUE,
             charge = -100)  %>% 
  saveNetwork(file=paste0(normalizePath("./docs"),"/tangled-d3.html"), selfcontained = TRUE)
