library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)
library(networkD3)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(lubridate)


tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

tangled <- tangled %>% mutate(note = if_else(is.na(note), "",note))

# attempt to roll up the payments
tangled <- tangled %>% filter(type %in% c("payment", "loan", "investment", "fine")) %>% 
  mutate(amt = as.numeric(note)) %>%
  group_by(from, to, type) %>%
  summarize(date = last(date),
            sum = sum(amt),
            note = if_else(is.na(sum), last(note), format(sum, scientific = F))
  ) %>% bind_rows(
    tangled %>% filter(type=="association")
  )

graph <- as_tbl_graph(tangled) %>% mutate(group = as.character(group_walktrap()))

# the below few line will find the pagerank for all nodes, and use the 
# max pagerank as the group label
g<-graph %>% 
  mutate(centrality = centrality_pagerank()) %>% activate(nodes) %>% 
  group_by(group) %>% mutate(g_max =  max(centrality))


max_cent_df <- g %>% activate(nodes) %>% as_tibble() %>% group_by(group) %>% summarize(g_max = max(centrality))

# the last summarize there handles ties
max_cent <- g %>% activate(nodes) %>% as_tibble()%>% 
  filter(centrality %in% max_cent_df$g_max)  %>% rename(group_label = name) %>% ungroup() %>%
  group_by(group) %>% summarize(group_label = first(group_label),
                                centrality = first(centrality),
                                g_max = first(g_max))

graph <- g  %>% activate(nodes) %>%
  inner_join(max_cent, by = c("group" = "group", 
                              "g_max" = "centrality")) %>% 
  select(-g_max.y)

# now handle some aesthetics
n_group <- graph %>% activate(nodes) %>% pull(group) %>% n_distinct

my_pal <- c(few_pal(palette = "Dark")(7), 
            brewer_pal(palette = "Dark2")(8),
            brewer_pal(type="qual")(8))

# ensure I have enough colors for groups
while(length(my_pal) < n_group) {
  my_pal = c(my_pal, my_pal)
}

ggraph(graph, layout = 'igraph', algorithm="nicely" ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(2,"mm"), spread = 3, start_cap = circle(2,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(guide = "none", values=c(5,1,1,1,1)) +
  scale_edge_color_brewer(name="Relationship", type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group_label),size = 4) + geom_node_point(color = "white",size = 1)+
  geom_node_label(aes(label=name), size=3, repel = TRUE, alpha=0.75) + 
  scale_color_manual(name = "Community", values = my_pal) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
    caption = paste(now("UTC"))
  )

ggsave("./docs/tangled.png", height=11, width = 22, dpi=200)

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
