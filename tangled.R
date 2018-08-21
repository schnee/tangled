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
  filter(centrality %in% max_cent_df$g_max)  %>% 
  rename(group_label = name) %>% ungroup() %>%
  group_by(group) %>% arrange(g_max, desc(centrality), group_label) %>% summarize(group_label = first(group_label),
                                centrality = first(centrality),
                                g_max = first(g_max))

graph <- g  %>% activate(nodes) %>%
  inner_join(max_cent, by = c("group" = "group", 
                              "g_max" = "centrality")) %>% 
  select(-g_max.y)

# for FR layouts, let's set an edge weight: in group = 2, out of group = 1
get_group <- function(node, graph) {
  graph %>% activate(nodes) %>% as_tibble() %>% filter(row_number() == node) %>% pull(group) %>% as.numeric()
}

weights <- graph %>% activate(edges) %>% as_tibble() %>% rowwise() %>% 
  mutate(the_group = if_else(get_group(to,graph) == get_group(from,graph),get_group(from,graph),NULL)) %>%
  group_by(the_group) %>% mutate(n=n()) %>% ungroup() %>% 
  mutate(max_n = max(n), weight = if_else(!is.na(the_group), n /max_n /2 + 1, 0.5)) %>% pull(weight)

graph <- graph %>% activate(edges) %>% mutate(weight = weights)


# now handle some aesthetics
n_group <- graph %>% activate(nodes) %>% pull(group) %>% n_distinct

my_pal <- c(few_pal(palette = "Dark")(7), 
            brewer_pal(palette = "Dark2")(8),
            brewer_pal(type="qual")(8))

# ensure I have enough colors for groups
while(length(my_pal) < n_group) {
  my_pal = c(my_pal, my_pal)
}

num_nodes <- graph %>% activate(nodes) %>% as_tibble() %>% summarize(n=n()) %>% pull(n)
#the_layout <- create_layout(graph, layout = "igraph", algorithm="lgl", maxiter = 200*num_nodes)

the_layout <- create_layout(graph, layout = "igraph", algorithm = "drl", options = igraph::drl_defaults$final)#, maxiter = 200*num_nodes)

ggraph(the_layout ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(0.1, "inches"))) +
  scale_edge_linetype_manual(guide = "none", values=c(5,1,1,1,1)) +
  scale_edge_color_brewer(name="Relationship", type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group_label),size = 6) + geom_node_point(color = "white",size = 2)+
  geom_node_label(aes(label=name), size=4, repel = TRUE, alpha=0.75) + 
  scale_color_manual(name = "Community", values = my_pal) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
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
