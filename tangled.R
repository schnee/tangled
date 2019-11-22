library(readr)
library(ggraph)
library(ggpubr)
library(dplyr)
library(tidygraph)
library(networkD3)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(lubridate)

source("./functions.R")
source("./top_pagerankers.R")


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

graph <- make_graph(tangled) %>% weight_graph(.50, 0.08) %>% 
  activate(nodes) %>% 
  mutate(n_tri = local_triangles()) %>%
  mutate(the_alpha = if_else(n_tri > 0, 0.75, 0))

graph %>% saveRDS(file = "./data/graph.RDS")

my_pal <- get_palette(graph)

num_nodes <- graph %>% activate(nodes) %>% as_tibble() %>% summarize(n=n()) %>% pull(n)

the_edge_types <- graph %>% activate(edges) %>% pull(d_type) %>% factor() %>% levels()

graph <- graph %>% activate(edges) %>% mutate(d_type = factor(d_type, levels=the_edge_types))

fed_ct <- graph %>% activate(nodes) %>% as_tibble %>% mutate(row_num = row_number()) %>%
  filter(name == "Federal Court") %>% pull(row_num)

local_neighborhood <-
  graph %>% convert(to_local_neighborhood, node = fed_ct, 3)

rec_target_node <- "Federal Court"
recommendations <- get_node_recommendations(graph, rec_target_node) %>% 
  rename(Node = nodes,
         Strenth = ratings)

the_rec_table_grob <- ggtexttable(recommendations, 
                              rows = NULL,
                              cols = c("Name", "Strength"),
                              theme = ttheme("mBlackWhite", base_size = 6,
                                             padding = unit(c(1.5,1.5), "mm"))
                              )

arranged <- ggarrange(the_rec_table_grob, ncol = 1, nrow = 1)

entitled <- annotate_figure(arranged, top = text_grob(paste("Best Recommendations for\n", rec_target_node)))

edge_pal <- c("#C0C0C0", "#FFA500", "#00B300", "#FF0000")

the_layout <- create_layout(graph, layout = "igraph", algorithm = "drl", options = igraph::drl_defaults$final)#, maxiter = 200*num_nodes)

# get the extents of the layout, so we can place the table
xmin <- the_layout$x %>% min()
xmax <- the_layout$x %>% max()
ymin <- the_layout$y %>% min()
ymax <- the_layout$y %>% max()

p <- ggraph( the_layout ) +
  geom_edge_fan(aes(linetype=d_type, colour = d_type,
                    label=stringr::str_wrap(note, width=20)), edge_width=.35,
                end_cap=circle(3,"mm"), strength = 3, start_cap = circle(3,"mm"),
                label_dodge = unit(2,"mm"), label_size = 2,
                arrow = arrow(type="closed", length = unit(0.05, "inches"))) +
  scale_edge_linetype_manual(guide = "none", values=c(5, rep(1, length(the_edge_types) -1))) +
  scale_edge_colour_manual(name="Relationship", values = edge_pal) +
  geom_node_point(color = "black", size = 4.5) +
  geom_node_point(aes(colour = group_label),size = 3.5) +
  geom_node_point(color = "white", size = 1)+
  geom_node_label( aes(label=name, alpha = the_alpha), size=2, repel = TRUE) +
  scale_alpha(range = c(0.1,0.75)) +
  scale_color_manual(name = "Community", values = my_pal) +
  annotation_custom(grob = ggplotGrob(entitled), xmin = 0.9 * xmax , xmax= xmax, ymin = 0.9*ymin, ymax = 0.2 * ymin) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
    title = paste0(graph %>% activate(nodes) %>% as_tibble %>% arrange(desc(centrality)) %>% pull(name) %>% first(),"'s Tangled Web"),
    caption = paste("https://schnee.github.com/tangled", now("UTC"))
  ) + guides(alpha = FALSE)

ggsave("./docs/tangled.png", plot = p, height=15, width = 20, dpi=200)

# make the ranking plot
make_ranking_plot(tangled, 16L, start = 0.02, step = 0.02)



links <- graph %>% activate(edges) %>% as_tibble() %>% 
  mutate(from = from -1, to = to -1) %>% 
  mutate(lc = if_else(e_type == "money", "red", "blue"))
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
