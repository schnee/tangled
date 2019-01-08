library(ggraph)
library(tidygraph)
library(stringr)
library(dplyr)

g <- readRDS(gzcon(url("https://github.com/schnee/tangled/blob/master/data/graph.RDS?raw=true")))

fed_ct <- g %>% activate(nodes) %>% as_tibble %>% mutate(row_num = row_number()) %>%
  filter(name == "Federal Court") %>% pull(row_num)

l_g <-
  g %>% convert(to_local_neighborhood, node = fed_ct, 3)

p <- ggraph( l_g ) +
  geom_edge_fan(aes(linetype=d_type, colour = d_type,
                    label=str_wrap(note, width=20)), edge_width=.35,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 2,
                arrow = arrow(type="closed", length = unit(0.05, "inches"))) +
  scale_edge_colour_manual(name="Relationship", values = c("#60BD68", "#F15854", "#FAA43A")) +
  geom_node_point(size = 3.5) +
  geom_node_label( aes(label=name, alpha = the_alpha), size=2, repel = TRUE) +
  scale_alpha(range = c(0.1,0.75))+ 
  guides(alpha = FALSE)

ggsave("./docs/prob.png", plot = p, height=15, width = 20, dpi=200)
