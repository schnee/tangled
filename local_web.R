source("./functions.R")

# To call from a cmd line, do something like:
# Rscript local_web.R "Donald J Trump"
# the node name must be in quotes from the command line

args <- commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  node_name <- 'Sam Patten'
} else if (length(args)==1) {
  # default output file
  node_name <- args[1]
}

print(node_name)

tangled <- read_csv("./data/tangled.csv")
graph <- make_graph(tangled) %>%
  activate(nodes) %>% 
  mutate(n_tri = local_triangles())
my_pal <- get_palette(graph)

#node_name <- 'Felix Sater'
node_id <- graph %>% activate(nodes) %>% mutate(node_id = row_number()) %>%
  filter(name == node_name) %>% pull(node_id)

local_neighborhood <- graph %>% to_local_neighborhood(node=node_id, order=2)

local_graph <- local_neighborhood$neighborhood
the_edge_types <- local_graph %>% activate(edges) %>% pull(type) %>% factor() %>% levels()

#local_graph <- local_graph %>% filter(n_tri > 0)

the_layout <- create_layout(local_graph, layout = "auto")

ggraph(the_layout ) +
  geom_edge_fan(aes(linetype=type, color = type, label=note), edge_width=.65,
                end_cap=circle(3,"mm"), spread = 3, start_cap = circle(3,"mm"), 
                label_dodge = unit(2,"mm"), label_size = 3,
                arrow = arrow(type="closed", length = unit(1.25, "mm"))) +
  scale_edge_linetype_manual(guide = "none",values=c(5, rep(1, length(the_edge_types) -1))) +
  scale_edge_color_brewer(name="Relationship", type="qual", palette = "Dark2") +
  geom_node_point(aes(colour = group_label),size = 4) + geom_node_point(color = "white",size = 1)+
  geom_node_label(aes(label=name), size=4, repel = TRUE, alpha=0.75, show.legend = FALSE) + 
  scale_color_manual(name = "Community", values = my_pal) +
  ggthemes::theme_few() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  labs(
    title = paste0("The ", node_name, " Tangled Web"),
    caption = paste(now("UTC"),"https://schnee.github.io/tangled",sep='\n')
  )

fn <- tolower(node_name) %>% gsub('[^a-z]', '', .)

fp <- paste0("./docs/", fn, ".png")

ggsave(fp, height=8, width = 12, dpi=100)
