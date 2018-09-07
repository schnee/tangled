library(plotly)

source('./functions.R')

# read in the graph, get the palette
tangled <- read_csv("./data/tangled.csv")
graph <- make_graph(tangled) %>% weight_graph(.5, 0.05) %>% 
  activate(nodes) %>% 
  mutate(n_tri = local_triangles())
my_pal <- get_palette(graph)

# initialize a newtork with the pallete
tangled_web <- plot_ly(type = "scatter",
                   colors = my_pal)

# create a layout
the_layout <- create_layout(graph, layout = "igraph", algorithm = "drl", options = igraph::drl_defaults$final)

# get the edges
edges <- graph %>% activate(edges) %>% as_tibble()

n_edges <- edges %>% nrow

# this loop does a couple of things, sadly
# rip through the edges and creates a list of line
# segments for each edge. Since I want to put annotations on the edges, I need to 
# synthesize a node in the middle of each edge. The Annotation goes on the synthetic node.

#edge_shapes <- list()
edge_annotation <- list()
for (i in 1:n_edges) {
  v0 <- edges[i, ]$from
  v1 <- edges[i, ]$to
  
  edge_shape = list(
    type = "line",
    hover = edges[i, ]$note,
    x0 = the_layout[v0, ]$x,
    y0 = the_layout[v0, ]$y,
    x1 = the_layout[v1, ]$x,
    y1 = the_layout[v1, ]$y,
    link_type = edges[i, ]$type
  )
  
#  edge_shapes[[i]] <- edge_shape

  # Add the edges 
  tangled_web <- tangled_web %>%
    add_trace(
      x = c(edge_shape$x0, edge_shape$x1),
      y = c(edge_shape$y0, edge_shape$y1),
      mode = "lines",
      color = I("#aaaaaa"),
      #text = edge_shape$hover,
      #hoverinfo = "text",
      showlegend = FALSE
    )
  
  # build the annotation. Some annotations are linkable, some are not
  the_text = if_else(
    is.na(edges[i, ]$source),
    edges[i, ]$note,
    paste0("<a href='", edges[i, ]$source, "'>", edges[i, ]$note, '</a>')
  )
  
  edge_note = list(x = mean(c(edge_shape$x0, edge_shape$x1)),
                   y = mean(c(edge_shape$y0, edge_shape$y1)),
                   text = the_text)
  
  edge_annotation[[i]] <- edge_note
}

# get all the annotations as a DF
edge_notes <- edge_annotation %>% purrr::map_df(`[`)

tangled_web <- tangled_web %>%
  #add the annotations
  add_annotations(
    data = edge_notes,
    x = ~ x,
    y = ~ y,
    text = ~ text,
    showarrow = FALSE,
    showlegend = FALSE
  )  %>%
  # add all the nodes with labels
  add_trace(
    data = the_layout,
    x = ~ x,
    y = ~ y,
    mode = "markers+text",
    color = ~ group_label,
    name = ~ group_label,
    text = ~ name,
    textposition = 'middle right',
    textfont = list(size = 15),
    hoverinfo = "text",
    marker = list(symbols = "circle-dot", size = 10)
  )


axis <-
  list(
    title = "",
    showgrid = FALSE,
    showticklabels = FALSE,
    zeroline = FALSE
  )

p <- layout(title = "The Tangled Web",
            tangled_web,
            #  shapes = edge_shapes,
            xaxis = axis,
            yaxis = axis)

p
