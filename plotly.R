library(plotly)

network <- plot_ly(type = "scatter",
                   colors = my_pal)

g <- attributes(the_layout)$graph

edges <- g %>% activate(edges) %>% as_tibble()
n_edges <- edges %>% nrow

edge_shapes <- list()
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
  
  edge_shapes[[i]] <- edge_shape
  
  network <- network %>%
    add_trace(
      x = c(edge_shape$x0, edge_shape$x1),
      y = c(edge_shape$y0, edge_shape$y1),
      mode = "lines",
      color = I("#aaaaaa"),
      #text = edge_shape$hover,
      #hoverinfo = "text",
      showlegend = FALSE
    )
  
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

edge_notes <- edge_annotation %>% purrr::map_df(`[`)

network <- network %>%
  # add_trace(data = edge_notes, x = ~x, y = ~y, mode = "markers",
  #           text = ~text,
  #           hoverinfo = "text",
  #           hoverlabel = list(bgcolor = 'white'),
  #           showlegend=FALSE) %>%
  add_annotations(
    data = edge_notes,
    x = ~ x,
    y = ~ y,
    text = ~ text,
    showarrow = FALSE,
    showlegend = FALSE
  )  %>%
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
            network,
            #  shapes = edge_shapes,
            xaxis = axis,
            yaxis = axis)

p
