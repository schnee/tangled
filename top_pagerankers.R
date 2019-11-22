suppressPackageStartupMessages({
  library(readr)
  library(ggraph)
  library(dplyr)
  library(tidygraph)
  library(networkD3)
  library(ggthemes)
  library(RColorBrewer)
  library(scales)
  library(lubridate)
  library(purrr)
})

# source("./functions.R")
# 
# show.top.n <- 10L
# 
# tangled <-
#   read_csv(
#     "https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv"
#   )

make_ranking_plot <- function(tangled, show_top_n, start = 0.5, step = 0.5) {
  my_theme <- function() {
    # Colors
    color.background = "#fffbf0ff"
    color.text = "#22211d"
    
    # Begin construction of chart
    theme_bw(base_size = 15) +
      
      # Format background colors
      theme(panel.background = element_rect(fill = color.background, color =
                                              color.background)) +
      theme(plot.background  = element_rect(fill = color.background, color =
                                              color.background)) +
      theme(panel.border     = element_rect(color = color.background)) +
      theme(strip.background = element_rect(fill = color.background, color =
                                              color.background)) +
      
      # Format the grid
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.minor.y = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "none") +
      
      # Format title and axis labels
      theme(plot.title       = element_text(
        color = color.text,
        size = 20,
        face = "bold"
      )) +
      theme(axis.title.x     = element_text(
        size = 14,
        color = "black",
        face = "bold"
      )) +
      theme(axis.title.y     = element_text(
        size = 14,
        color = "black",
        face = "bold",
        vjust = 1.25
      )) +
      theme(axis.text.x      = element_text(
        size = 10,
        vjust = 0.5,
        hjust = 0.5,
        color = color.text
      )) +
      theme(axis.text.y      = element_text(size = 10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, .2, 0.3, .2), "cm"))
  }
  
  
  get_tangler <- function(fraction, tangled, topNCt) {
    topPctCt <- tangled %>% nrow() %>%
      magrittr::multiply_by(fraction) %>%
      ceiling()
    
    graph <- tangled %>% top_n(topPctCt) %>%
      mutate(note = if_else(is.na(note), "", note)) %>%
      as_tbl_graph() %>%
      mutate(centrality = centrality_pagerank())
    
    graph %>% activate(nodes) %>% as_tibble() %>%
      arrange(desc(centrality)) %>%
      dplyr::top_n(topNCt, centrality) %>% select(name, centrality) %>%
      mutate(frac = fraction)
  }
  
  the_fractions <- seq(from = start, to = 1.0, by = step)
  
  if(the_fractions[length(the_fractions)] != 1.0) {
         the_fractions <- c(the_fractions, 1)
  }
  
  the_df <- the_fractions %>% map_df(get_tangler, tangled, show_top_n)
  
  # annotate the_df to create a bump chart
  
  the_df <- the_df %>%
    group_by(frac) %>%
    arrange(frac, desc(centrality)) %>%
    mutate(ranking = row_number()) %>% ungroup()
  
  the_names <- the_df %>% select(name) %>% unique() %>% pull(name)
  
  dummies <- data.frame(
    name = rep(the_names, length(the_fractions)),
    frac = the_fractions,
    ranking = rep(show_top_n + 1L, length(the_fractions) * length(the_names)),
    centrality = rep(0.0, length(the_fractions) * length(the_names))
  ) %>% tidyr::expand(name, frac, ranking, centrality)
  
  
  plot_df <-
    the_df %>% right_join(dummies, by = c("name" = "name", "frac" = "frac")) %>%
    group_by(frac) %>%
    mutate(centrality.x = if_else(is.na(centrality.x), centrality.y, centrality.x)) %>%
    mutate(ranking.x = if_else(is.na(ranking.x), ranking.y, ranking.x)) %>%
    ungroup() %>%
    select(name,
           centrality = centrality.x,
           frac,
           ranking = ranking.x) %>%
    arrange(frac, ranking)
  
  final_order <- plot_df %>% filter(frac == max(frac)) %>%
    arrange(desc(centrality)) %>%
    pull(name)
  
  plot_df <-
    plot_df %>% mutate(name = factor(name, levels = final_order))
  
  
  
  rank_pal <- c(
    ggthemes::few_pal("Dark")(8),
    brewer.pal(8, name = "Accent"),
    rep("#aaaaaa", length(the_names) - 8 - 8)
  )
  
  ggplot(data = plot_df, aes(x = frac, y = ranking, group = name)) +
    geom_line(aes(color = name), size = 2) +
    geom_point(aes(color = name), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = 1:show_top_n)  +
    geom_label(
      data = plot_df %>% filter(frac == min(frac)),
      aes(label = name, x = 0.04) ,
      hjust = 0,
      nudge_y = 0.3,
      fontface = "bold",
      color = "#555555",
      size = 4
    ) +
    geom_label(
      data = plot_df %>% filter(frac == max(frac)),
      aes(label = name, x = 1.0) ,
      hjust = 1,
      nudge_y = 0.3,
      fontface = "bold",
      color = "#555555",
      size = 4
    ) +
    coord_cartesian(ylim = c(1, show_top_n)) +
    scale_color_manual(values = rank_pal) +
    theme(legend.position = "none") +
    labs(
      x = "Fraction of data",
      y = "Page Rank",
      title = "The Tangled Web's Most Important Names",
      caption = "Data ordered by entry date, not event date"
    ) +
    my_theme()
  
  ggsave("docs/rankings.png", width = 16, height = 9)
}
