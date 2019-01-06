library(readr)
library(ggraph)
library(dplyr)
library(tidygraph)
library(networkD3)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(lubridate)
library(randomcoloR)

make_graph <- function(tangled) {
  
  tangled <- tangled %>% mutate(note = if_else(is.na(note), "",note))
  
  # attempt to roll up the payments
  money_types <- c("payment", "loan", "investment", "fine")
  tangled <- tangled %>% filter(e_type %in% money_types) %>% 
    mutate(amt = as.numeric(note)) %>%
    group_by(from, to, e_type) %>%
    summarize(date = last(date),
              sum = sum(amt),
              note = if_else(is.na(sum), last(note), format(sum, scientific = F))
    ) %>% bind_rows(
      tangled %>% filter(!(e_type %in% money_types))
    )
  
  # make a new type that summarizes the original types
  
  display_types <- tibble(
    e_type = c("payment", "association", "investment", "loan", "fine", "verdict" ),
    d_type = c("money", "contact", "money", "money", "verdict", "verdict")
  )
  
  tangled <- tangled %>%
    full_join(display_types, by = c("e_type" = "e_type"))
  
  graph <- as_tbl_graph(tangled) %>% mutate(group = as.character(group_spinglass()))
  
  # the below few line will find the pagerank for all nodes, and use the 
  # max pagerank as the group label
  g<-graph %>% 
    mutate(centrality = centrality_pagerank()) %>% activate(nodes) %>% 
    group_by(group) %>% mutate(g_max =  max(centrality))
  
  
  max_cent_df <- g %>% activate(nodes) %>% 
    as_tibble() %>% 
    group_by(group) %>% 
    summarize(g_max = max(centrality))
  
  # the last summarize there handles ties
  max_cent <- g %>% activate(nodes) %>% as_tibble()%>% 
    filter(centrality %in% max_cent_df$g_max)  %>% 
    rename(group_label = name) %>% ungroup() %>%
    group_by(group) %>% 
    arrange(g_max, desc(centrality), group_label) %>% 
    summarize(
      group_label = first(group_label),
      centrality = first(centrality),
      g_max = first(g_max)
    )
  
  graph <- g  %>% activate(nodes) %>%
    inner_join(max_cent, by = c("group" = "group", 
                                "g_max" = "centrality")) %>% 
    select(-g_max.y)
  

  
  graph
}

weight_graph <- function(graph, in_group, out_group) {
  
  # for FR layouts, let's set an edge weight: in group = 2, out of group = 1
  get_group <- function(node, graph) {
    graph %>% activate(nodes) %>% as_tibble() %>% filter(row_number() == node) %>% pull(group) %>% as.numeric()
  }
  
  weights <- graph %>% activate(edges) %>% as_tibble() %>% rowwise() %>% 
    mutate(the_group = if_else(get_group(to,graph) == get_group(from,graph),get_group(from,graph),NULL)) %>%
    group_by(the_group) %>% mutate(n=n()) %>% ungroup() %>% 
    mutate(max_n = max(n), weight = if_else(!is.na(the_group), in_group, out_group)) %>% pull(weight)
  
  graph <- graph %>% activate(edges) %>% mutate(weight = weights)
  
  graph
}

get_palette <- function(graph) {
  # now handle some aesthetics
  n_group <- graph %>% activate(nodes) %>% pull(group) %>% n_distinct()
  
  base_pal <- c("#8AAFDD", "#ECD1FB", "#F48CDC", "#F06B8A", "#529756", "#BB3DD1", 
              "#D7A4B4", "#E8E1AE", "#9E922A", "#D299F2", "#FEECF2", "#AEF9E1", 
              "#41BA30", "#5A6AC5", "#6DEAA3", "#D3F79C", "#AFB7B1", "#FEA185", 
              "#F5FC72", "#86E4FB", few_pal()(8))
  
  #distinctColorPalette(n_group, runTsne = TRUE)
  base_pal
}