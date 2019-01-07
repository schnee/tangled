library(linkprediction)
library(igraph)

fed_ct <- graph %>% activate(nodes) %>% as_tibble %>% mutate(row_num = row_number()) %>%
  filter(name == "Federal Court") %>% pull(row_num)

mat <- proxfun(graph = igraph::as.undirected(graph), method="cn")

V(graph)[as.numeric(names(sort(mat[,fed_ct], decreasing=TRUE)))]


View(mat)


as.numeric(names(sort(mat[,117])))


V(graph)[174]

colnames(mat) <- V(graph)
rownames(mat) <- V(graph)


