library(tidyverse)
library(tidytext)
library(ggraph)
data(stop_words)
dat <- read_csv("starwars.csv") %>% 
  mutate(
    id = 1:nrow(.),
    text = gsub("[-/]", " ", title),
    text = tolower(gsub("[^A-Za-z ]", "", text))
  ) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(word != "cnet")

cos_mat <- cosine_matrix(dat, .01, .80, .80); dim(cos_mat)
g <- graph_from_adjacency_matrix(cos_mat, "undirected", TRUE)

topics <- walktrap_topics(g)
V(g)$cluster <- arrange(topics, word)$group

set.seed(1839)
ggraph(g, layout = "nicely") +
  geom_edge_link(
    aes(alpha = weight), 
    show.legend = FALSE
  ) + 
  geom_node_label(
    aes(label = name, color = cluster),
    show.legend = FALSE
  ) +
  theme_void()

topics
