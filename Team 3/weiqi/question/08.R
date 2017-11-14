load("data/modifieddata/HP6filterbigrams.Rda")

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n >3 ) %>%
  graph_from_data_frame()

png('graph/p5.png', width = 2000, height = 1600)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

print (ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void())

dev.off()
