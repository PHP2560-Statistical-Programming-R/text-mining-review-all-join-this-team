load("data/rawdata/HP6.Rda")
png('graph/p4.png', width = 800, height = 500)

colorPalette="Dark2"

p4 <- Book6 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, 
                 max.words = 100, 
                 scale=c(11,0.8),
                 random.order = TRUE, 
                 random.color = FALSE, 
                 colors= c("indianred1","indianred2","indianred4","indianred")))

dev.off()

