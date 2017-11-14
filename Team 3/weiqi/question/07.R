load("data/rawdata/HP6bigrams.Rda")

#split bigram into two columns, which are word1 and word2
bigrams_separated <- Book6bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#remove cases where either is a stop-word
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#unite the two words again 
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#count the most popular bigrams in every chapter 
Book6TopBi <- bigrams_united %>%
  group_by(chapter) %>%
  count(bigram, chapter) %>%
  top_n(1)

save(bigrams_filtered, file = "data/modifieddata/HP6filterbigrams.Rda")
save(Book6TopBi, file = "data/modifieddata/HP6topbi.Rda")

