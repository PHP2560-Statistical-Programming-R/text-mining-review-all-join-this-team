load("data/rawdata/HP6.Rda")

Book6Word <- Book6 %>%
  group_by(chapter)  %>%
  #remove stop words
  anti_join(stop_words) %>%
  count(word, chapter) %>%
  top_n(1)

save (Book6Word, file = "data/modifieddata/HP6Top.Rda")

