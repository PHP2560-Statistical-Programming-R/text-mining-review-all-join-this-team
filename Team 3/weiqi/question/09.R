load("data/modifieddata/HP6filterbigrams.Rda")

#split bigram into two columns, which are word1 and word2
bigrams_separated <- Book6bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#remove cases where either is a stop-word
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#define a vector of negation words
negation_words <- c("not", "no", "never", "without")

#filter all the rows which word1 belongs to one of the negation words. 
#using "afinn" to get the sentiment of word2, and count how many times the word appers in a chapter
not_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>%
  group_by(chapter) %>%
  inner_join(get_sentiments("afin"), by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()


#because we assumed "not object"as positive words before, so we need to substract two times of the (n*score) in order to find the net sentiment value of each chapter. We take the negative value because the actual socre is opposite to the sentiment of the word contribution
contribution <- not_words %>%
  mutate(contributionscore = (-2* n * score)) %>%
  select (chapter, contributionscore) %>%
  group_by(chapter) %>%
  summarise(n =sum(contributionscore))

contribution

realresult <- merge(contribution, Book6Senti, by="chapter") %>%
  mutate(n= (n.x + n.y))

x <- realresult %>%
  select(chapter, n, n.y)

png('graph/p6.png', width = 800, height = 500)
p6 <- ggplot(x, aes(x = chapter)) + 
  geom_line(aes(y = n.y, color = 'original')) + 
  geom_line(aes(y = n, color = 'adjusted'))

print (p6+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(color = "black"), plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(
    breaks = c(1:30)) +
  geom_hline(yintercept = 0, color = 'blue') +
  scale_y_continuous(
    breaks = c(seq(-250,250,by=50)) 
  )+ 
  ggtitle("Change in Sentiment of Half-Blood Prince After Adjustment "))

dev.off()



