# create directory for graphs
dir.create("graph/", showWarnings = FALSE)

# filter out the stop words and plot the ten most frequently-used words
tidy_words %>% 
  anti_join(stop_words) %>%
  count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(10) %>%
  ggplot(aes(word, n, fill = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top 10 expressions") +
    theme(plot.title = element_text(hjust = 0.5))

# save the plot
ggsave('graph/top10.png')

# save the image of foloowing wordclouds
png('graph/wordcloud.png')

# create weighted wordclouds
par(mfrow = c(2,2))

tidy_words %>% 
  anti_join(stop_words) %>% 
  filter(year == 2017)  %>%
  count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, scale = c(1.5, .2), n, max.words = 100, random.order = F, random.color = F, colors = brewer.pal(8, "Dark2")))
text(x=0.5, y= 0, "2017")

tidy_words %>% 
  anti_join(stop_words) %>% 
  filter(year == 2016)  %>%
  count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, scale = c(1.5, .2), n, max.words = 100, random.order = F, random.color = F, colors = brewer.pal(8, "Dark2"), main = "P"))
text(x=0.5, y=0, "2016")

tidy_words %>% 
  anti_join(stop_words) %>% 
  filter(year == 2015)  %>%
  count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, scale = c(1.5, .2), n, max.words = 100, random.order = F, random.color = F, colors = brewer.pal(8, "Dark2")))
text(x=0.5, y=0.2, "2015")

tidy_words %>% 
  anti_join(stop_words) %>% 
  filter(year == 2014)  %>%
  count(word, sort = T) %>%
  mutate(word = reorder(word, n)) %>%
  with(wordcloud(word, scale = c(1.5, .2), n, max.words = 100, random.order = F, random.color = F, colors = brewer.pal(8, "Dark2")))
text(x=0.5, y=0.2, "2014")

dev.off()


word_counts_all %>%
  # Take the top 5 words for each sentiment
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  # Set up the plot with aes()
  ggplot(aes(word, n, mapping = sentiment)) +
  geom_col(show.legend = FALSE, width = 0.5) +
  facet_wrap(~ sentiment, scales = "free", nrow = 5) +
  coord_flip() +
  ggtitle("Top 5 words for each sentiment") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('graph/top5_sentiments.png')


word_counts_pos_neg %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ggtitle("Top 10 positive & negative words") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('graph/top10_pos_and_neg.png')


sentiment_by_time %>%
  # Filter for positive and negative words
  filter(sentiment %in% c("positive", "negative")) %>%
  # Count by date, sentiment, and total_words
  count(time, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  # Set up the plot with aes()
  ggplot(aes(time, percent, mapping = sentiment)) +
  geom_line(aes(colour = sentiment, group = sentiment), size = 1) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0) +
  ggtitle("Sentiment change over time") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('graph/change_over_time.png')

