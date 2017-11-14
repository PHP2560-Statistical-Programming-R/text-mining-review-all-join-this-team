word_counts_all <- tidy_words %>%
  # Implement sentiment analysis using the "nrc" lexicon
  inner_join(get_sentiments("nrc")) %>%
  # Count by word and sentiment
  count(word, sentiment) %>%
  # Group by sentiment
  group_by(sentiment)

word_counts_pos_neg <- tidy_words %>%
  # Implement sentiment analysis using the "nrc" lexicon
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  # Count by word and sentiment
  count(word, sentiment)

sentiment_by_time <- tidy_words %>%
  # Define a new column using floor_date()
  mutate(time = floor_date(as_datetime(date), unit = "6 months")) %>%
  # Group by date
  group_by(time) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  # Implement sentiment analysis using the NRC lexicon
  inner_join(get_sentiments("nrc"))



