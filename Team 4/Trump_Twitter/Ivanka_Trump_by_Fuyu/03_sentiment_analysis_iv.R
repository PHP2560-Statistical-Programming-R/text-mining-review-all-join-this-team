#remove stop words like "the","of","to"
library(tidytext)

data(stop_words)

data_tidy <- data_clean %>%
  select(-text) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,text_clean)%>%
  anti_join(stop_words)

tidy_text <- text_clean %>%
  mutate(linenumber=row_number())%>%
  unnest_tokens(word,text_clean) %>%
  anti_join(stop_words)

library(tidytext)
library(tidyr)
text_s <- data_tidy %>%
  filter(!is.na(year)) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(year,index = linenumber %/% 100, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(text_s, aes(index,sentiment,fill=year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year,ncol=3,scales = "free_x")

# Most common positive and negative words
bing_word_counts <- data_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()
bing_word_counts