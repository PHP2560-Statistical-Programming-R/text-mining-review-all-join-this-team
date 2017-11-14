dir.create("graph/", showWarnings = FALSE)

# find the most used words
library(ggplot2)
tidy_text %>%
  count(word,sort=TRUE) %>%
  filter(n>500)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Top 10 expressions") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('Trump_Twitter/Ivanka_Trump_by_Fuyu/graph/top10_ivanka.png')

# most used word every year
par(mfrow = c(3,3))

library(wordcloud)
library(dplyr)

for(i in seq(2009,2017)){
  frequency <- data_tidy %>%
    group_by(year,word) %>%
    filter(year == i) %>%
    count(year,word,sort = T) %>%
    with(wordcloud(word,n,max.words =100,random.order = F, random.color = F, colors=brewer.pal(8, "Dark2")))
}
png("Trump_Twitter/Ivanka_Trump_by_Fuyu/graph/wordcloud.png")
dev.off()

# Changes in word use
library(lubridate)

word_by_time <- data_tidy %>%
  mutate(timestamp=ymd(date))%>%
  mutate(time_floor = floor_date(timestamp,unit = "1 month")) %>%
  count(time_floor,word) %>%
  ungroup() %>%
  group_by(time_floor)%>%
  mutate(time_total = sum(n)) %>%
  group_by(word)%>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 400)

as_tibble(word_by_time)
nested_data <- word_by_time %>%
  nest(-word)

library(purrr)
nested_models <- nested_data %>%
  mutate(models = map(data,~glm(cbind(count,time_total) ~ time_floor, ., family = "binomial")))

library(broom)
slopes <- nested_models %>%
  unnest(map(models,tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>%
  filter(adjusted.p.value < 0.1)

word_by_time %>% 
  inner_join(top_slopes,by="word") %>%
  ggplot(aes(time_floor,count/time_total,color=word),show.legend = FALSE) +
  geom_line(size = 0.8) +
  labs(x = NULL, y = "Word Frequency")
ggsave('Trump_Twitter/Ivanka_Trump_by_Fuyu/graph/word_change_by_time.png')

# sentiment analysis by sentences in every month and every year
library(ggplot2)
ggplot(text_s, aes(index,sentiment,fill=year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year,ncol=3,scales = "free_x")
ggsave('Trump_Twitter/Ivanka_Trump_by_Fuyu/graph/sentence_sentiment.png')

# Top 10 positive and negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  filter(word != "trump") %>% # it is related to her name and her brand, should not be used in sentiment analysis
  top_n(10)%>%
  ungroup %>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
ggsave('Trump_Twitter/Ivanka_Trump_by_Fuyu/graph/top10_pos_and_neg.png')
