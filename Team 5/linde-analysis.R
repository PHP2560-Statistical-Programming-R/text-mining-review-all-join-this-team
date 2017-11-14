load("data/nyt_words.rda")
sentiments = inner_join(words, get_sentiments("afinn"), by = "word")
sentiments$Date = as.Date(as.Date(sentiments$Date, format = "%d-%b-%y"), format = "%y-%b-%d")

by_topic = 
  sentiments %>%
  arrange(Topic.Code)

by_topic

topic_scores = 
  by_topic %>%
  group_by(Topic.Code) %>%
  summarise(mean.score = mean(score))

library(ggplot2)

plot_scores = 
  ggplot(topic_scores, aes(x = Topic.Code, y = mean.score)) + geom_point(colour = "blue") +
  xlab ("Topic Code") + ylab("Mean Sentiment Score") + geom_line(y = 0, colour = "navy")

# From the graph that examines the distribution of mean sentiment scores with respect
# to each topic code, we discover that most topics contain predominantly negative sentiments.
# Next, we will examine the topics that 1) either have overwhelmingly negative sentiments,
# or have 2) predominantly positive sentiments.

negative_topics = 
  topic_scores %>% 
  arrange(desc(abs(mean.score))) %>%
  head(2)

plot_scores + 
  geom_point(aes(x = 27, y = -2.210526), size = 3, colour = "red") +
  geom_point(aes(x = 30, y = -1.916667), size = 3, colour = "red")

# Topic codes 27 and 30, highlighted in red, appear to reflect the most negative sentiments, 
# with mean sentiment scores of -2.210526	and -1.916667, respectively.	

ggsave("linde_graphs/unnamed-chunk-8-1.png")

positive_topics = 
  topic_scores %>%
  arrange(desc(mean.score)) %>%
  head(2)

plot_scores + 
  geom_point(aes(x = 29, y = 1.065789), size = 3, colour = "forestgreen") +
  geom_point(aes(x = 21, y = 0.375000), size = 3, colour = "forestgreen")

# Topic codes 27 and 30, highlighted in red, appear to reflect the most negative sentiments, 
# with mean sentiment scores of 1.065789 and 0.375000, respectively. 

ggsave("linde_graphs/unnamed-chunk-10-1.png")

word_freq = 
  by_topic %>%
  group_by(Topic.Code, word) %>%
  summarise(freq = n())

words_neg = 
  word_freq %>%
  filter(Topic.Code == "27" | Topic.Code == "30") %>%
  group_by(Topic.Code) %>%
  top_n(3, freq)

# The two topics with most negative sentiments both address 'death' and 'crash'. 
# I will examine how these two topics have evolved over time. In particular, 
# I will examine the most common causes of death as reported by the NYT, 
# and how that has changed over time. 

words_pos = 
  word_freq %>%
  # investigate what might topic codes 21 and 29 address based on most frequent words used in subject
  filter(Topic.Code == "21" | Topic.Code == "29") %>%
  group_by(Topic.Code) %>%
  top_n(3, freq)

# The two topics with most positive sentiments both address 'win' and 'no'. 
# I will examine how these two topics have evolved over time. In particular, 
# I also will examine the most common reasons for "winning" as reported by the NYT, 
# and how that has changed over time.

library(lubridate)

death_freq = 
  by_topic %>%
  filter(word == "death" | word == "dead" | word == "crash") %>%
  group_by(year = floor_date(Date, "year"), word) %>%
  summarise(freq = n())

win_freq = 
  by_topic %>%
  filter(word == "win" | word == "wins" | word == "no") %>%
  group_by(year = floor_date(Date, "year"), word) %>%
  summarise(freq = n())

death_freq
win_freq

library(ggplot2)

plot_neg = 
  ggplot(death_freq, 
         aes(x = death_freq$year, y = death_freq$freq)) + 
  geom_point(colour = "red") + geom_smooth(colour = "orange", span = 1) +
  xlab("Year") + ylab("Frequency of (-) Words") 


plot_pos = 
  ggplot(win_freq, 
         aes(x = win_freq$year, y = win_freq$freq)) + 
  geom_point(colour = "navy") + geom_smooth(colour = "cornflower blue", span = 1) +
  xlab("Year") + ylab("Frequency of (+) Words")

library(ggplot2)
library(gridExtra)
grid.arrange(plot_neg, plot_pos, ncol = 1)

ggsave("linde_graphs/unnamed-chunk-18-1.png")

# Here, we compare the trend of the usage of the most negative and positive words over time. 

# We observe the movement in frequency of the most negative words (death and crash) between 1996-2006. 
# The frequency of their usage dips slighly around the year 2000, then bounces back around the year 2003. 
# However, the mean frequency appears to stabalize at around 2.25. 

# We also observe the movement in frequency of the most positive words (win and no). 
# The frequency of their usage appears to steadily decline over time, although suggesting 
# a slight uptick after the year 2004. 

plot_neg + facet_wrap(~word)

ggsave("linde_graphs/unnamed-chunk-19-1.png")


# Here we break down the frequency of negative words usage in the NYTimes headlines by words. 
# We observe dramatic movements in usage of the word "death" between the years 2002 and 2006.

plot_pos + facet_wrap(~word)

ggsave("linde_graphs/unnamed-chunk-20-1.png")

# Here we break down the frequency of positive words usage in the NYTimes headlines by words. 
# It is difficult to draw conclusions and patterns from this graph, as the movement in frequency
# of positive words usages appears to fluctuate in general for all the positive words under speculation. 