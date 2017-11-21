##Load Data from Beer Tweets Data Wrangling RMD file
##Because of private credentials, Data Wrangling file is
AllBeerTweets <- read.csv("AllFormattedTweets.csv")

##Sentiment Analysis From package syuzhet
TweetsSent <- AllBeerTweets %>% 
  mutate(reformattedtext = as.character(reformattedtext)) %>% 
  mutate(sentiment = get_sentiment(reformattedtext, method = "syuzhet")) %>% 
  mutate(sentimentnrc = get_sentiment(reformattedtext, method = "nrc"))

head(TweetsSent)

dir.create("graph/",showWarnings = F)
png('graph/SentHist.png')


#Density Plot
ggplot(TweetsSent, aes(x = sentiment, fill = "red")) + 
  geom_density(position = "identity", alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Density of Sentiment Scores for all beer companies") + 
  xlab("Sentiment Score") + ylab("Density")
dev.off()

png('graph/SentHistIndividual.png')

TweetsSent2 <- TweetsSent

#Individual Density Plot
ggplot(TweetsSent2, aes(x = sentiment, fill = "green")) + 
  geom_density(position = "identity", alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Density of Sentiment Scores for individual beer companies") + 
  xlab("Sentiment Score") + ylab("Density") +
  facet_wrap(~screenName)

dev.off()

#Summarise Average Sentiment Scores
testdf <- TweetsSent %>% 
  group_by(screenName, weekday) %>% 
  summarise(Mean = mean(sentiment, na.rm = T)) %>% 
  arrange(screenName, weekday)

#Relevel the days of the week to progress towards weekend
levels(testdf$weekday) <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")


png('graph/SentWeekday.png')

#Individual Sentiment Plot per weekday
ggplot(testdf, aes(weekday, Mean)) +
  geom_point() +
  facet_wrap(~screenName) + 
  theme_bw() +
  theme(legend.position = "none") + 
  ggtitle("Average Sentiment Score as the week progresses to the weekend") + 
  ylab("Average Sentiment Score") + xlab("Day of the Week") +
  facet_wrap(~screenName)

dev.off()


