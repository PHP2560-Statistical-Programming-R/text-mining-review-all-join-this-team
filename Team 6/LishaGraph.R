library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(rebus)
library(tidytext)
library(Hmisc)

#QUESTION 3: Most popular words among the tweets that have highest number of retweets in each month?
tweets<-read.csv("AllFormattedTweets.csv")

tweets$date<-as.Date(tweets$date.stamp)
tweets$yearmon<-format(tweets$date,"%Y-%m")

retweets <- tweets %>%
  group_by(yearmon) %>%
  filter(retweetCount==max(retweetCount)) %>%
  select(reformattedtext, screenName,yearmon,retweetCount)
save(retweets,file="data/retweets.Rda")
# Tokenize text into words and remove stop words
retweets$reformattedtext<-as.character(retweets$reformattedtext)
retweets <-retweets %>%
  unnest_tokens(word,reformattedtext) 
clean_tweets <- anti_join(retweets,stop_words,by="word")
clean_tweets <- clean_tweets %>%
  filter(word %nin% c("ve","de","80","e2","ec","eb","a6","a4","ed","98","94","99","ea","9c","8b","84","bc","b8","ad","ac","9d","95"))
top_tweets <-clean_tweets %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(10) %>%
  arrange(desc(n))

#ggplot
dir.create("graph/",showWarnings = F)
png('graph/plot.png')

ggplot(top_tweets) +
  geom_bar(mapping = aes(x=reorder(word,n),y=n),stat="identity")+xlab("words") +ylab("Frequency") + coord_flip()+
  ggtitle("Common words among tweets with highest retweets monthly") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(size = 11,face="bold"))
dev.off()
