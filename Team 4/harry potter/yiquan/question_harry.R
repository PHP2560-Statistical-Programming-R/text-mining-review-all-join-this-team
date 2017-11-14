library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)

#Analyzing by one of the book
##Question1:By ordering the frequency of the word that appears(remove stop words), could you find the word that appears most frequently?
tidy_freq<-tidy_stone%>%filter(freq>100)%>%mutate(word=reorder(word,freq))%>%ggplot(aes(word,freq))+geom_col()+xlab(NULL)+coord_flip()
##reorder means converts word from a character that would be plotted in alphabetical order to a factor that will be plotted in order of n.coord_flip flips the coordinates.geom_col makes the barchart.

##Question2: By doing sentiment analysis, which sentiment word is most likely to be used by author?
stone_sentiment <- stone_df%>%inner_join(get_sentiments("bing"))
stone_sentiment_count<-stone_sentiment%>%group_by(word)%>%count%>%ungroup()
sentiment_likely<-stone_sentiment_count%>%filter(n>20)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n))+geom_col()+xlab(NULL)+coord_flip()

##Question3: By dividing into two groups, which word is used most frequently in each group?
bing_word_counts <- stone_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_freq<-bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#Question4: Can you draw wordcloud picture of the stone book?
tidy_stone2<-stone_df%>%group_by(word)%>%anti_join(stop_words)
stone_df%>%anti_join(stop_words)%>%count(word)%>%with(wordcloud(word,n,max.words=100))
tidy_stone2<-tidy_stone2%>%inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort=T)%>%acast(word~sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),max.words=80)

#Question5: Can you draw a picture of the percentage of the word?
##Caculate the percentage of each word.
stone_word<-stone_df%>%count(word,sort=T)%>%ungroup()
stone_word<-stone_word%>%mutate(total=sum(n))
stone_word<-ggplot(stone_word,aes(n/total))+geom_histogram(show.legend=F,fill="hotpink",color="black")+xlim(NA,0.0009)

#Question6: What is the relationship between the rank and frequency?
freq_by_rank<-stone_word%>%mutate(rank=row_number(),freq=n/total)
freq_by_rank<-freq_by_rank%>%ggplot(aes(rank,freq))+geom_line(size=1,alpha=0.8,color="slateblue")+
  geom_smooth(col="lightblue")+
  scale_x_log10()+scale_y_log10()
#As rank increases, frequency will decrease.

#Question7: Could you divide the text by bigrams rather than words?
stone_bigrams<-text_df%>%unnest_tokens(bigram,text,token="ngrams",n=2) #separate the text into bigram
stone_bigrams%>%count(bigram,sort=T) #count the number of each bigram
bigram_sep<-stone_bigrams%>%separate(bigram,c("word1","word2"),sep=" ")
bigram_filter<-bigram_sep%>%filter(!word1 %in% stop_words$word)%>%filter(!word2 %in%stop_words$word) #remove the stop word
bigram_counts<-bigram_filter%>%count(word1,word2,sort=T) #count the word removed the stop word
bigram_counts
bigram_united<-bigram_filter%>%unite(bigram,word1,word2,sep=" ") #unite the bigram
bigram_united

#Question8: What is the tf_idf of the bigram_united we create in last question?
bigram_tf_idf<-bigram_united%>%count(line,bigram)%>%bind_tf_idf(bigram,line,n)%>%arrange(desc(tf_idf))
bigram_tf_idf

#Question9: Can you show the word after the negative word?
bigram_sep%>%filter(word1=="not")%>%count(word1,word2,sort=T) #count the number of word which is behind "not"
Afinn<-get_sentiments("afinn")
not_words<-bigram_sep%>%filter(word1=="not")%>%inner_join(Afinn,by=c(word2="word"))%>%count(word2,score,sort=T)%>%ungroup() #inner join with "Afinn" to make sentiment analysis
not_words
not_words%>%mutate(contribution=n*score)%>%
  arrange(desc(abs(contribution)))%>%
  mutate(word2=reorder(word2,contribution))%>%
  ggplot(aes(word2,n*score,fill=n*score>0))+geom_col(show.legend = F)+
  xlab("words precede by \"not\"")+ylab("sentiment score * number of occurrences")+coord_flip()
negation_word<-c("not","no","never","without")
negated_words<-bigram_sep%>%
  filter(word1 %in% negation_word)%>%
  inner_join(Afinn,by=c(word2="word"))%>%
  count(word1,word2,score,sort=T)%>%ungroup() #find the number of words behind the negation words and inner join with Afinn to get the score of the word and make sentiment analysis
negated_words<-ggplot(negated_words,aes(word2,score,fill=score>0))+geom_col(show.legend = F)+
  facet_wrap(~word1,scale="free_y")+xlab("words preceded by negation term")+
  ylab("sentiment score * # of occurrence")+coord_flip()

#Question10: Draw the picture of the relationship between different words, using arrows to show the relation.
bigram_graph<-bigram_counts%>%filter(n>6)%>%graph_from_data_frame()
bigram_graph
set.seed(2017)
a<-grid::arrow(type="closed",length=unit(.1,"inches"))
bigram_ggraph<-ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n),show.legend = F,arrow=a,end_cap=circle(.07,"inches")) +
  geom_node_point(color="lightskyblue3",size=5) +
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5)+
  theme_void()

#Question11: What is the correlation between each word?
word_cor<-stone_df%>%group_by(word)%>%filter(n()>=20)%>%pairwise_cor(word,line,sort=T)
word_cor

#Analyzing by book

##Question1: What are the top words appear in each of the book?
top_book<-book%>%group_by(title)%>%count(title,word,sort=T)%>%
  top_n(5)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=title))+
  geom_col()+
  coord_flip()+
  facet_wrap(~title,scales="free_y")

#Question2: Dividing by books, what is the top joy words in each of them?
top_joy_book<-book%>%group_by(title)%>%count(title,word,sort=TRUE)%>%
  inner_join(get_sentiments("nrc"))%>%filter(sentiment=="joy")%>%
  top_n(5,n)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=title))+geom_col()+
  coord_flip()+facet_wrap(~title,scales="free_y")

#Question3: Dividing by books, what is the top "sadness" words in each of them?
top_sad_book<-book%>%group_by(title)%>%count(title,word,sort=T)%>%
  inner_join(get_sentiments("nrc"))%>%filter(sentiment=="sadness")%>%
  filter(word!="harry")%>%   ##remove harry 
  top_n(5,n)%>%
  ungroup()%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=title))+geom_col()+
  coord_flip()+
  facet_wrap(~title,scale="free_y")

#Question4: What is the ratio of the positive word and negative word in each book according to the chapter?
ratio<-book%>%inner_join(get_sentiments("bing"))%>%
  group_by(title)%>%
  count(title,chapter,sentiment,sort=T)%>%
  spread(sentiment,n)%>%
  mutate(ratio=negative/positive)%>%
  ggplot(aes(chapter,ratio,fill=title))+
  geom_col()+
  geom_line(color="blue")+
  facet_wrap(~title,scale="free")

#Question5: What is the difference of negative words and positive words in each book according to the chapter?
diff<-book%>%inner_join(get_sentiments("bing"))%>%
  group_by(title)%>%
  count(title,chapter,sentiment,sort=T)%>%
  spread(sentiment,n)%>%
  mutate(difference=negative-positive)%>%
  ggplot(aes(chapter,difference,fill=title))+
  geom_col()+
  facet_wrap(~title,scale="free")