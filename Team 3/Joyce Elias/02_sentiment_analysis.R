## This script tracks the negative and positive sentiment across one book in the harry potter series, in 2 ways. The first way is to use the bing lexicon and the second way is the afin lexicon
load("Joyce Elias/clean data/books.rda")
dir.create("./Joyce Elias/graph/", showWarnings = FALSE) # this creates a directory to store my results in 


book_count= 
  books %>%
  filter(title== "philosophers_stone" ) %>%  # Just looking at the rows which contain Philsopher's stone
  inner_join(get_sentiments("bing")) %>%  # we are using the bing lexicon to examine sentiment
  group_by(chapter,sentiment) %>% # I want to group by chapter and by sentiment ( positive or negative)
  dplyr::  count(chapter,sentiment) %>% #I want to the count the sentiment based on chapter
  mutate(total_words=sum(n),prop=n/total_words)  # what proportion of the total words are positive and negative in each chapter


  ggplot(book_count,aes(x = chapter, y = prop, col = as.factor(sentiment)))  +
    geom_line()+   # plotting the sentiment over each chapter in the book
    ggtitle("Tracking Positive and Negative Sentiment Across the Philosopher's Stone") +
    labs(y="proportion of total words") 
# Save plot in correct folder
  ggsave('./Joyce Elias/graph/plot1.png') 



#I can also use a different lexicon that assings a score rather than saying positive and negative. Affin lexicon 

affin_lex<-
  books %>%
  filter(title=="philosophers_stone")%>%
  inner_join(get_sentiments("afinn"))%>%  # This lexicon assigns a score from -5 to +5 based on negative and positive 
  dplyr::group_by(chapter) %>%
  dplyr::summarise(total_score=sum(score)) # what is the total score for each chapter. Do some chapters tend to be more positive than negative?

ggplot(affin_lex, aes(x = chapter, y = total_score)) +
  geom_line(col="blue1")+   # plotting the sentiment over each chapter in the book based on score
  geom_hline(aes(yintercept=0),color="darkorchid4",linetype="dashed")+
  ggtitle("Tracking Sentiment Across The Philosopher's Stone") +
  labs(y="score")

ggsave('./Joyce Elias/graph/plot2.png')

#For this part I want to see the most common words of the whole book and then the most common words by chapter.
philosophers_stone<-
  books %>%
  filter(title=="philosophers_stone") 

nrc_lex<-philosophers_stone %>%
  inner_join(get_sentiments("nrc"))


sentiment_by_chapter<-
nrc_lex %>%
  group_by(chapter,word) %>%
  dplyr::count(word)%>%
  top_n(2)  # Since I grouped the data by chapter, top_n lets me see the top x # of words for each chapter. For example top_n(5) means I will see the top 5 words in each chapter. 



  ggplot(sentiment_by_chapter,aes(x=reorder(word,n),y=n))+
  geom_col(stat="identity",fill="coral1")+
  coord_flip()+
  ggtitle("Most Common Words in the Philosopher's Stone")+
  labs(y="count")

  ggsave('./Joyce Elias/graph/plot3.png')

#I can also make this graph for each chapter.
top10<-
nrc_lex %>%
  group_by(chapter) %>%  # i grouped it by sentiment
  dplyr::count(word)%>% # I count the number of times the word appears in the book
  top_n(10)# I then select the top 10 words for each sentiment
  

  ggplot(top10,aes(word,n,fill=chapter))+ # I am making a plot that fills in the bar with the sentiment
  geom_col(show.legend=FALSE)+ # this makes columns and excludes the legend
  ggtitle("Top Ten Words in Each Chapter of The Philosopher's Stone")+
  labs(y="count")+
  facet_wrap(~chapter,scales="free")+ # this feature tells R to make this graph for each sentiment
  coord_flip() # flip the x and y axis

  ggsave('./Joyce Elias/graph/plot4.png')

# Next I would like to look at the most popular characters in the first harry potter book.
characters<-
  philosophers_stone%>%
  select(word) %>%
  filter(str_detect(word,UPPER %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR)) %>%
  filter(!word %in% c( "The","They","What","There","It's","Then")) %>%
  group_by(word) %>%
  dplyr::summarise(count=n()) %>%
  top_n(10)%>%
  arrange(desc(count))

ggplot(characters,aes(x=reorder(word,count),y=count))+
  geom_col(fill="salmon") +
  ggtitle("Most Popular Characters in the Philosopher's Stone") +
  labs(x="Characters") +
  geom_text(aes(label=paste0(count)),nudge_y=1) +
  coord_flip()

ggsave('./Joyce Elias/graph/plot5.png')

# For this next part I would like to see how the anticipation changes throughout the book. JK rowling is known for how she strings the reader along and how she infuses anxiety into her text. We will examine this here using the NRC lexicon 
nrc_look=
  books %>%
  filter(title=="philosophers_stone") %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment=="anticipation")%>%
  group_by(chapter) %>%
  dplyr::count(sentiment)



ggplot(nrc_look,aes(x=chapter, y=n))+ 
  geom_col(stat="identity",fill="gold3")+
  ggtitle("The number of Anticipation words by Chapter")+
  labs(y="count")
ggsave('./Joyce Elias/graph/plot6.png')

#Another way of tracking anticipation would be to look at the proportion of words which have an anticipation sentiment rather than looking at the raw usage of the words :
total_words=
  books %>%
  filter(title=="philosophers_stone") %>%
  inner_join(get_sentiments("nrc"))%>%
  group_by(chapter,sentiment) %>%
  dplyr::count(word) %>%
  ungroup()%>%
  group_by(chapter,sentiment)%>%
  dplyr::summarise(word_occurence=sum(n))%>%
  mutate(total_words=sum(word_occurence)) %>%
  
filter(sentiment=="anticipation") %>%
  mutate(proportion= word_occurence/total_words) 

ggplot(total_words,aes(x=chapter,y=proportion))+
  geom_line(col="deeppink3")+
  ggtitle("Tracking the Anticipation sentiment in Philosopher's Stone")+
  labs(y="Proportion of Anticipation Words")

ggsave('./Joyce Elias/graph/plot7.png')

#For my own personal analysis I want to look at a variety of aspects in the first harry potter book.
##I want to start by looking at the top 10 words in each sentiment across the entire book
sentiment<-
nrc_lex %>%
  group_by(sentiment) %>%  # i grouped it by sentiment
  dplyr::count(word)%>% # I count the number of times the word appears in the book
  top_n(10)  # I then select the top 10 words for each sentiment
  

  ggplot(sentiment,aes(word,n,fill=sentiment))+ # I am making a plot that fills in the bar with the sentiment
  geom_col(show.legend=FALSE)+ # this makes columns and excludes the legend
  facet_wrap(~sentiment,scales="free")+ # this feature tells R to make this graph for each sentiment
  ggtitle("Word Contribution by Sentiment")+
  coord_flip()+ # flip the x and y axis
  labs(y="count")

  ggsave('./Joyce Elias/graph/plot8.png')
  
## I would like to see the contribution that each sentiment makes based on the afinn lexicon
sentiment_contribution<-
  philosophers_stone %>%
  dplyr::count(chapter,word) %>% # counts the number of items a specific word appears in the chapter
  inner_join(get_sentiments("afinn")) %>%  # inner_join with the afinn lexicon which assigns a score based on how positive or negative the word is
  group_by(chapter)%>%
  mutate(contribution = (score*n)/sum(n)) %>%  # this essentially weights each word by the number of times it appears
  top_n(10)%>%
  ungroup%>%
  mutate(word = reorder(word, n))



ggplot(sentiment_contribution,aes(x=word,y=contribution))+
  geom_col(show.legend=FALSE,fill="darkseagreen2") +
  facet_wrap(~chapter,scales="free")+  # I facet wrapped based on chapter so I could see the top 10 words in each chapter and the corresponding contribution
  coord_flip()+
  ggtitle("Most Commons Words in Each Chapter")

ggsave('./Joyce Elias/graph/plot9.png')
