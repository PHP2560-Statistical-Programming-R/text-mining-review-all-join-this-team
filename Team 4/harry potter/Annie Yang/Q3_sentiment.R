# 3. Sentiment analysis
  
# 3.1 What are common joy words and sad words in the seven novels?
  

# Extract joy words from sentiment dataset NRC.
nrcjoy<-get_sentiments("nrc")%>%filter(sentiment=="joy")

# Use inner_join to perform the sentiment analysis.
joy<-sevenbook%>%
  inner_join(nrcjoy)%>%
  count(title,word,sort=T)%>%
  rename(joyword=word)

joy_graph<-joy%>%group_by(title)%>%
  top_n(10,n) %>%
  ungroup()%>%
  mutate(joyword = reorder(joyword, n)) %>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(joyword, n,fill=title)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  facet_wrap(~title,scales="free")

# We can see in each novel, the common joy words is "found". Also, "ministry", "magical", "hope", "smile"... are frequently used joy words in seven books.




# Extract sad words from sentiment dataset NRC.
nrcsad<-get_sentiments("nrc")%>%filter(sentiment=="sadness")

# Use inner_join to perform the sentiment analysis.
sad<-sevenbook%>%inner_join(nrcsad)%>%count(title,word,sort=T)%>%rename(sadword=word)

sad_graph<-sad%>%filter(sadword!="harry")%>% # Since "harry" is a sadword in nrc, here we eliminate "harry" from sad
  group_by(title)%>%
  top_n(10,n) %>%
  ungroup()%>%
  mutate(sadword = reorder(sadword, n))%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(sadword,n,fill=title)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  facet_wrap(~title,scales="free")

# We can see in each novel, the common sad words is "black", "dark". Also, "kill", "bad", "leave", "death"... are frequently used sad words in seven books. If we use NRC to do the sentiment analysis, we will find something wierd since "mother" is in both joy and sad words list.

# Check the word "mother" in NRC lexicon. We can see that "mother" can be different sentiment.

get_sentiments("nrc")%>%filter(word=="mother")



# 3.2 How does the sentiment change along with the series / chapters? Does it become more positive or negative? 
  
# 3.2.1 Compare the ratio of negative and positive words used in the seven books. Bigger ratio indicate more negative sentiment.

ratio_np<-sevenbook%>%inner_join(get_sentiments("bing"))%>%
  count(title,series,sentiment,sort=T)%>%
  spread(sentiment,n)%>%
  arrange(series)%>%
  mutate(ratio=negative/positive)%>%
  ggplot(aes(series,ratio))+
  geom_line(color="blue")

# The line graph shows that the ratio of negative and positive words fluctuates, a high ratio usually followed by a relatively low ratio in the next book, except that the ratio of prisoner_of_azkaban is higher than chamber of secrets.

# 3.2.2 How does the ratio change through chapters in each book?
harry_title<-c("philosophers_stone","chamber_of_secrets","prisoner_of_azkaban","goblet_of_fire","order_of_the_phoenix","half_blood_prince","deathly_hallows")

ratio_chapter_np<-sevenbook%>%inner_join(get_sentiments("bing"))%>%
  group_by(title)%>%
  count(series,chapter,sentiment,sort=T)%>%
  spread(sentiment,n)%>%
  ungroup()%>%
  arrange(series)%>%
  mutate(ratio=negative/positive)%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(chapter,ratio,color=title))+
  geom_line()+
  facet_wrap(~title,scales="free")

# The line graphs of each book show that at the end of the story, the ratio of negative and postive words declines to a lower level, which means the story has a relatively "happy ending". Also according to the fluctuation of each book, we know the ups and downs of the sentiment. For example, in the half blood prince, there is a peak of negative sentiment in chapter 29.
