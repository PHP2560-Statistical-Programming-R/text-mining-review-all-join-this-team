# 2. Character analysis: How does the proportion of the three main characters change along with the series / chapters?
#How does the proportion of other characters change along with the series?

# Calculate the proportion of word in each novel
words_prop<-sevenbook%>%
  group_by(title)%>%
  count(series,word,sort=T)%>%
  mutate(proportion=n/sum(n))%>%
  ungroup()

# Calculate the words' proportion by chapters in each novel

words_prop_chapter<-sevenbook%>%
  group_by(title,chapter)%>%
  count(series,chapter,word,sort=T)%>%
  mutate(proportion=n/sum(n))%>%
  ungroup()


# 2.1 How does the proportion of the three main characters change along with the series?

# Plot the proportion of the three main characters in each book. 

prop_book_graph<-words_prop%>%filter(word%in%c("harry","ron","hermione"))%>%
  ggplot(aes(series,proportion,color=word))+
  geom_line()

## The propotion of harry and ron slightly decreases with the series, while the proportion of hermione slightly increases. 
# In the second book (chamber of secrets) there is a relatively big gap between the proportion of Ron and Hermione.


# 2.2 How does the proportion of three main characters change along with the chapters in each book?
  
# Draw line plots of each novel to compare the proportion change 

prop_chapter_graph<-words_prop_chapter%>%filter(word%in%c("harry","ron","hermione"))%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(chapter,proportion,color=word))+
  geom_line()+
  facet_wrap(~title,scale="free_x")

## For the fans of ron or hermione, they can find in which chapter the character has a relatively high proportion. For example, in the first book (philosophers stone), Ron and Hermione appear from the 6th chapter.


# 2.3 How does the proportion of other characters change along with the series?
  
other_prop<-words_prop%>%filter(word%in%c("dumbledore","snape","hagrid","voldemort"))%>%
  ggplot(aes(series,proportion,color=word))+
  geom_line()+
  scale_x_continuous(breaks=seq(1,7,1))

# The line plot shows that the proportion of Hagrid goes down with the series. Overall, the proportion of Dumbledore goes up from 1 to 6 and it drops in series 7. 
