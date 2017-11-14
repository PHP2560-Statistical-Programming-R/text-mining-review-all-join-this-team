# Examine how sentiment changes throughout each novel by section
  

# Create a tidy text format that record the line number of each word.
tidy_sent<-vector("list",7)
for(i in 1:7) {
  tidy_sent[[i]]<-harry_df[[i]]%>%unnest_tokens(sentence,text,token="sentences")%>%
    mutate(linenumber=row_number())%>%ungroup()%>%unnest_tokens(word,sentence)
  data(stop_words)
  tidy_sent[[i]]<-tidy_sent[[i]]%>%anti_join(stop_words)%>%mutate(title=harry_title[i],series=i)
}

series<-do.call(rbind,tidy_sent)
series



# Use Bing lexicon to analyze
series_bing<-series%>%
  inner_join(get_sentiments("bing"))%>%
  group_by(title)%>%
  count(index=linenumber%/%100,sentiment)%>% # Using 100 lines as a section
  spread(sentiment,n,fill=0)%>%mutate(sentiment=positive-negative)%>%
  ungroup()%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(index,sentiment,fill=title))+
  geom_col(show.legend = F)+
  ylab("sentiment=positive-negative")+
  ggtitle(" BING sentiment ")+
  facet_wrap(~title,scales="free")
## Usually, there are more negative words in each section.


# Use AFINN lexicon to analyze
series_afinn<-series%>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(title)%>%
  mutate(index=linenumber%/%100)%>%
  ungroup()%>%
  group_by(title,index)%>%
  summarise(sentiment=sum(score))%>% # Calculate the total score of each section throughout the novel
  ungroup()%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(index,sentiment,fill=title))+
  geom_col(show.legend = F)+
  ylab("sentiment=sum(score)")+
  ggtitle(" AFINN sentiment ")+
  facet_wrap(~title,scales="free")
## The results seem to be more reasonable by using AFINN lexicon.

# Take philosophers stone as an example to examine how sentiment changes throughout the chapter - bing
stone_sentence<-unnest_tokens(harry_df[[1]],sentence,text,token="sentences")%>%
  group_by(chapter)%>%
  mutate(linenumber=row_number())%>%
  ungroup()%>%
  unnest_tokens(word,sentence)

sentence_sent<-stone_sentence%>%
  inner_join(get_sentiments("bing"))%>%
  count(chapter,index=linenumber%/%50,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(sentiment=positive-negative)

stone_graph<-ggplot(sentence_sent,aes(index,sentiment,fill=chapter))+
  geom_col(show.legend = F)+
  ylab("sentiment=positive-negative")+
  ggtitle(" BING sentiment - Philosophers Stone ")+
  facet_wrap(~chapter,ncol=5,scales="free")


