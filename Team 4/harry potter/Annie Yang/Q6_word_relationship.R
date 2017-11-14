# Create bigram and analyze the relationship between words

harry_bigrams<-vector("list",7)

for(i in 1:7) {
  harry_bigrams[[i]]<-harry_df[[i]]%>%unnest_tokens(bigram,text,token="ngrams",n=2)
  harry_bigrams[[i]]<-harry_bigrams[[i]]%>%
    mutate(title=harry_title[i],series=i)
}

sevenbook_bigrams<-do.call(rbind,harry_bigrams)

# Examine the most common bigrams
bigram_n<-sevenbook_bigrams%>%group_by(title)%>%
  count(bigram,sort=T)
bigram_n
# The most common bigrams are some we are not interesting




# Remove cases either is a stop-word

bigrams_separated<-sevenbook_bigrams%>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# We can see that names are the most common pairs in Harrypotter series. 
# Harry and ron usually appear together. 
# Also, Ron and Hermione usually appear together.
character_relationship<-bigram_counts %>% 
  mutate (rank=row_number()) %>% 
  filter((word1=="harry"& word2=="ron") | (word1=="ron" & word2=="harry")
         |(word1=="ron" & word2 == "hermione")|(word1=="hermione" & word2 == "ron")
         |(word1=="harry" & word2 == "hermione")|(word1=="hermione" & word2 == "harry"))



# Unite and analyze

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(title,series,bigram, sort=T) %>%
  arrange(series)

bigrams_united

# And Professor Mcgonagall is a common character in Harry Potter. From the plot, we find that in the book order of the phoenix, the frequency goes up.

united_graph<-bigrams_united %>% filter(bigram=="professor mcgonagall") %>%
  ggplot(aes(series,n))+
  geom_line()+
  ylab("number of occurrence")+
  ggtitle("Occurrence of Professor Mcgonagall")

# We find that in goblet_of_fire, Harry and Ron usually appear together. 
bigram_harry<-bigrams_united %>%
  filter(str_detect(bigram,".*harry.*")&bigram!="harry potter")%>% 
  # filter the bigrams that contain "harry" except for "harry potter"
  arrange(desc(n))

bigram_harry


# Analyze sentiment associated with Harry
harry_sentiment1<-bigrams_filtered%>%
  filter(word1=="harry")%>%
  inner_join(get_sentiments("afinn"),by=c(word2="word"))%>%
  rename(word=word2)%>%
  ungroup()%>%
  select(-word1)

harry_sentiment2<-bigrams_filtered%>%
  filter(word2=="harry")%>%
  inner_join(get_sentiments("afinn"),by=c(word1="word"))%>%
  rename(word=word1)%>%
  ungroup()%>%
  select(-word2)

harry_sentiment<-rbind(harry_sentiment1,harry_sentiment2)%>%count(word,score,sort=T)

harry_graph<-harry_sentiment%>%
  mutate(contribution=n*score)%>%
  arrange(desc(abs(contribution)))%>%
  head(20)%>%
  mutate(word=reorder(word,contribution))%>%
  ggplot(aes(word,n*score,fill=n*score>0))+
  geom_col(show.legend = F)+
  xlab("sentiment words associated with Harry")+
  ylab("sentiment score*number of occurrence")+
  coord_flip()+
  ggtitle("Harry Sentiment")





# network of bigrams
# Filter for only relatively common combination

bigram_graph<-bigram_counts%>%
  filter(n>60)%>%
  graph_from_data_frame()

bigram_graph



set.seed(2017)

ggraph(bigram_graph,layout="fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),vjust=1,hjust=1)



set.seed(2016)

a<-grid::arrow(type="closed",length=unit(.15,"inches"))

network<-ggraph(bigram_graph,layout="fr")+
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
