# Q1: Common words analysis

## 1.1 What are top words in each book?

top_words<-sevenbook%>%
  group_by(title)%>%
  count(word,sort=T)%>%
  top_n(10,n)%>%
  arrange(title)

theme_update(plot.title = element_text(hjust = 0.5)) # center title

graph_top<-sevenbook%>%
  group_by(title)%>%
  count(word,sort=T)%>%
  top_n(10,n) %>%
  ungroup()%>%
  mutate(word=reorder(word,n))%>%
  mutate(title= factor(title,levels=harry_title))%>%
  ggplot(aes(word,n,fill=title))+
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  facet_wrap(~title,scales="free")

# From the bar charts, we find that main characters are Harry, Ron and Hermione.
# And most common words are usually related to characters.

## 1.2 What are common words in the series after removing characters' names?

# Custom stop-words
custom_stop_words<-bind_rows(
  data_frame(word=c("harry","harry's","potter","ron","hermione",
                    "dumbledore","snape","hagrid","weasley",
                    "voldemort","malfoy","professor"),
             lexicon=c("custom")),
  stop_words
)
# After removing the characters's names, what are top 10 words in each novel?

no_char_graph<-sevenbook%>%
  anti_join(custom_stop_words)%>%
  group_by(title)%>%
  count(word,sort=T)%>%
  top_n(10,n) %>%
  ungroup()%>%
  mutate(title= factor(title,levels=harry_title))%>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n,fill=title)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  facet_wrap(~title,scales="free_y")

# The bar charts show that "looked", "eyes", "time"... are in the top words