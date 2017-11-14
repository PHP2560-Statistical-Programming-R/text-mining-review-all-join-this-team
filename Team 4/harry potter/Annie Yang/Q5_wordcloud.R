# 5. Using wordcloud to find the most common words in Harry Potter

sevenbook%>%
  count(word)%>%
  with(wordcloud(word,n,max.words=100))

# Throughout the seven books, according to the wordcloud, we also get the main characters are "Harry", "Ron", "Hermione", "Dumbledore" and "Hagrid"...

# Find the most common positive and negative words
  
sevenbook%>%
  inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort=T)%>%
  acast(word~sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),
                   max.words=50)  

# From the word cloud, we find that the most common positive words throughout the series are "magic", "top", "happy", "gold", "love", "nice"... 
# And the most common negative words are "dark", "fell", "hard", "death"...