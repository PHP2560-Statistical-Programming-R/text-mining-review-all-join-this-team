load("Julia/data/books.Rda")
#Q1:How the sentiment changes across the entire book
HP_Q1<-function(){
  sentiments<- books%>%
    #filter for book in the series
    filter (title == "order_of_the_phoenix") %>% 
    #join using the lexicon afinn
    inner_join(get_sentiments("afinn")) %>%
    #group by chapter
    group_by(chapter) %>%
    #count the sentiment by summing the score of each word
    summarise(n= sum(score))
  sentiments
  
  p1 <- ggplot(sentiments, aes(x=chapter, y=n)) + # I am making a plot that fills in the chapter with the score count
    geom_line(show.legend = FALSE, size = 0.3) + #this makes a line and excludes the legend
    scale_color_brewer(palette = "Set1") + #selects the color of the scale
    geom_smooth(se=FALSE, size = 0.2, linetype = 4) #selects the type of line for the graph we want
  
  p1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),axis.line = element_line(colour = "black") )+
    xlim(1, 38) +
    scale_x_continuous(
      breaks = c(1:38)) + #create breaks for each chapter
    geom_hline(yintercept = 0)+ #create a y-intercept at 0 in order to help depict negative values better
  ggsave('Julia/graph/HP_Q1.png')
}
HP_Q1()

#Q2:What are the most common words by chapter
HP_Q2<-function(){
common_words<-books%>%
  #filter for the book in series
  filter(title =="order_of_the_phoenix")%>%
  #eliminate the stop words
  anti_join(stop_words)%>%
  #group by chapter
  group_by(chapter)%>%
  #count the words and sort by most used
  count(word, sort=TRUE)%>%
  #top word for each chapter
  top_n(3)%>%
  #arrange by chapter
  arrange(chapter)
common_words


common_words2 <- common_words %>%
  filter(chapter %in% c(1:15))

ggplot(common_words2, aes(word, n, fill = chapter))+ # I am making a plot that fills in the bar with the most common words
  geom_col(show.legend=FALSE)+ # this makes columns and excludes the legend
  facet_wrap(~chapter, scales="free") + # this feature tells R to make this graph for each chapter
  coord_flip()+ # flip the x and y axis
  ggtitle("Common Words Per Chapter in Order of the Phoenix")+
  ggsave('Julia/graph/HP_Q2a.png')

common_words3 <- common_words %>%
  filter(chapter %in% c(16:30))

ggplot(common_words3, aes(word, n, fill = chapter))+ # I am making a plot that fills in the bar with the most common words
  geom_col(show.legend=FALSE)+ # this makes columns and excludes the legend
  facet_wrap(~chapter, scales="free") + # this feature tells R to make this graph for each chapter
  coord_flip()+ # flip the x and y axis
  ggtitle("Common Words Per Chapter in Order of the Phoenix")+
  ggsave('Julia/graph/HP_Q2b.png')

common_words4 <- common_words %>%
  filter(chapter %in% c(31:38))

ggplot(common_words4, aes(word, n, fill = chapter))+ # I am making a plot that fills in the bar with the most common words
  geom_col(show.legend=FALSE)+ # this makes columns and excludes the legend
  facet_wrap(~chapter, scales="free") + # this feature tells R to make this graph for each chapter
  coord_flip()+ # flip the x and y axis
  ggtitle("Common Words Per Chapter in Order of the Phoenix")+
  ggsave('Julia/graph/HP_Q2c.png')
}
HP_Q2()

#Q3:Which characters appear the most 
HP_Q3<-function(){
# clean data
books = vector(mode = "list", length = 7)
for(i in 1:length(books)){
  # convert text to dataframe
  data <- data_frame(text = book_names[[i]])
  # add column for chapter numbers and title of book
  data <- mutate(data, chapter = c(1:nrow(data)), title = names(book_names)[i])
  # split by word and remove punctuation
  data <- data %>%
    # include upper case words  
    unnest_tokens(word, text, to_lower = FALSE)
  # store clean data to list
  books[[i]] <- data
}

# make one data frame from list of data frames
books <- plyr::ldply(books, data.frame)

most.char <- 
  #filter for book in the series
  filter(books, title=="order_of_the_phoenix") %>%
  select(word) %>% 
  # regular expression for character names
  filter(str_detect(word, UPPER %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR)) %>%  
  # remove words that aren't character names
  filter(!(word %in% c("The","They","What","There","It's","Then"))) %>%
  filter(word != "Harry's", word != "Professor", word != "Gryffindor") %>%
  # get frequency for most popular character names
  group_by(word) %>%
  dplyr::summarize(count=n()) %>%
  arrange(desc(count))

ggplot(most.char[1:9, ],aes(x=reorder(word,count),y=count)) + 
  geom_col(fill="cyan",color="blue") +
  xlab("Characters") +
  ggtitle("Most Popular Characters in Order of the Phoenix") +
  # Put frequency labels next to bars  
  geom_text(aes(label = paste0(count)), nudge_y = 1) +
  coord_flip()+
  ggsave('Julia/graph/HP_Q3.png')
}
HP_Q3()

#Q4:How the emotion anticipation changes throughout the book
HP_Q4<-function(){
anticipation_senti<-books %>%
  #filter for the book in the series
  filter(title == "order_of_the_phoenix") %>%
  #join using the lexicon "nrc"
  inner_join(get_sentiments("nrc"))%>%
  #group by chapter
  group_by(chapter)%>%
  #count all sentiments
  count(sentiment)

anticipation<-anticipation_senti%>%
  #ungroup by chapter
  ungroup()%>%
  #filter for anticipation
  filter(sentiment=="anticipation")%>%
  #calculate percentage of anticipation based on title
  mutate(total_anticipation=sum(n), percent=n/total_anticipation)%>%
  arrange(chapter)
anticipation

ggplot(anticipation, aes(x=chapter, y=percent))+
  geom_line(aes(fill=sentiment)) +
  ggtitle("Anticipation Per Chapter in Order of the Phoenix")+
  ggsave('Julia/graph/HP_Q4.png')
}
HP_Q4()

#Q5:What is the word count for each book?
HP_Q5<-function(){
#select cleaned data
chapter_words<-books%>%
  #filter for the book in the series
  filter(title == "order_of_the_phoenix")%>%
  #group by chapter
  group_by(chapter)%>%
  #count words
  count()

count_hp5<-chapter_words%>%
  ungroup()%>%
  #calculate total words by summing all the chapters after ungrouping
  mutate(total_words=sum(n))
  save(count_hp5, file = "Julia/data/count_hp5.Rda")
}
HP_Q5()

#Q6:All sentiments in the order of the phoenix
HP_Q6<-function(){
all_senti<-books %>%
  #filter for the book in the series
  filter(title == "order_of_the_phoenix") %>%
  #join uding the nrc lexicon
  right_join(get_sentiments("nrc")) %>%
  #filter to not include any sentiments that are N/A
  filter(!is.na(sentiment)) %>%
  #count all the sentiments present and sort by most popular
  count(sentiment, sort = TRUE)
all_senti

ggplot(all_senti, aes(sentiment, n, fill = sentiment))+ # I am making a plot that fills in the bar with the most common words
  geom_col(show.legend=TRUE)+
  ggtitle("All Sentiments in Order of the Phoenix")+
  ggsave('Julia/graph/HP_Q6.png')
}
HP_Q6()
