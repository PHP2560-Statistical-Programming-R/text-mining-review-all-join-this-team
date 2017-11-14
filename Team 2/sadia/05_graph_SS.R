
load("sadia/data_SS/data_bing.Rda")
load("sadia/data_SS/data_afinn.Rda")
dir.create("sadia/graph_SS/", showWarnings = FALSE)


##Plot of most frequent words in each chapter:
frq_words_chapter<- data_bing %>%
  filter(gutenberg_id == 751) %>% 
  group_by(chapter) %>%
  count(word) %>%
  # Take the top 5 words for each chapter
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, chapter, sep = "__"), n)) %>%
  ggplot(aes(word, n, fill=chapter)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ chapter, scales = "free") +
  coord_flip() +
  labs(x="", y="Frequency of Words") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 7)) 
  
ggsave(filename="sadia/graph_SS/01_frq_words_chapter.png", plot=frq_words_chapter, height = 4, width = 4)

#frequency distribution of positive and negative words by each chapter
pos_neg_chapter <- data_bing %>% filter(gutenberg_id == 751 & chapter>0) %>% group_by(chapter) %>% count(chapter, sentiment) %>% rename(total_words=n) %>%
  #Pyramid plot of each chapter by positive and negative words (word frequency by each chapter):
  ggplot(aes(x = as.factor(chapter), fill = sentiment)) + 
  geom_bar(data=filter(data_bing, gutenberg_id == 751 & chapter >0 & sentiment == "positive")) + 
  geom_bar(data=filter(data_bing, gutenberg_id == 751 & chapter >0 & sentiment == "negative"),aes(y=..count..*(-1))) + #aes option aligns the plot to 0 and counts on both side of x axis from 0 to positive
  scale_y_continuous(breaks=seq(-350,350,50),labels=abs(seq(-350,350,50))) + 
  coord_flip() +
  labs(x="Chapter", y="Frequency") +
  theme(text = element_text(size = 5)) +
  scale_fill_brewer(palette = "Set1")

ggsave(filename="sadia/graph_SS/02_pos_neg_chapter.png", plot=pos_neg_chapter, height = 2, width = 4)


#Percentage of positive and negative sentiments throughout the chapter
percent_pos_neg<-data_bing %>% filter(gutenberg_id == 751 & chapter>0) %>%
  group_by(chapter, sentiment) %>% mutate(total_words=n()) %>% distinct(chapter, .keep_all=TRUE) %>% 
  ungroup() %>% group_by(chapter) %>% mutate(total = sum(total_words), percent = total_words/total) %>%
  ggplot(aes(chapter, percent, color=sentiment)) +
  geom_line(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5))
# Throughout the chapters positve emotions increasing and negatives are decreasing and it looks like positive words had an abrupt increase at the end of the story.

ggsave(filename="sadia/graph_SS/03_percent_pos_neg.png", plot=percent_pos_neg, height = 2, width = 5)


#Scatterplot of sentiment throughout the sections (made-up sections where each section have 50 lines)
alice_section <- data_bing %>% filter(gutenberg_id == 751 & chapter>0) %>%
  count(chapter, section = linenumber %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 

p1 <- ggplot(alice_section, aes(section, sentiment, color=sentiment)) +
  geom_smooth(aes(group=1), method="lm", se=FALSE, color="red") +
  geom_point(shape=1, size=1) +
  scale_colour_gradientn(colours=rainbow(7), name="Sentiment Score by \n bing lexicon") +
  theme(text = element_text(color="gray20", size=5))

poinstToLabel<-c("2", "3", "4", "6", "17", "21", "60", "37", "13", "76","169", "170", "139", "184", "7", "8", "24", "27", "86", "46", "68", "141", "142", "79")

p2 <-p1 + geom_text(aes(label=section), color="gray20", 
                   data= subset(alice_section, section %in% poinstToLabel),
                   hjust=1.5, size=2) +   
        labs(x="Section of Alice's Adventure in Wonderland", y="Sentiment score") +
        theme_light() +
        theme(text = element_text(color="gray20", size=5),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title.align = 0.5,
        legend.key.height=unit(0.5,"line"),
        axis.text = element_text(face="italic")) 

ggsave(filename="sadia/graph_SS/04_sentiment_per_section.png", plot=p2, height = 2.5, width = 4)

#Wordcloud of most common words of the book
png("sadia/graph_SS/05_wc.png")

cloud <- data_bing %>% filter(gutenberg_id == 751 & chapter>0) %>% count(word, sentiment) %>% arrange(desc(n)) #data frame for wordcloud
wc<- wordcloud(words = cloud$word, freq = cloud$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 

dev.off()


#comparison of books
#Change of sentiments throughout the sections in all 5 books
# sentiment in the books throughout the sections (assuming a section has on average 50 lines)

label <- c("751" = " Alice's adventure in wonderland", 
                 "174" = "Picture of Dorian Gray", "2800" = "Quran", 
                 "10" = "Bible", "18223" = "The Essence of Buddhism")

sentiment_per_book <- data_bing %>%
  count(gutenberg_id, index = linenumber %/% 50, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(index, sentiment, fill=sentiment)) +
  geom_col() +
  # Separating panels with facet_wrap()
  facet_wrap(~ gutenberg_id, scales = "free_x", labeller=labeller(gutenberg_id = label)) +
  scale_fill_gradient(low="blue", high="red", name="overall sentiment=\npositive-negative") +
  labs(x="Sections of the book (50 lines per section)", y="Overall sentiment") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 5),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height=unit(0.5,"line"),
        strip.text.x = element_text(size = 5, colour = "darkblue"))

ggsave(filename="sadia/graph_SS/06_sentiments_per_book.png", plot=sentiment_per_book, height = 2.5, width = 4)


#Total Sentiment score and score per word of the books
#sentiment score and comparing with other books
total<-function(df) { #function to calculate the total sentiment score with "afinn" lexicon
  df %>% inner_join(get_sentiments("afinn")) %>% summarise(total= sum(score))
} 
score<-function(df){ #function to calculate the sentiment score per word with "afinn" lexicon
  df %>% inner_join(get_sentiments("afinn")) %>% mutate(total=sum(score)) %>% summarize(score_per_word=sum(score)/nrow(df))
}
alice_total<-total(data_bing %>% filter(gutenberg_id == 751 & chapter>0))
alice_score<-score(data_bing %>% filter(gutenberg_id == 751 & chapter>0))

Bible_total<-total(data_bing %>% filter(gutenberg_id == 10 )) 
Bible_score<-score(data_bing %>% filter(gutenberg_id == 10))

Buddhism_total<-total(data_bing %>% filter(gutenberg_id == 18223 )) 
Buddhism_score<-score(data_bing %>% filter(gutenberg_id == 18223 ))

Quran_total<-total(data_bing %>% filter(gutenberg_id == 2800 )) 
Quran_score<- score(data_bing %>% filter(gutenberg_id == 2800 )) 

Picture_total<-total(data_bing %>% filter(gutenberg_id == 174 & chapter>0)) 
Picture_score<- score(data_bing %>% filter(gutenberg_id == 174 & chapter>0))


score<-bind_rows(alice_score, Bible_score,Buddhism_score, Quran_score, Picture_score) %>%   mutate(book=c("Alice's Adventures in Wonderland","Bible","The Essence of Buddhism","Quran", "Picture of Dorian Gray")) 
total<-bind_rows(alice_total, Bible_total, Buddhism_total, Quran_total, Picture_total) %>% mutate(book=c("Bible","The Essence of Buddhism","Quran", "Picture of Dorian Gray", "Alice's Adventures in Wonderland")) 

#plot
score_per_word<-ggplot(score, aes(x=reorder(book, score_per_word) ,y=score_per_word,fill=book)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  xlab("") + 
  ylab("Score per word") + 
  ggtitle("Sentiment score per word") +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 6),
        axis.text.y  = element_text(size=7))

total_score<-ggplot(total,aes(x=reorder(book, total),y=total,fill=book)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  xlab("") + 
  ylab("Total score") + 
  ggtitle("Total sentiment score of each book") +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 6),
        axis.text.y  = element_text(size=7))

plots<- grid.arrange(total_score, score_per_word, ncol=2)

ggsave(filename="sadia/graph_SS/07_comparison_other_book.png", 
       plot=plots, height = 2.5, width = 10)







