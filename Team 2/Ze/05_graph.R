load("data/bing_books.Rda")
load("data/afinn_books.Rda")
load("data/nrc_books.Rda")
load("data/tidy_books.Rda")


#Tables
#Find top10 negative words in each book
Bible_t1<- data_bing %>% filter(gutenberg_id == 10) %>% filter(sentiment=="negative") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)
Buddhism_t1<- data_bing %>% filter(gutenberg_id == 18223) %>% filter(sentiment=="negative") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)
Koran_t1<- data_bing %>% filter(gutenberg_id == 2800) %>% filter(sentiment=="negative") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)
#Find top10 positive words in each book
Bible_t2<- data_bing %>% filter(gutenberg_id == 10) %>% filter(sentiment=="positive") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)
Buddhism_t2<- data_bing %>% filter(gutenberg_id == 18223) %>% filter(sentiment=="positive") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)
Koran_t2<- data_bing %>% filter(gutenberg_id == 2800) %>% filter(sentiment=="positive") %>% group_by(word) %>% count() %>% arrange(desc(n)) %>% rename(frequency=n) %>% head(10)


dir.create("graph/", showWarnings = FALSE)
#Graph1
#Sentiment words distribution for each book
#Get the proportion distribution table for Bible
Bible_prop<-data_nrc %>% filter(gutenberg_id == 10) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n)) 
#Creat Barplot 
g1_Bi<-ggplot(Bible_prop, aes(x=sentiment,y=proportion,fill=sentiment)) + geom_bar(stat = "identity",show.legend = F)  + xlab("Sentiment Words") + ylab("Proportion") + ylim(0,0.25) + ggtitle("Sentiment words Distribution for Bible")

#Get the proportion distribution table for Buddhism                                                                 
Buddhism_prop<-data_nrc %>% filter(gutenberg_id == 18223) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n)) 
#Creat Barplot 
g1_Bu<-ggplot(Buddhism_prop,aes(x=sentiment,y=proportion,fill=sentiment)) + geom_bar(stat = "identity",show.legend = F) + xlab("Sentiment Words") + ylab("Proportion") + ylim(0,0.25) + ggtitle("Sentiment words Distribution for Buddhism")

#Get the proportion distribution table for Koran
Koran_prop<-data_nrc %>% filter(gutenberg_id == 2800) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n))
g1_Ko<-ggplot(Koran_prop,aes(x=sentiment,y=proportion,fill=sentiment)) + geom_bar(stat = "identity",show.legend = F) + xlab("Sentiment Words") + ylab("Proportion") + ylim(0,0.25) + ggtitle("Sentiment words Distribution for Koran")
#Combine barplots for comparison
Graph1<-grid.arrange(g1_Bi,g1_Bu,g1_Ko,ncol=2)
ggsave(filename="graph/Graph1.png", plot=Graph1, width = 12, height = 12)

#Graph2
#Find positive/negative proportions for each book
#positive/negative proportion for Bible
Bible_pn<-data_bing %>% filter(gutenberg_id == 10) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n)) %>% select(sentiment,proportion)
#positive/negative proportion for Buddhism
Buddhism_pn<-data_bing %>% filter(gutenberg_id == 18223) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n)) %>% select(sentiment,proportion)
#positive/negative proportion for Koran
Koran_pn<-data_bing %>% filter(gutenberg_id == 2800) %>% group_by(sentiment) %>% count() %>% ungroup() %>% mutate(proportion=n/sum(n)) %>% select(sentiment,proportion)
#Combine all books
all_book<-inner_join(Bible_pn,Buddhism_pn,by="sentiment") %>% inner_join(Koran_pn,by="sentiment") %>% rename(Bible=proportion.x, Buddhism=proportion.y,Koran=proportion) %>% 
  #Modify the table to the form that applicable to ggplot
  gather("book","proportion",2:4)
#Create barplot
Graph2<-ggplot(all_book,aes(x=book,y=proportion,fill=sentiment))+geom_bar(stat = "identity") 
ggsave(filename="graph/Graph2.png", plot=Graph2)

#Graph3
#Sentiment score per word for each book
data_tidy <- do.call("rbind", tidy_books)
Bible_score<- data_afinn %>% filter(gutenberg_id == 10) %>% summarize(score_per_word=sum(score)/nrow(filter(data_tidy,gutenberg_id == 10)))
Buddhism_score<-data_afinn %>% filter(gutenberg_id == 18223) %>% summarize(score_per_word=sum(score)/nrow(filter(data_tidy,gutenberg_id == 18223)))
Koran_score<-data_afinn %>% filter(gutenberg_id == 2800) %>% summarize(score_per_word=sum(score)/nrow(filter(data_tidy,gutenberg_id == 2800)))
score<-rbind(Bible_score,Buddhism_score,Koran_score) %>% mutate(book=c("Bible","Buddhism","Koran"))
#Create Barplot
Graph3<-ggplot(score,aes(x=book,y=score_per_word,fill=book)) + geom_bar(stat = "identity", show.legend = FALSE) + xlab("Book") + ylab("Score")
ggsave(filename="graph/Graph3.png", plot=Graph3)

#Graph4 and Graph5
#Combine all books from the group
#Score per word for CML's book
Dorien.score<-data_afinn %>% filter(gutenberg_id == 174) %>% summarize(score_per_word=sum(score)/nrow(filter(data_tidy,gutenberg_id == 174)))
#Total score for CML's book
Dorien.score_t<-data_afinn %>% filter(gutenberg_id == 174) %>% summarize(score_t=sum(score))
#Score per word for Sadia's book
alice_score<-data_afinn %>% filter(gutenberg_id == 751) %>% summarize(score_per_word=sum(score)/nrow(filter(data_tidy,gutenberg_id == 751)))
#Total score for Sadia's book
alice_score_t<-data_afinn %>% filter(gutenberg_id == 751) %>% summarize(score_t=sum(score))
#Total scores for my books
Bible_score_t<-data_afinn %>% filter(gutenberg_id == 10) %>% summarize(score_t=sum(score))
Buddhism_score_t<-data_afinn %>% filter(gutenberg_id == 18223) %>% summarize(score_t=sum(score))
Koran_score_t<-data_afinn %>% filter(gutenberg_id == 2800) %>% summarize(score_t=sum(score))
#Total sentiment score for all books comparison
score_all_t<-rbind(Bible_score_t,Buddhism_score_t,Koran_score_t,Dorien.score_t,alice_score_t) %>% mutate(book=c("Bible","The Essence of Buddhism","Koran", "Picture of Dorian Gray", "Alice's Adventures in Wonderland"))
Graph4<-ggplot(score_all_t,aes(x=reorder(book,score_t),y=score_t,fill=book)) + theme_classic() + geom_bar(stat = "identity", show.legend = FALSE) + xlab("") + ylab("Score") + ggtitle("Total Sentiment score for each book") + coord_flip()
#Sentiment score per word for all books comparison
score_all<-rbind(Bible_score,Buddhism_score,Koran_score,Dorien.score,alice_score) %>% mutate(book=c("Bible","The Essence of Buddhism","Koran", "Picture of Dorian Gray", "Alice's Adventures in Wonderland"))
Graph5<-ggplot(score_all,aes(x=reorder(book,score_per_word),y=score_per_word,fill=book)) + theme_classic() + geom_bar(stat = "identity", show.legend = FALSE) + xlab("") + ylab("Score") + ggtitle("Sentiment score per word") + coord_flip()
ggsave(filename="graph/Graph4.png", plot=Graph4)
ggsave(filename="graph/Graph5.png", plot=Graph5)

#Graph6 Graph7 Graph8
library(wordcloud)
#Wordcloud of most common words of the book
#wordcloud for Bible
cloud_Bi<-data_bing %>% filter(gutenberg_id == 10) %>% count(word, sentiment) %>% arrange(desc(n)) 
png("graph/Graph6.png")
wordcloud(words = cloud_Bi$word, freq = cloud_Bi$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
png("graph/wordcloud.png")
#wordcloud for Buddhism
cloud_Bu<-data_bing %>% filter(gutenberg_id == 18223) %>% count(word, sentiment) %>% arrange(desc(n)) 
png("graph/Graph7.png")
wordcloud(words = cloud_Bu$word, freq = cloud_Bu$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
#wordcloud for Koran
cloud_Ko<-data_bing %>% filter(gutenberg_id == 2800) %>% count(word, sentiment) %>% arrange(desc(n)) 
png("graph/Graph8.png")
wordcloud(words = cloud_Ko$word, freq = cloud_Ko$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()



