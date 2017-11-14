

load("data/cloud_book.rda")
load("data/bing_book.rda")
load("data/afinn_book.rda")
load("data/nrc_book.rda")
load("data/total_book.rda")

dir.create("graph/", showWarnings = FALSE)


##Plot The Picture of Dorian Gray Plots

sentiment_count_PDG<- data_nrc %>%
  filter(gutenberg_id == 174) %>% 
  ggplot(aes(x=sentiment, y=n, group=sentiment))+ theme_classic() + geom_label(aes(label=n, color=sentiment), show.legend = F)+ ggtitle("Sentiment Count \nThe Portrait of Dorian Grey") + ylab("Count") + xlab("")

ggsave(filename="graph/sentiment_count_PDG.png", plot=sentiment_count_PDG)


Bargraph_PDG<- data_bing %>% filter(gutenberg_id == 174, chapter>0) %>% group_by(chapter) %>% 
  count(chapter, sentiment) %>% ggplot(aes(x = chapter, y=n)) + theme_classic()+
  geom_col(aes(fill=sentiment)) + scale_fill_manual("legend", values = c("goldenrod3","cadetblue")) +  
  ggtitle ("Positive and Negative Sentiment by Chapter \nThe Portait of Dorian Gray") +ylab("Word Count")

ggsave(filename="graph/Bargraph_PDG.png", plot=Bargraph_PDG)


Sentiment_Per_Chapter_PDG<- data_bing %>% filter(gutenberg_id == 174, chapter>0) %>% group_by(chapter) %>% 
  count(chapter, sentiment) %>% ggplot(aes(chapter, n, color=sentiment)) + theme_classic()+geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) + ylab("Number")+ 
  ggtitle("Dichotomized Sentiment Per Chapter")+ expand_limits(y = 0)

ggsave(filename="graph/Sentiment_Per_Chapter_PDG.png", plot=Sentiment_Per_Chapter_PDG)

chapter_score_PDG<- data_afinn %>% filter(gutenberg_id == 174) %>% 
  ggplot(aes(chapter, chapter.score)) + theme_classic()+ geom_line(size = 1, color="cadetblue") +
  geom_smooth(method = "lm", se = FALSE, lty = 2, color="goldenrod3") + ylab("Score")+ xlab("Chapter") +
  ggtitle("Chapter Score of The Picture of Dorien Gray ") + expand_limits(y = 0)

ggsave(filename="graph/chapter_score_PDG.png", plot=chapter_score_PDG)

wordcloud_PDG<- wordcloud(words= data_cloud$word[data_cloud$gutenberg_id==174], freq = data_cloud$n[data_cloud$gutenberg_id==174], min.freq = 7, max.words=200, random.order=F, rot.per=0.4,colors=brewer.pal(8, "Dark2"))

ggsave(filename="graph/wordcloud_PDG.png", plot=wordcloud_PDG)

Total_comparison<- data_total %>% arrange(desc(total)) %>% ggplot(aes(x=book,y=total,fill=book)) + theme_classic() +geom_bar(stat = "identity", show.legend = FALSE)  + xlab("")+ ylab("Score") + 
  ggtitle("Total Score Book Comparison") +coord_flip()

ggsave(filename="graph/Total_comparison.png", plot=Total_comparison)





