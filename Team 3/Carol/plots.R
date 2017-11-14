
library(ggplot2)

dir.create("Carol/graph/", showWarnings = FALSE)

load("Carol/cleandata/q1.Rda")
#plot positives versus negatives
ggplot(q1, aes(x = chapter, y = percent, fill=sentiment)) +geom_bar(stat='identity', color='black')+
  guides(fill=guide_legend(reverse=T)) + #add legend
  scale_fill_brewer(palette='Blues')+ #change colors of the bars
  ggtitle("Sentiments Per Chapter")+
  ylab("Percentage") + 
  xlab("Chapter")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Carol/graph/plot1.png')

load("Carol/cleandata/q2.Rda")
#plot most common words
ggplot(q2, aes(x=percent, y=reorder(word, percent)))+
  geom_point(size=1, aes(color=factor(chapter)))+ #dot plot
  theme_classic()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color='grey', linetype = "dashed"), 
        plot.title = element_text(hjust = 0.5))+#change major/minor presentation
  ggtitle("Most Common Words In Goblet Of Fire")+
  ylab("Words") + 
  xlab("Mentioned By Percentage of Each Chapter")
ggsave('Carol/graph/plot2.png')

load("Carol/cleandata/q3.Rda")
#plot percentages
ggplot(q3, aes(x=percent, y=reorder(word, percent)))+
  geom_point(size=2)+ #dot plot
  theme_classic()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color='grey', linetype = "dashed"), 
        plot.title = element_text(hjust = 0.5))+ #change major/minor presentation
  ggtitle("Most Appeared Charcters")+
  ylab("Characters Names") + 
  xlab("Mentioned By Percentage of The Book")
ggsave('Carol/graph/plot3.png')

load("Carol/cleandata/q4.Rda")
#plot points
ggplot(q4, aes(x=chapter, y=n, label=n))+
  geom_line(colour="dark blue")+ #add line graph
  geom_point(size=1, shape=21, fill='white')+ #make points white
  stat_smooth(method=loess, se=F, colour="grey")+
  geom_text(aes(y=y_label, label=n), vjust=1.5, color='black')+   ggtitle("Changes in Anticipation")+
  ylab("Anticipation") + 
  xlab("Chapter")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('Carol/graph/plot4.png')

load("Carol/cleandata/q5.Rda")
#plot points
ggplot(q5, aes(x=Title, y=word_count))+
  geom_bar(stat='identity', aes(fill=Title))+ 
  ggtitle("Number Of Words Per Book")+
  ylab("Number of Words") + 
  xlab("Book")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip()
ggsave('Carol/graph/plot5.png')

load("Carol/cleandata/q6.Rda")
#plot beasts mentioned, stacked by title
ggplot(q6, aes(x = beasts, y = n)) +
  geom_bar(stat='identity', aes(fill=Title))+ #stacked bars by title
  theme(axis.text.x = element_text(size=.00005)) + #reformat the size of text for x-axis
  ggtitle("Fantastic Beasts in Harry Potter Series")+
  ylab("Number of Times Mentioned") + 
  xlab("Beasts")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() #flip bars
ggsave('Carol/graph/plot6.png')