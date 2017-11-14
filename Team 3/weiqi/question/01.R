load("data/rawdata/HP6.Rda")

Book6Senti <- Book6 %>%
  #using "afin" to get the sentiment value of each word
  inner_join(get_sentiments("afin")) %>%
  #group by chapter
  group_by(chapter) %>%
  #calculate the sum of sentiment over every chapter
  summarise(n=sum(score))

save (Book6Senti, file = "data/modifieddata/HP6Senti.Rda")

png('graph/p1.png', width = 600, height = 300)

p1 <- ggplot(Book6Senti, aes(x=chapter, y=n))+
      geom_line(show.legend = FALSE, size = 0.4, color = "blue") +
      scale_color_brewer(palette = "Set1") +
      geom_smooth(se=FALSE, size = 0.4, linetype = 1, color = "green") +
      geom_point(color="blue", size = 1) +
      geom_hline(yintercept = 0, color = "red")

print(p1 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"), 
                 plot.title = element_text(hjust = 0.5))+
           scale_x_continuous(breaks = c(1:30)) +
           scale_y_continuous(breaks = c(seq(-250,250,by=50))) +
           ggtitle("Change in Sentiments of Half-Blood Prince") +
           labs(x="Chapter",y="Net Sentiment"))

dev.off()


