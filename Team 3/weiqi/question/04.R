load("data/rawdata/HP6.Rda")
Book6Anti <- Book6 %>% 
  inner_join(get_sentiments("nrc"))  %>%
  group_by(chapter) %>%
  count (sentiment) %>%
  filter(sentiment == "anticipation") 


png('graph/p3.png', width = 800, height = 500)

p3 <- ggplot(Book6Anti, aes(x=chapter, y=n)) +
      geom_col(show.legend = FALSE, fill = "blue", width  = 0.5) +
      geom_smooth(se=FALSE, size = 0.6, linetype = 1, color = "red")

print(p3 + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"), 
                 plot.title = element_text(hjust = 0.5) ) + 
           scale_x_continuous(limits = c(0,31),
                              expand = c(0,0),
                              breaks = c(1:30)) +
           scale_y_continuous(limits = c(0,200),
                              expand = c(0,0),
                              breaks = c(seq(0,200,by=20))) +
           ggtitle("Change of Emtion Anticipation in Half-Blood Prince") +
           labs(x="Chapter",y="Count of Anticipation"))


dev.off()


