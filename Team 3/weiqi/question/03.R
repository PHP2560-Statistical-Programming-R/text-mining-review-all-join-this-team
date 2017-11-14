load("data/rawdata/HP6.Rda")

Book6Char <- Book6  %>%
  group_by(word) %>%
  select(word) %>%
  filter(word == "harry" | 
         word == "ron" | 
         word == "hermione" | 
         word == "dumbledore" |
         word == "voldemort" | 
         word == "dobby" |
         word == "snape" |
         word == "draco" |
         word == "ginny" |
         word == "hagrid") %>%
  count (word, sort = TRUE) %>%
  arrange (desc(n))

save(Book6, file = "data/modifieddata/HPChar.Rda")


png('graph/p2.png', width = 800, height = 500)

p2 <- ggplot(Book6Char, aes(x=word, y=n)) +
            geom_bar(stat="identity", fill = "dodgerblue") +
            geom_text(aes(label=n), vjust= -1) +
            scale_y_continuous(expand = c(0,0), 
                               limits = c (0,3000), 
                               breaks = seq(0, 3000, by = 200)) +
            ggtitle("The Count of Main Characters") +
            labs(x="character", y="count(n)") 

print (p2 + theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  plot.title = element_text(hjust = 0.5)) )


dev.off()


