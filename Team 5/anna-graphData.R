# Load all relevant data
load("data/nyt_sentimentScores")
load("data/nyt_intensityScores")
load("data/nyt_yearSentiment")
load("data/nyt_yearIntensity")

# plot mean sentiment scores for each title 

ggplot(nyt.sent,aes(title.score)) + 
  labs(x = "Mean Score") +
  geom_histogram(color="black",fill="lightblue1",aes(y=..density..), binwidth=1) +
  geom_density() + 
  scale_x_continuous(breaks = pretty(nyt.sent$title.score, n = 10)) +
  ggtitle("NY Times Title Mean Sentiment Scores Distribution") +
  theme(plot.title = element_text(color="black", face="bold", size=18,hjust = 0.5)) +
  #theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

#SAVE
ggsave("anna_graphs/title.sent_graph.png")

ggplot(nyt.intensity,aes(mean.intensity)) + 
  # label the x-axis 
  labs(x = "Intensity Score") +
  # create histogram with a density curve
  geom_histogram(color="black",fill="indianred2",aes(y=..density..), binwidth=0.5) +
  geom_density() + 
  # label the x-axis tick marks in a way that's intuitive
  scale_x_continuous(breaks = pretty(nyt.intensity$mean.intensity, n = 10)) +
  # make the title nice
  ggtitle("NY Times Title Mean Intensity Scores Distribution") +
  theme(plot.title = element_text(color="black", face="bold", size=18,hjust = 0.5)) +
  # get the background to be white and get rid of the grid marks
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.background = element_rect(fill='white')) +
  # make the axis tick marks and text look nicer 
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

#SAVE
ggsave("anna_graphs/title.intensity_graph.png")

####### Plot Mean Sentiment Scores
# This plots the sentiment scores
ggplot(data=year.sent, aes(x=year.num,y=year.sent)) +
  geom_line(color="steelblue4", linetype="dashed") +
  labs(x = "Year", y = "Mean Score") +
  scale_x_continuous(breaks = pretty(year.sent$year.num, n = 10),labels=c(1996:2006)) +
  scale_y_continuous(breaks = pretty(year.sent$year.sent, n = 8)) +
  geom_point(size=3,color="steelblue4") +
  ggtitle("NY Times Mean Title Sentiment Scores Over Time") +
  theme(plot.title = element_text(color="black", face="bold", size=18,hjust = 0.5)) +
  #theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

#SAVE
ggsave("anna_graphs/time.sent_graph.png")

# This plots the intensity scores
intensity.time_graph = ggplot(data=year.intensity, aes(x=year.num,y=intensity)) +
  geom_line(color="firebrick3", linetype="dashed") +
  labs(x = "Year", y = "Mean Intensity") +
  scale_x_continuous(breaks = pretty(year.intensity$year.num, n = 10),labels=c(1996:2006)) +
  scale_y_continuous(breaks = pretty(year.intensity$intensity, n = 8)) +
  geom_point(size=3,color="firebrick3") +
  ggtitle("NY Times Mean Title Sentiment Intensity Over Time") +
  theme(plot.title = element_text(color="black", face="bold", size=18,hjust = 0.5)) +
  #theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

#SAVE
ggsave("anna_graphs/time.intensity_graph.png")