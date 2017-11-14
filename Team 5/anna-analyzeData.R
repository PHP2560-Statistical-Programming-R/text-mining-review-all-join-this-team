load("data/nyt_data.rda")
source("check_packages.R")
check_packages(c("tidytext","stringr","dplyr","rebus","ggplot2")) #loads packages

############## PART 1 ##############

####### SENTIMENTS #######

# Get dictionary with sentiment scores using lexicon afinn 
sentiment.scores = as.data.frame(get_sentiments("afinn"))

# Prepare the titles as strings by converting title to characters
NYTimes$title = (as.character(NYTimes$Title))

# Create a data frame that will contain the sentiment scores for each word in the title
nyt.sent = NYTimes %>%
  # group the data by title
  group_by(Title) %>%
  # get each word from the title out by unnesting
  unnest_tokens(output=word, input=title) %>%
  # get the sentiment scores for each word in the title 
  inner_join(sentiment.scores) %>%
  # get a mean of the score for each word in the title to get an average sentiment score for each title 
  summarize(title.score = mean(score))

####### INTENSITY #######

# Get absolute value
nyt.sent = nyt.sent %>%
  mutate(intensity = abs(title.score))

# Get mean intensity

nyt.intensity = nyt.sent %>%
  group_by(Title) %>%
  summarize(mean.intensity = mean(intensity))

############## PART 2 ##############

####### PREPARE DATE/YEARS #######

#1. Convert Dates to Strings

# make the date as characters
NYTimes$Date = as.character(NYTimes$Date)

# now convert the date to strings
for (i in 1:length(NYTimes$Date)){
  NYTimes$Date[i] = toString(NYTimes$Date[i])
}

#2. Extract the Year
# create a pattern for how the year will look like
year_pattern = DGT %R% DGT %R% END
# get the year using this pattern and put this year in our data frame with scores
NYTimes$year = str_extract(NYTimes$Date, pattern=year_pattern)

####### SENTIMENT #######

# Get sentiment scores for titles
year.sent = NYTimes %>%
  # group the data by title
  group_by(Title, year) %>%
  # get each word from the title out by unnesting
  unnest_tokens(output=word, input=title) %>%
  # get the sentiment scores for each word in the title 
  inner_join(sentiment.scores) %>%
  # get a mean of the score for each word in the title to get an average sentiment score for each title 
  summarize(title = mean(score))

# Now, get mean sentiment scores for years
mean.year.sent = year.sent %>%
  group_by(year) %>%
  summarize(year.sent = mean(title))

####### INTENSITY #######

# create dataframe containing the intensity
year.intensity = mean.year.sent %>%
  mutate(intensity = abs(year.sent))

####### CLEAN YEARS #######

# change years to numeric now, first in the year.sent dataframe
mean.year.sent$year = as.numeric(mean.year.sent$year)

# and make it go from year 1 to year 10 (with 1996 as year 1)
mean.year.sent = mean.year.sent %>%
  mutate(year.num = ifelse(year<=6, 10-year, abs(96-year)))

# now also save these year numbers into year.intensity dataframe
year.intensity$year.num = mean.year.sent$year.num


############## GRAPH EVERYTHING ##############

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
ggplot(data=mean.year.sent, aes(x=year.num,y=year.sent)) +
  geom_line(color="steelblue4", linetype="dashed") +
  labs(x = "Year", y = "Mean Score") +
  scale_x_continuous(breaks = pretty(mean.year.sent$year.num, n = 10),labels=c(1996:2006)) +
  scale_y_continuous(breaks = pretty(mean.year.sent$year.score, n = 8)) +
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
  scale_y_continuous(breaks = pretty(year.intensity$year.intensity, n = 8)) +
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