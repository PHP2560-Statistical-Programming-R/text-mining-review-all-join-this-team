
#Packages

library(twitteR)
library(ROAuth)
library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(stringr)
library(sentimentr)
library(stm)
library(geometry)
library(rsvd)
library(Rtsne)
library(ggplot2)

clean.beer <- read_csv("AllFormattedTweets.csv")

#just want specific data as meta data --> data that would serve as a covariate: beer company and date 

beer.data <- clean.beer %>% 
  select(X1, text, screenName, weekday)

#process the data via STM textProcessor 
proc.beer <- textProcessor(documents = beer.data$text, metadata = beer.data)

out.put <- prepDocuments (proc.beer$documents, proc.beer$vocab, proc.beer$meta)
prep.docs <- out.put$documents 
prep.vocab <- out.put$vocab
prep.meta <- out.put$meta

#need to instal Rtsne, rsvd, and geometry

#from here, we can run the stm function for topic modelling 
beerModelFit <- stm(documents = prep.docs,
                    vocab = prep.vocab,
                    K =  40, #we can set k to pretty much anything 
                    prevalence =~ screenName + weekday,
                    data = prep.meta,
                    init.type = "Spectral",
                    seed=5674309) 

#Time to visualize the topics and see how much weekday influences the data. 

beertheta <- as.data.frame(beerModelFit$theta)

beertheta$topic <- apply(beertheta[,1:40], 1, which.max)
beertheta$topic.value <- apply(beertheta[, 1:40], 1, max)
beertheta$X1 <- 1:nrow(beertheta)

#take only variables of interest 
theta.clean <- beertheta %>% 
  select(X1, topic, topic.value)

#prepDoc deleted lines in beermeta, therefore X1 is not numbered correctly, have to delete and renumber, the rows 

beermeta <- within(prep.meta, rm(X1))

beermeta$X1 <- 1:nrow(prep.meta)

beermeta[8,] #X1 = 8, while row num = 9, but it is displayed correctly, X1 matches order correctly

#now that our beermeta's X1 is labeled correctly and corresponds with the actual number of the document, we can join the two datasets using X1 

beer.final.DOW <- prep.meta %>%
  left_join(theta.clean, by = "X1")


#Data visualization

brand.topic.freq <- beer.final.DOW %>% 
  select(screenName, topic) %>%
  group_by(screenName) %>% 
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) 

brand.topic.freq


#creating a data frame for each day of the week for blue moon to see what the topics are for day of the week for one company.

DOW.BM.M <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Mon") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.M 

DOW.BM.T <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Tues") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.T

DOW.BM.W <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Wed") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.W

DOW.BM.THUR <- 
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Thurs") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.THUR

DOW.BM.FRI <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Fri") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.FRI

DOW.BM.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.SAT

DOW.BM.SUN <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sun") %>%
  filter(screenName == "BlueMoonBrewCo") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BM.SUN

#After looking at the days of the week, top five topics, blue moon generally tweets about the same 5 topics every day of the week. Since we are going to get a lot of bar plots with the same bar plots, we are going to compare the topics across our 5 companies on one day of the week. We will choose Saturday night.

#Creating a data frame just for saturday for each company to prep data for visualization

DOW.DE.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "DosEquis") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.DE.SAT


DOW.BL.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "budlight") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.BL.SAT

DOW.TS.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "tsingtao") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.TS.SAT

DOW.GUS.SAT <-
  beer.final.DOW %>% 
  select(screenName, topic, weekday) %>%
  group_by(screenName, weekday) %>% 
  filter(weekday=="Sat") %>%
  filter(screenName == "GuinnessUS") %>%
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(5)

DOW.GUS.SAT

#Barpolts of the top 5 most frequently tweeted topics on Saturdays by Company Name
#ggplot

dir.create("graph/",showWarnings = F)

dev.copy(png,'graph/Blue Moon Beer Top 5 Topics on Sat Final.png')
v = data.frame(topic=c('23','35','14','26','5'), n=c(89,69,50,26,24))
barplot(v$n, names.arg=v$topic, main = "Top 5 Tweeted topics on Saturdays by Blue Moon Beer Co.", ylim = c(0,100),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))
dev.off()


dev.copy(png, 'graph/Dos Equis Top 5 Topics on Sat Final.png')
w = data.frame(topic=c('37','28','36','31','2'),n=c(54,16,11,8,5))
barplot(w$n, names.arg=w$topic, main = "Top 5 Tweeted topics on Saturdays by Dos Equis Beer Co.", ylim = c(0,60),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))

dev.off()


dev.copy(png,'graph/Budlight Top 5 Topics on Sat Final.png')
x = data.frame(topic=c('33','27','10','37','32'),n=c(111,37,31,22,19))
barplot(x$n, names.arg=x$topic, main = "Top 5 Tweeted topics on Saturdays by Budlight Beer Co.", ylim = c(0,120),ylab = "Count", xlab = "Topic",col=c("burlywood1","coral"))

dev.off()

dev.copy(png,'graph/Tsingtao Top 5 Topics on Sat Final.png')
y = data.frame(topic=c('20','19','38','3', '16'),n=c(8,3,3,1,1))
barplot(y$n, names.arg=y$topic, main = "Top 5 Tweeted topics on Saturdays by Tsingtao Beer Co.", ylim = c(0,10),ylab = "Count", xlab = "Topic",col=c("burlywood1","coral") )

dev.off()


dev.copy(png,'graph/Guinness US Top 5 Topics on Sat Final.png')

z = data.frame(topic=c('11','5','25','29','34'),n=c(114,7,3,3,3))
barplot(z$n, names.arg=z$topic, main = "Top 5 Tweeted topics on Saturdays by Guinness US Beer Co.", ylim = c(0,120),ylab = "Count", xlab = "Topic", col=c("burlywood1","coral"))

dev.off()
