clean.beer <- read_csv("AllFormattedTweets.csv")

#just want specific data as meta data --> data that would serve as a covariate: beer company and date 

beer.data <- clean.beer %>% 
  select(X1, reformattedtext, screenName, weekday, favoriteCount)

#process the data via STM textProcessor 
proc.beer <- textProcessor(documents = beer.data$reformattedtext, metadata = beer.data)

out <- prepDocuments(proc.beer$documents, proc.beer$vocab, proc.beer$meta)
beerdocs <- out$documents 
beervocab <- out$vocab
beermeta <- out$meta

#tested various models, searchK, and K=53 is decent with the new reformatted tweets 

BeerModelFormatted <- readRDS("BeerModelFormatted.rds")

thoughts <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(48), n=3)

plot(thoughts)

#check to see if its pulling up the right documents
plotQuote(thoughts)
beermeta$reformattedtext[c(8923,7997,7931)] #looks good! 

#combine metadata file with new metadata: "theta"
#what does it look like? 
theta <- as.data.frame(BeerModelFormatted$theta)
head(theta, n=2)

#add in function that take the column name with the highest value, and add it to a variable we call "topic" and then create a varaible that gives us the actual value, and then we need to create a variable "X1" so we can do a join 

theta$topic <- apply(theta[,1:49], 1, which.max)
theta$topic.value <- apply(theta[, 1:49], 1, max)
theta$X1 <- 1:nrow(theta)

#take only variables of interest 
theta.clean <- theta %>% 
  select(X1, topic, topic.value)

#prepDoc deleted lines in beermeta, therefore X1 is not numbered correctly, have to delete and renumber, the rows 

beermeta <- within(beermeta, rm(X1))

beermeta$X1 <- 1:nrow(beermeta)

beermeta[8,] #X1 = 8, while row num = 9, but it is displayed correctly, X1 matches order correctly

#now that our beermeta's X1 is labeled correctly and corresponds with the actual number of the document, we can join the two datasets using X1 

beer.final <- beermeta %>%
  left_join(theta.clean, by = "X1")

#let's figure out what the most popular topic is, and who tweets about them the most 
company.topic.freq <- beer.final %>% 
  select(screenName, topic) %>%
  group_by(screenName) %>% 
  count(topic) %>%
  ungroup() %>%
  arrange(desc(n)) 

company.topic.freq

#we can check/visualize this by creating a bar chart for each topic 
dir.create("graph/",showWarnings = F)
png('graph/topicfreq.png')

counts <- table(beer.final$topic)
barplot(counts, main = "Frequency of Topics", xlab = "Topic Number")
dev.off()

#now let's see which topic is favorited the most, and which company is it coming from 
favorite.topic.freq <- beer.final %>% 
  select(screenName, topic, favoriteCount) %>%
  group_by(screenName, topic) %>% 
  summarise(favorites = sum(favoriteCount)) %>% 
  ungroup() %>%
  arrange(desc(favorites))

favorite.topic.freq

#just for fun let's see which company has the most favorites, along with how many tweets they have 
most.fav.company <- beer.final %>% 
  select(screenName, favoriteCount) %>%
  group_by(screenName) %>%
  summarise(favorited = sum(favoriteCount),
            ntweets = n(),
            ratio = favorited/ntweets) %>%
  arrange(desc(favorited))

most.fav.company 

#okay, lets take the 3 most favorited topics for each company, time to make a function! 
header <- function(data, name) {
  x1 <- data %>%
    filter(screenName == name)
  return(head(x1, n=3))
}

header(favorite.topic.freq, "DosEquis")
#Bud: 24,19,20
#tsingtao: 22,42,33
#Guinness: 31,27,7
#BlueMoon: 35, 1, 41
#DosEquis: 38, 6, 34



#most tweeted topic by company? 
header(company.topic.freq, "DosEquis")
#Bud: 24,20,28
#tsingtao: 42,43,14
#Guinness: 31,1,27
#BlueMoon: 1,32,15
#DosEquis: 6,38,34



#let's investigate the one topic that overlaps as most favorited and most tweeted about for each company:
#Budlight: 24 
#TsingTao: 42
#Guinness: 31
#BlueMoon: 1

#DosEquis: 38 

#budlight
dir.create("graph/",showWarnings = F)
png('graph/budlight.png')

thoughts.budlight <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(24), n=4)
plot(thoughts.budlight, main="Budlight Topic 24", sub="Topic = Football Season")
dev.off()

#TsingTao
dir.create("graph/",showWarnings = F)
png('graph/tsingtao.png')

thoughts.tsingtao <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(42), n=4)
plot(thoughts.tsingtao, main="TsingTao Topic 42", sub="Topic = Brand Compeition")
dev.off()

#Guiness 
dir.create("graph/",showWarnings = F)
png('graph/Guinness.png')

thoughts.guinness <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(31), n=4)
plot(thoughts.guinness, main="Guinness Topic 31", sub="Topic = Fan Interaction")
dev.off()

#BlueMoon
dir.create("graph/",showWarnings = F)
png('graph/bluemoon.png')

thoughts.bluemoon <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(1), n=4)
plot(thoughts.bluemoon, main="BlueMoon Topic 1", sub="Topic = Promotional Correspondence")
dev.off()

#DosEquis
dir.create("graph/",showWarnings = F)
png('graph/DosEquis.png')

thoughts.de <- findThoughts(BeerModelFormatted, beermeta$reformattedtext, topics=c(38), n=4)
plot(thoughts.de, main="DosEquis Topic 38, Most Favorited Overall", sub="Topic = Most Interesting Man")
dev.off()




