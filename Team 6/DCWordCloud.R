# Extraction Libraries
library(stringr)
library(dplyr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(tm)
library(wordcloud)

# Importing our twitter data into the environment
AllFormattedTweets <- read.csv("AllFormattedTweets.csv")

# Filters tweets by company 
budlightTweets <- AllFormattedTweets %>% filter(screenName == "budlight")
DosEquisTweets <- AllFormattedTweets %>% filter(screenName == "DosEquis")
GuinnessUSTweets <- AllFormattedTweets %>% filter(screenName == "GuinnessUS")
tsingtaoTweets <- AllFormattedTweets %>% filter(screenName == "tsingtao")
BlueMoonBrewCoTweets <- AllFormattedTweets %>% filter(screenName == "BlueMoonBrewCo")

# Converts the text of our tweet csv file to a corpus 
budlightCorpus <- Corpus(VectorSource(budlightTweets$reformattedtext)) #corpus is a structure in the tm package that represents a collection of text documents
DosEquisCorpus <- Corpus(VectorSource(DosEquisTweets$reformattedtext))
GuinnessUSCorpus <- Corpus(VectorSource(GuinnessUSTweets$reformattedtext))
tsingtaoCorpus <- Corpus(VectorSource(tsingtaoTweets$reformattedtext))
BlueMoonBrewCoCorpus <- Corpus(VectorSource(BlueMoonBrewCoTweets$reformattedtext))
AllCorpus <- Corpus(VectorSource(AllFormattedTweets$reformattedtext))

# Removes spaces and non-English characters
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
budlightCorpus <- tm_map(budlightCorpus, content_transformer(removeNumPunct))
DosEquisCorpus <- tm_map(DosEquisCorpus, content_transformer(removeNumPunct))
GuinnesUSCorpus <- tm_map(GuinnessUSCorpus, content_transformer(removeNumPunct))
tsingtaoCorpus <- tm_map(tsingtaoCorpus, content_transformer(removeNumPunct))
BlueMoonBrewCoCorpus <- tm_map(BlueMoonBrewCoCorpus, content_transformer(removeNumPunct))
AllCorpus <- tm_map(AllCorpus, content_transformer(removeNumPunct))

# Removes commonly used words
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp", "us", "can", "we", "he", "she")
budlightCorpus <- tm_map(budlightCorpus, removeWords, myStopwords)
DosEquisCorpus <- tm_map(DosEquisCorpus, removeWords, myStopwords)
GuinnessUSCorpus <- tm_map(GuinnessUSCorpus, removeWords, myStopwords)
tsingtaoCorpus <- tm_map(tsingtaoCorpus, removeWords, myStopwords)
BlueMoonBrewCoCorpus <- tm_map(BlueMoonBrewCoCorpus, removeWords, myStopwords)
AllCorpus <- tm_map(AllCorpus, removeWords, myStopwords)

# Removes any additional whitespace
budlightCorpus <- tm_map(budlightCorpus, stripWhitespace)
DosEquisCorpus <- tm_map(DosEquisCorpus, stripWhitespace)
GuinnessUSCorpus <- tm_map(GuinnessUSCorpus, stripWhitespace)
tsingtaoCorpus <- tm_map(tsingtaoCorpus, stripWhitespace)
BlueMoonBrewCoCorpus <- tm_map(BlueMoonBrewCoCorpus, stripWhitespace)
AllCorpus <- tm_map(AllCorpus, stripWhitespace)

# Term-Document Matrix created from corpus with terms as rows and documents as columns
budlight.tdm <- TermDocumentMatrix(budlightCorpus, control = list(wordLengths = c(1, 20)))
DosEquis.tdm <- TermDocumentMatrix(DosEquisCorpus, control = list(wordLengths = c(1, 20)))
GuinnessUS.tdm <- TermDocumentMatrix(GuinnessUSCorpus, control = list(wordLengths = c(1, 20)))
tsingtao.tdm <- TermDocumentMatrix(tsingtaoCorpus, control = list(wordLengths = c(1, 8)))
BlueMoonBrewCo.tdm <- TermDocumentMatrix(BlueMoonBrewCoCorpus, control = list(wordLengths = c(1, 20)))
All.tdm <- TermDocumentMatrix(AllCorpus, control = list(wordLengths = c(1, 20)))

# Converting tdm to a matrix 
budlight.m <- as.matrix(budlight.tdm)
DosEquis.m <- as.matrix(DosEquis.tdm)
GuinnessUS.m <- as.matrix(GuinnessUS.tdm)
tsingtao.m <- as.matrix(tsingtao.tdm)
BlueMoonBrewCo.m <- as.matrix(BlueMoonBrewCo.tdm)
All.m <- as.matrix(All.tdm)

# Calculates and sorts the frequency of words by decreasing frequency
budlight.freq <- sort(rowSums(budlight.m), decreasing = T)
DosEquis.freq <- sort(rowSums(DosEquis.m), decreasing = T)
GuinnessUS.freq <- sort(rowSums(GuinnessUS.m), decreasing = T)
tsingtao.freq <- sort(rowSums(tsingtao.m), decreasing = T)
BlueMoonBrewCo.freq <- sort(rowSums(BlueMoonBrewCo.m), decreasing = T)
All.freq <- sort(rowSums(All.m), decreasing = T)

# Adding Color
color <- brewer.pal(9, "BuGn")[-(1:4)]

# Creates a png file within the graph folder for the wordcloud
dir.create("graph/",showWarnings = F)
png('graph/budlightCloud.png')

# Creating a wordcloud plot with a decreasing word frequency 
wordcloud(words = names(budlight.freq), freq = budlight.freq, max.words = 100, min.freq = 5,
          random.order = F, colors = color)
dev.off()

png('graph/DosEquisCloud.png')
wordcloud(words = names(DosEquis.freq), freq = DosEquis.freq, max.words = 100, min.freq = 5,
          random.order = F, colors = color)
dev.off()

png('graph/GuinnessUSCloud.png')
wordcloud(words = names(GuinnessUS.freq), freq = GuinnessUS.freq, max.words = 100, min.freq = 5,
          random.order = F, colors = color)
dev.off()

png('graph/tsingtaoCloud.png')
wordcloud(words = names(tsingtao.freq), freq = tsingtao.freq, max.words = 100, min.freq = 1,
          random.order = F, colors = color)
dev.off()

png('graph/BlueMoonBrewCoCloud.png')
wordcloud(words = names(BlueMoonBrewCo.freq), freq = BlueMoonBrewCo.freq, max.words = 100, min.freq = 5,
          random.order = F, colors = color)
dev.off()

png('graph/AllBeerCloud.png')
wordcloud(words = names(All.freq), freq = All.freq, max.words = 150, min.freq = 5,
          random.order = F, colors = color)
dev.off()
