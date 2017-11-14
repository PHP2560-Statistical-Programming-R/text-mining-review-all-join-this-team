suppressMessages(suppressWarnings(library(RTextTools)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(caTools)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(rebus)))
suppressMessages(suppressWarnings(library(foreign)))
suppressMessages(suppressWarnings(library(nnet)))
suppressMessages(suppressWarnings(library(magrittr)))
set.seed(23)

load("data/nyt_data.rda")
load("data/nyt_words.rda")

#clean/match
nrc.sents = inner_join(words, get_sentiments("nrc"), by="word")
bing.sents = inner_join(words, get_sentiments("bing"), by="word")


bing.words <- bing.sents %>% select(word)
nrc.words <- nrc.sents %>% filter(sentiment %in% c("positive", "negative")) %>% select(word)
#The above procedure is done to ensure that each token is counted only once. As described below, 
#NRC gives a positive/negative value exactly once for each word.
num.bing=dim(bing.words)[1] #number of bing matches
num.nrc= dim(nrc.words)[1] #number of nrc matches

nrc.lengths <- apply(nrc.words, 1, nchar) #length of each word
bing.lengths <- apply(nrc.words, 1, nchar) #length of each word
df <- data.frame(length=c(nrc.lengths, bing.lengths), dict = c(rep("NRC", length(nrc.words)), rep("Bing", length(bing.words))))
brianp1=ggplot(df, aes(x=factor(dict),y=length,fill=factor(dict)))+
  geom_boxplot() + labs(title="Word Lengths by Dictionary") +facet_wrap(~dict)
ggsave('graph/brianp1.png')
