# Get libraries
library(tidytext)
library(dplyr)
library(rebus)
library(stringr)
library(ggplot2)

# Get Data
library(RTextTools)
data(NYTimes)

# Get dictionary with sentiment scores using lexicon afinn 
sentiment.scores = as.data.frame(get_sentiments("afinn"))

# Prepare the titles as strings by converting title to characters
NYTimes$title = (as.character(NYTimes$Title))