options(repos="https://cran.rstudio.com")
install.packages("RTextTools")

library(RTextTools)
data(NYTimes)

library(tidyverse)
install.packages("tidytext")
library(tidytext)
library(dplyr)

NYTimes$Title = as.character(NYTimes$Title)
words = unnest_tokens(NYTimes, word, Title)
sentiments = inner_join(words, get_sentiments("afinn"), by = "word")
sentiments$Date = as.Date(as.Date(sentiments$Date, format = "%d-%b-%y"), format = "%y-%b-%d")