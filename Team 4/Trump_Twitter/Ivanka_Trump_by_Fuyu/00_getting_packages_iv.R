names <- c("stringr","rebus","tidyverse","tidytext","lubridate","purrr","dplyr","readr","wordcloud")


for(i in names) {
  if(!(i %in% installed.packages()))
    install.packages(name, repos="http://cran.us.r-project.org")
  library(i, character.only = TRUE)
}
