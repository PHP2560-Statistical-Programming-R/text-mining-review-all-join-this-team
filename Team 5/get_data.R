get_data <- function() {
  
  data("NYTimes")
  
  save(NYTimes,file="data/nyt_data.rda")
}

source("check_packages.R")
check_packages(c("RTextTools","tidytext","stringr","dplyr","tidyverse","rebus")) #load packages everyone needs
get_data()