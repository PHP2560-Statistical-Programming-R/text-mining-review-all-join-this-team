check_packages <- function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages("twitteR")
check_packages("ROAuth")
check_packages("tidyverse")
check_packages("tidytext")
check_packages("tm")
check_packages("lubridate")
check_packages("stringr")
check_packages("stm")
check_packages("dplyr")
check_packages("stringi")
check_packages("DataCombine")
check_packages("rebus")
check_packages("syuzhet")
check_packages("ggplot2")
check_packages("rebus")
check_packages("Hmisc")
check_packages("wordcloud")




