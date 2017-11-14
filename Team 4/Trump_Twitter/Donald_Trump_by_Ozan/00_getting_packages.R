# check for requiered packages and get them if needed
names <- c(
            "dplyr", 
            "stringr", 
            "rebus", 
            "lubridate", 
            "ggplot2", 
            "tidytext", 
            "wordcloud"
           )

for(name in names) {
  if (!(name %in% installed.packages()))
    install.packages(name, repos="http://cran.us.r-project.org")
  library(name, character.only=TRUE)
}
