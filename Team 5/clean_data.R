clean_data <- function() {
  
  load("data/nyt_data.rda")
  
  NYTimes$Title = as.character(NYTimes$Title)
  words_caps = unnest_tokens(NYTimes, word, Title,to_lower=FALSE)
  words = unnest_tokens(NYTimes, word, Title)
  
  save(words_caps,file="data/nyt_data_caps.rda")
  save(words,file="data/nyt_words.rda")
}

clean_data()