
##This function adds a caps var to the data
add_vars <- function(data) {
  caps_pattern = "^[^a-z]*$"
  data$caps <- str_detect(data$word,caps_pattern)
  
  #Calculate total num of words in this case for later 
  data <- data %>%
    group_by(caps) %>%
    mutate(case_total=n()) %>%
    ungroup()
  
  return(data)
}

##This function matches words with sentiments
sent_analysis <- function(data) {
  ##Now change to lowercase for sentiment matches
  words$word = str_to_lower(words$word)
  sents.nrc = inner_join(words, get_sentiments("nrc"), by="word")
  
  return(sents.nrc)
}

##This function returns a tibble with percent of that
## emotion (must be given a string)
emotion_percent <- function(emotion,data) {
  emotion_tib <- data %>%
    count(caps,sentiment,case_total) %>%
    mutate(percent = n/case_total) %>%
    filter(sentiment==emotion)
  
  return(emotion_tib)
}


# Function to create a table for use in a prop.test
create_table <- function(tib) {
  tib$n_not <- tib$case_total-tib$n
  prop_table <- tib[,c(4,6)]
  
  prop_table <- matrix(c(prop_table$n[1],prop_table$n[2],prop_table$n_not[1],prop_table$n_not[2]),ncol=2)
  rownames(prop_table) <- c("lower","upper")
  
  return(prop_table)
}

#Function to run prop test for that emotion
run_test <- function(emotion,data) {
  perc_table <- emotion_percent(emotion,data)
  cleaned_table <- create_table(perc_table)
  colnames(cleaned_table) <- c(emotion,paste0("not_",emotion))
  prop.test(cleaned_table)
  
}

#Now actual work:
load("data/nyt_data_caps.rda")
words<-add_vars(words_caps)
sents.nrc <- sent_analysis(words)

pos_results <- run_test("positive",sents.nrc)
neg_results<- run_test("negative",sents.nrc)

save(sents.nrc,file="data/nyt_data_sents-nrc.rda")
save(pos_results,file="olivia_results/positive.rda")
save(neg_results,file="olivia_results/negative.rda")

