##read in main data set
ted_main <- read_csv("TedTalks/data/ted_main.csv")

##read in transcripts
transcripts <- read_csv("TedTalks/data/transcripts.csv")

### Fix url formatting so the two data sets match:
transcripts$url = str_replace_all(transcripts$url, pattern = "\r", replacement = "")

###combine data sets
full_data <- inner_join(ted_main, transcripts, by = "url")

###remove any text in the transcript that is surrounded by parenthesis

for (i in 1:nrow(full_data)){
  full_data[i, "transcript"] = 
    str_replace_all(
      full_data[i, "transcript"],
      pattern = "\\([^()]+\\)", 
      " "
    )
}

## extract max rating for each talk from the ratings column
for(i in 1:nrow(full_data)) {
  rating_string <- str_sub(full_data$ratings[i], 2,-2)
  rating_vector <- unlist(strsplit(rating_string, split="}"))
  names <- str_extract_all(rating_vector, pattern = "'name': '" %R% one_or_more(WRD) %R% optional('-') %R% 
                             one_or_more(WRD), simplify = T)
  names <- str_replace(names, pattern = "'name': '", "")
  counts <- str_extract_all(rating_vector, pattern = "'count': " %R% one_or_more(DGT), simplify = T)
  counts <- str_replace(counts, pattern = "'count': ", "")
  full_data$max_rating[i] <- names[which.max(counts)]
}

#save full_data 
save(full_data, file = "TedTalks/data/full_data.Rda")


#use unnest_tokens to create a separate row for each word in each talk
transcripts_clean <- full_data %>% unnest_tokens(word, transcript)

#add a wordcount column to the transcripts_clean data
transcripts_clean <- transcripts_clean %>%
  group_by(name) %>%
  mutate(wordcount = n()) %>%
  ungroup()

save(transcripts_clean, file = "TedTalks/data/transcripts_clean.Rda")

#join transcript data with the bing + nrc lexicons, respectively
sentiments_bing <- transcripts_clean %>% inner_join(get_sentiments("bing")) %>% filter(!word %in% c("like", "right"))
save(sentiments_bing, file = "TedTalks/data/sentiments_bing.Rda")

sentiments_nrc <- transcripts_clean %>% inner_join(get_sentiments("nrc")) %>% filter(!word %in% c("like", "right"))
save(sentiments_nrc, file = "TedTalks/data/sentiments_nrc.Rda")

