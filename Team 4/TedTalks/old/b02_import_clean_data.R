
### Read in metadata:
ted_main <- read_csv("data/ted_main.csv")

### Read in lecture transcripts:
transcripts <- read_csv("data/transcripts.csv")

### Fix url formatting so the two data sets match:
transcripts$url = str_replace_all(transcripts$url, pattern = "\r", replacement = "")

###Combine data sets
full_data <- inner_join(ted_main, transcripts, by = "url")

### Remove any text in the transcript that is surrounded by parenthesis
### This eliminates audience applause, music, etc.:

for (i in 1:nrow(full_data)){
  full_data[i, "transcript"] = 
    str_replace_all(
      full_data[i, "transcript"],
      pattern = "\\([^()]+\\)", 
      " "
    )
}

###Break transcripts into tokens
transcripts_clean = full_data %>% unnest_tokens(output = word, input = transcript)

###Add a wordcount column
transcripts_clean = transcripts_clean %>%
  group_by(name) %>%
  mutate(wordcount = n()) %>%
  ungroup()

###Prepare for sentiment analysis by
###joining with NRC and Bing Lexicons
transcripts_bing = transcripts_clean %>% inner_join(get_sentiments("bing"))
transcripts_nrc = transcripts_clean %>% inner_join(get_sentiments("nrc")) %>% filter(!word %in% c("like"))


###Save as Rda files for faster loading
save(full_data, file = "data/bfull_data.Rda")
save(transcripts_clean, file = "data/btranscripts_clean.Rda")
save(transcripts_bing, file = "data/btranscripts_bing.Rda")
save(transcripts_nrc, file = "data/btranscripts_nrc.Rda")