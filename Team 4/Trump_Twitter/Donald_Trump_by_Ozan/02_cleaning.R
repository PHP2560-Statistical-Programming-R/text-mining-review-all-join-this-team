date_and_time <- DT_all_tweets$created_at %>%
  str_split(pattern = " ", simplify = TRUE) %>%
  as_tibble() %>%
  rename(date = V1, time = V2) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

date <- date_and_time$date %>%
  str_match(
    pattern = capture(one_or_more(DGT)) %R% 
      "-" %R% capture(one_or_more(DGT)) %R% 
      "-" %R% capture(one_or_more(DGT))
          ) %>%
  as_tibble() %>%
  rename(date = V1, year = V2, month = V3, day = V4)
# filter out retweets
clean_data <- DT_all_tweets %>%
  cbind(date) %>%
  filter(is_retweet == "FALSE") %>%
  select(text, date, year, month, day) 

# store the text as character
clean_data$text <- as.character(clean_data$text)

# extract the words and filter out the unnecessary ones
tidy_words <- clean_data %>%
  as.tbl() %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c( 
                     "t.co", 
                     "http", 
                     "https", 
                     "amp",
                     "twitter",
                     "android",
                     "web",
                     "client",
                     "realdonaldtrump"
                     )))
