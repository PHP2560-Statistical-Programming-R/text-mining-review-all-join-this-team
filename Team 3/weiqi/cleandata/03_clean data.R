titles <- c("philosophers_stone", 
            "chamber_of_secrets", 
            "prisoner_of_azkaban", 
            "goblet_of_fire", 
            "order_of_the_phoenix", 
            "half_blood_prince", 
            "deathly_hallows")

books <- list(philosophers_stone, 
              chamber_of_secrets, 
              prisoner_of_azkaban,
              goblet_of_fire, 
              order_of_the_phoenix, 
              half_blood_prince,
              deathly_hallows)

hp_bigrams <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  hp_bigrams <- rbind(hp_bigrams, clean)
}


Book6bigrams <- hp_bigrams%>%
  filter (book == "half_blood_prince")

save(Book6bigrams, file = "data/rawdata/HP6bigrams.Rda")

