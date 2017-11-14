#make a vector which contains the titles of the 7 books
titles <- c("philosophers_stone", 
            "chamber_of_secrets", 
            "prisoner_of_azkaban", 
            "goblet_of_fire", 
            "order_of_the_phoenix", 
            "half_blood_prince", 
            "deathly_hallows")

#define books as a list of the seven books
books <- list(philosophers_stone, 
              chamber_of_secrets, 
              prisoner_of_azkaban,
              goblet_of_fire, 
              order_of_the_phoenix, 
              half_blood_prince,
              deathly_hallows)

#create an empty tibble   

series <- tibble()

#run a full loop to make the seven books into one-token-per-row structure.
for(i in seq_along(titles)) {
  #create an tibble called clean which has three columns, which are the ith book title, the chpater in ith book and             word (one word in each row)
  clean <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>%
    #using unnext_tokens to break the text into individual tokens 
    unnest_tokens(word, text) %>%
    #create a new column called that is the ith vector in vector titles
    mutate(book = titles[i]) %>%
    #select the columns book and the remaining columns 
    select(book, everything())
  
  #combine the empty tibble and clean,
  series <- rbind(series, clean)
}


Book6 <- series %>%
  filter (book == "half_blood_prince")

save(series, file = "data/rawdata/HP.Rda")
save(Book6, file = "data/rawdata/HP6.Rda")
