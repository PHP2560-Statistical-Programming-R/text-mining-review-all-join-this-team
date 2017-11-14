library(harrypotter)
#clean the data
clean_HP<-function(){
#make a list of book names
book_names <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban, goblet_of_fire, order_of_the_phoenix, half_blood_prince, deathly_hallows)
#concatenate a vector of names
names(book_names) <- c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows")

books = vector(mode = "list", length = 7)
#create a for loop to go through the chapters of the books and unnest the text into words
for(i in 1:length(books)){
  data <- data_frame(text = book_names[[i]])
  data <- mutate(data, chapter = c(1:nrow(data)), title = names(book_names)[i])
  data <- data %>%
    unnest_tokens(word, text, to_lower = TRUE)
  books[[i]] <- data
}
#new dataset books
books <- ldply(books, data.frame)
save(books, file = "Julia/data/books.Rda")
}
clean_HP()