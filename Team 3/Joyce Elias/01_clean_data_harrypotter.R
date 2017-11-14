
source("check_packages.R") 
check_packages(c("tidytext","dplyr","ggplot2","rebus", "devtools","harrypotter","tidyr","gridExtra")) # this checks the person's computer for these packages. If the person doesn't have them then it will download them for them.




devtools::install_github("bradleyboehmke/harrypotter")  # This pulls all 7 of the harry potter books
#Create directory, if it already exists then dont show warnings.
# This eliminates the need for setwd
# you can run my code and it will create these files whereever you store this data in

dir.create("./Joyce Elias/clean data/", showWarnings = FALSE) 





#### Begin to Clean the Data and Compile all 7 books into one dataset.####

book_names <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban, goblet_of_fire, order_of_the_phoenix, half_blood_prince, deathly_hallows) # make a list of all the book names 

names(book_names) <- c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows")  # assign names to each element of the list

books = vector(mode = "list", length = 7) # create an empty vector to store all the books

for(i in 1:length(books)){
  data <- data_frame(text = book_names[[i]]) #this converts all the text to a dataframe
  data <- mutate(data, chapter = c(1:nrow(data)), title = names(book_names)[i]) # this adds a column called "chapter" that specifies what chapter of the book , each word comes from. This also forms a column called "title" that correctly puts the name of each book next to its text
  data <- data %>%
    unnest_tokens(word, text, to_lower = FALSE)
  books[[i]] <- data
}

books <- plyr:: ldply(books, data.frame)  # make into a data frame


save(books, file="Joyce Elias/clean data/books.rda")

