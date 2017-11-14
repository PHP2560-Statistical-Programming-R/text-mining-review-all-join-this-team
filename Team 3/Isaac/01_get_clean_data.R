## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("./Isaac/check_packages.R")
check_packages(c("devtools", "dplyr", "ggplot2", "gridExtra","harrypotter", "tidytext", "stringr", "tidyr", "rebus"))


####### Get the data #######
devtools::install_github("bradleyboehmke/harrypotter")


####### Tidy the data #######
library("harrypotter")
library("tidyverse")
library("stringr")
library("rebus")
library("tidytext")


# Create vector of book names
book_names <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban, goblet_of_fire, order_of_the_phoenix, half_blood_prince, deathly_hallows)
names(book_names) <- c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows")

# Initialize data frames
books1 = vector(mode = "list", length = 7)
books2 = vector(mode = "list", length = 7)

for(i in 1:7){
    data <- data_frame(text = book_names[[i]])    # Convert text to dataframe
    data <- mutate(data,chapter = c(1:nrow(data)), title = names(book_names)[i])   # add column for chapter numbers and title of book
    data1 <- data %>%
        unnest_tokens(word, text, to_lower = TRUE)   # Split by word and remove punctuation
    data2 <- data %>%
        unnest_tokens(word, text, to_lower = FALSE)   # Split by word and keep punctuation
    books1[[i]] <- data1   # Store clean lowercase data to one list
    books2[[i]] <- data2   # Store clean uppercase data to another list
}

# Make one data frame from list of data frames for both lowercase and uppercase
POA_lower <- plyr::ldply(books1, data.frame) %>%
             filter(title=="prisoner_of_azkaban")   # Prisoner of Azkaban in lowercase
POA_upper <- plyr::ldply(books2, data.frame) %>%   # Prisoner of Azkaban in uppercase
             filter(title=="prisoner_of_azkaban")
books_lower <- plyr::ldply(books1, data.frame) # Lowercase for all books


dir.create("./Isaac/data", showWarnings = FALSE)   # Create a directory to store data files

# Creates a file of data for both uppercase and lowercase for Prisoner of Azkaban
save(books_lower, file = "./Isaac/data/books_lower.Rda")
save(POA_lower, file = "./Isaac/data/POA_lower.Rda")
save(POA_upper, file = "./Isaac/data/POA_upper.Rda")
