## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("check_packages.R")
check_packages(c("devtools", "tidytext", "dplyr", "stringr", "harrypotter", "ggplot2", "tidyr", "rebus", "gridExtra"))


####### Get the data #######
devtools::install_github("bradleyboehmke/harrypotter")

# Create directory, if it already exists then dont show warnings.
# This eliminates the need for setwd
# you can run my code and it will create these files whereever you store this data in
dir.create("Carolina/clean_data/", showWarnings = FALSE)
  

####### Tidy the data #######
library(dplyr)
library(tidytext)
library(harrypotter)
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

saga <- tibble()

for (i in seq_along(titles)) { # create a tibble for each book
  data <- tibble(text = books[[i]]) %>% # add text
    mutate(book = titles[i]) %>% # define a new column for book name
    mutate(chapter = seq_along(books[[i]])) %>% # define a new column for chapter number
    group_by(book) %>% # group by book title
    unnest_tokens(word, text) %>%  # transform the non-tidy text data to tidy text data
    ungroup()
  
  saga <- rbind(saga, data) # bind rows of tibble of each book
}

saga

save(saga, file = "Carolina/clean_data/saga.rda")

####### Data for Chamber of Secrets only #######
book2 <- saga %>%
  filter(book=="Chamber of Secrets")

save(book2, file = "Carolina/clean_data/book2.rda")
  
