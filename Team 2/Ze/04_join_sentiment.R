#joining with the sentiment 
bing_book <- function(bookID){
load("data/tidy_books.Rda")
books_bing <- list()
for (i in 1:length(bookID)){
books_bing[[i]] <- tidy_books[[i]] %>% inner_join(get_sentiments("bing"))
  data_bing <- bind_rows(books_bing)
}
save(data_bing, file = "data/bing_books.Rda")
}

bing_book(bookID)

afinn_book <- function(bookID){
  load("data/tidy_books.Rda")
  books_afinn <- list()
  for (i in 1:length(bookID)){
    books_afinn[[i]] <- tidy_books[[i]] %>% inner_join(get_sentiments("afinn"))
    data_afinn <- bind_rows(books_afinn)
  }
  save(data_afinn, file = "data/afinn_books.Rda")
}

afinn_book(bookID)

nrc_book <- function(bookID){
  load("data/tidy_books.Rda")
  books_nrc <- list()
  for (i in 1:length(bookID)){
    books_nrc[[i]] <- tidy_books[[i]] %>% inner_join(get_sentiments("nrc"))
    data_nrc <- bind_rows(books_nrc)
  }
  save(data_nrc, file = "data/nrc_books.Rda")
}

nrc_book(bookID)




