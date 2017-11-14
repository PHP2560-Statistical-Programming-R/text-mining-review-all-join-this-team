#joining with the sentiment 
bing_book <- function(bookID){
load("sadia/data_SS/tidy_books.Rda")
books_bing <- list()
for (i in 1:length(bookID)){
books_bing[[i]] <- tidy_books[[i]] %>% inner_join(get_sentiments("bing"))
  data_bing <- bind_rows(books_bing)
}
save(data_bing, file = "sadia/data_SS/data_bing.Rda")
}

bing_book(bookID)

afinn_book <- function(bookID){
  load("sadia/data_SS/tidy_books.Rda")
  books_afinn <- list()
  for (i in 1:length(bookID)){
    books_afinn[[i]] <- tidy_books[[i]] %>% inner_join(get_sentiments("afinn"))
    data_afinn <- bind_rows(books_afinn)
  }
  save(data_afinn, file = "sadia/data_SS/data_afinn.Rda")
}

afinn_book(bookID)





