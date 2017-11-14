clean_book <- function(bookID){
  load("data/booklist.rda")
  tidy_books <- list()
  for (i in 1:length(bookID)){
    tidy_books[[i]] <- booklist[[i]] %>%mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("chapter|CHAPTER[\\divxlc]", 
                    ignore_case = TRUE)))) %>% unnest_tokens(word, text)
   
  }
  save(tidy_books, file = "data/tidy_books.Rda")
  #print( str(tidy_books)) #debug print
}

bookID<-c(751, 174, 2800, 10, 18223)

clean_book(bookID)
