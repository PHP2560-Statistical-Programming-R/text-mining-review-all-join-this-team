clean_book <- function(bookID){
  load("sadia/data_SS/booklist.rda")
  tidy_books <- list()
  for (i in 1:length(bookID)){
    tidy_books[[i]] <- booklist[[i]] %>%mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("chapter|CHAPTER[\\divxlc]", 
                    ignore_case = TRUE)))) %>% unnest_tokens(word, text)
     }
  save(tidy_books, file = "sadia/data_SS/tidy_books.Rda")
  #print( str(tidy_books)) #debug print
}

clean_book(bookID)
