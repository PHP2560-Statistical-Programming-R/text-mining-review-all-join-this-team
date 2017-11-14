
clean_book <- function(bookID){
  load("data/booklist.rda")
  clean <- list()
  for (i in 1:length(bookID)){
    clean[[i]] <- booklist[[i]] %>%mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("chapter|CHAPTER[\\divxlc]", ignore_case = TRUE)))) %>% unnest_tokens(word, text)
    data_tidy <- do.call("rbind", clean)
  }
  save(data_tidy, file = "data/clean.rda")
  # print(data_tidy) debug print
}


clean_book(bookID)