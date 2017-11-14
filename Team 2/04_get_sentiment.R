library(stringr)
bing_book <- function(bookID){
  load("data/clean.rda")
  books_bing <- list()
  for (i in 1:length(bookID)){
    books_bing[[i]] <- data_tidy %>% inner_join(get_sentiments("bing")) %>% group_by(sentiment) %>% mutate(total_words=n())
    data_bing <- bind_rows(books_bing)
  }
  save(data_bing, file = "data/bing_book.rda")
}

afinn_book <- function(bookID){
  load("data/clean.rda")
  books_afinn <- list()
  for (i in 1:length(bookID)){
    books_afinn[[i]] <- data_tidy %>% inner_join(get_sentiments("afinn")) %>% 
      group_by(gutenberg_id, chapter) %>% 
      summarise(chapter.score=sum(score)) %>%
    data_afinn <- bind_rows(books_afinn)
  }
  save(data_afinn, file = "data/afinn_book.rda")
}

nrc_book <- function(bookID){
  load("data/clean.rda")
  books_nrc <- list()
  for (i in 1:length(bookID)){
    books_nrc[[i]] <- data_tidy %>% inner_join((get_sentiments("nrc"))) %>%  group_by(gutenberg_id) %>% count(sentiment)
    data_nrc <- bind_rows(books_nrc)
  }
  save(data_nrc, file = "data/nrc_book.rda")
}

cloud_book <- function(bookID){
  load("data/clean.rda")
  books_cloud <- list()
  for (i in 1:length(bookID)){
    books_cloud[[i]] <- data_tidy %>% inner_join(get_sentiments("bing")) %>% 
      group_by(gutenberg_id) %>% count(word, sentiment) %>% arrange(desc(n))
    data_cloud <- bind_rows(books_cloud)
  }
  save(data_cloud, file = "data/cloud_book.rda")
}


total_book <- function(bookID){
  load("data/clean.rda")
  books_total <- list()
  for (i in 1:length(bookID)){
    books_total[[i]] <- data_tidy %>% inner_join(get_sentiments("afinn")) %>% 
      group_by(gutenberg_id) %>% summarise(total= sum(score)) %>% 
      mutate(book=c("The Bible","Picture of Dorian Gray", "Alice's Adventures in Wonderland", "The Koran", "The Essence of Buddhism")) %>% 
      arrange(desc(total))
    data_total <- as.data.frame(books_total)
  }
  save(data_total, file = "data/total_book.rda")
}

bing_book(bookID)
afinn_book(bookID)
nrc_book(bookID)
cloud_book(bookID)
total_book(bookID)







