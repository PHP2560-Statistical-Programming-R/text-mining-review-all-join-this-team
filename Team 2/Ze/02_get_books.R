#create a function to download the books and save the books in a folder (books) in rda format
get_book <- function(bookID){
  #Create directory, if it already exists then dont show warnings.
  # This eliminates the need for setwd
  # running my code will create these files whereever you store this data in
  dir.create("data/", showWarnings = FALSE)
  
  booklist <- list() #list of dataframes that will contain all the 5 books
  for (i in 1:length(bookID)){
    booklist[[i]] <- gutenberg_download(bookID[i]) #download one by one and save in the list
    save(booklist, file="data/booklist.rda")
  }
  # print(booklist) debug print
 }
bookID<-c(751, 174, 2800, 10, 18223)

get_book(bookID)