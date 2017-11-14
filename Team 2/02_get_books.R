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


bookID<- c(19033,174,10,2800,18223)
get_book(bookID)


get_books <- function(bookID){
  
  url_part1 <- "https://www.gutenberg.org/files/"
  url_part2 <- "/"
  url_part3 <- ".txt"
  
  dir.create("books/", showWarnings = FALSE)
  
  
  #Get the data
  #Write the data into a JSON file
  
  for (i in 1:length(bookID)){
    url <- paste0(url_part1, bookID[i],url_part2, bookID[i], url_part3)
    
    d=GET(url)
    file = paste0("books/",bookID[i],".txt")
    write(content(d, as="text"), file=file)
    
  }
}


get_books(bookID)