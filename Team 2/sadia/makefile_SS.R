## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("sadia/data_SS", recursive = TRUE) # where data is stored after clean
unlink("sadia/graph_SS", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("sadia/data_SS"), showWarnings = FALSE)
dir.create(file.path("sadia/graph_SS"), showWarnings = FALSE)


## run all scripts
bookID<-c(751, 174, 2800, 10, 18223)
source("sadia/01_get_packages_SS.R")   # install the required packages
source("sadia/02_get_books_SS.R")     # download the books
source("sadia/03_clean_books_SS.R")   # clean the books
source("sadia/04_join_sentiment_SS.R")   # join with the lexicon for sentiment analysis
source("sadia/05_graph_SS.R")     # create the graphs
rmarkdown::render("sadia/data_download_SS.Rmd", output_format = "html_document") # shows up the steps of downloading the data
rmarkdown::render("sadia/06_paper_SS.Rmd", output_format = "html_document") # I have included a downloaded picture of the book in the paper which is stored in pic_SS folder. 

