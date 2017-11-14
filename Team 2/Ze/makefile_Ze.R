


## run all scripts
source("01_get_packages.R")   # Scrape data
source("02_get_books.R")     # create useable data list
source("03_clean_books.R")     ## clean data
source("04_join_sentiment.R")  ## join with sentiment for analysis
source("05_graph.R")             ## creat graphs
rmarkdown::render("Paper_Ze.Rmd", output_format = "html_document")
