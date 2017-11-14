## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)


## run all scripts
source("01_get_packages.R") # Scrape data
source("02_get_books.R")  # create useable data list
source("03_clean_books.R") ## clean data 
source("04_get_sentiment.R") ## get sentiments
source("05_graphs.R")  ## create graph
rmarkdown::render("paper.Rmd", output_format = "html_document")