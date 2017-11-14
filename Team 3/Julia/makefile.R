## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("graph"), showWarnings = FALSE)


## run all scripts
source("Julia/01_Get_Harry_Potter.R")   # Scrape data
source("Julia/02_Clean_Data.R")     # clean data
source("Julia/03_Graphs.R")     # create graph
rmarkdown::render("Julia/Paper.Rmd", output_format = "html_document")
