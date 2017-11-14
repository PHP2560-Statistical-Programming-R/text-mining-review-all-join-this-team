## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("Carol/cleandata", recursive = TRUE) # where data json downloads
unlink("Carol/graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("Carol/cleandata"), showWarnings = FALSE)
dir.create(file.path("Carol/graph"), showWarnings = FALSE)


## run all scripts
source("Carol/text_mining.R")   # gather and clean data
source("Carol/plots.R")     # create graphs
rmarkdown::render("Carol/HarryPotter_Carol.Rmd", output_format = "html_document")
