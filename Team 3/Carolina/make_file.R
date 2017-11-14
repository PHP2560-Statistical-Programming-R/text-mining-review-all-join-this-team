## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("clean_data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("Carolina/clean_data"), showWarnings = FALSE)
dir.create(file.path("Carolina/graph"), showWarnings = FALSE)

## run all scripts
source("Carolina/01_get_and_clean_data.R")   # get and clean the data
source("Carolina/02_graph.R")    # create graph
rmarkdown::render("Carolina/paper.Rmd", output_format = "html_document")
