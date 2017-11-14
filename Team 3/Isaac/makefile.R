## clean all output from previous runs of scripts
unlink("./Isaac/data", recursive = TRUE) # where data is stored after clean
unlink("./Isaac/graphs", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("./Isaac/data"), showWarnings = FALSE)
dir.create(file.path("./Isaac/graphs"), showWarnings = FALSE)


## run all scripts
source("./Isaac/01_get_clean_data.R")   # Scrape data
source("./Isaac/02_get_graphs.R")     # create graph
rmarkdown::render("Isaac/Paper.Rmd", output_format = "html_document")
