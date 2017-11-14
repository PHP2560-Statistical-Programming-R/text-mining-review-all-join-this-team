## clean all output from previous runs of scripts
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("graph"), showWarnings = FALSE)

## run all scripts
source("00_getting_packages.R")       # install and prepare the requiered packages
source("01_getting_data.R")           # read and store the data
source("02_cleaning.R")               # clean the data
source("03_sentiment_analysis.R")     # conduct sentiment analysis
source("04_visualization.R")          # create plot and graphs
rmarkdown::render("Paper.Rmd", output_format = "html_document")