## clean all output from previous runs of scripts

unlink("./clean data", recursive = TRUE) # where the clean data is stored
unlink("./graph", recursive = TRUE) # where all the graphs are stored

# now re-create the results directory
dir.create(file.path("Joyce Elias/clean data"), showWarnings = FALSE)
dir.create(file.path("Joyce Elias/graph"), showWarnings = FALSE)

## run all scripts

source("Joyce Elias/01_clean_data_harrypotter.R")   # load and clean the data 
source("Joyce Elias/02_sentiment_analysis.R")     # use sentiment analysis to look at different questions and graph the analysis

rmarkdown::render("Joyce Elias/Sentiment Analysis.Rmd", output_format = "html_document")