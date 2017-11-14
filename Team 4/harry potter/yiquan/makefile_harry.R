## run all scripts
source("input_harry.R")     # Scrape data
source("clean_harry.R")     # clean data
source("question_harry.R")  # create graph

rmarkdown::render("textmining_Yiquan_Xu.Rmd", output_format = "html_document")

