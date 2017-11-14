## clean all output from previous runs of scripts
unlink("./maggie/data", recursive = TRUE) # where data are stored after cleaning
unlink("./maggie/plots", recursive = TRUE) # where plots are stored


# choose one of the following books to analyze:
## "philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", 
## "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows"
book_title <- "deathly_hallows"

# run all scripts
source("./maggie/01_clean_data.R")   # get & clean data
source("./maggie/02_analyze_data.R")    # analyze data and create plots
rmarkdown::render("./maggie/paper.Rmd", output_format = "html_document")