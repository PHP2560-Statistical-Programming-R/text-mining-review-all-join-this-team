## clean all output from previous runs of scripts
unlink("graph", recursive = TRUE) # where graphs are stores

# now re-create the results directory
dir.create(file.path("graph"), showWarnings = FALSE)
dir.create(file.path("data"), showWarnings = FALSE)

## run all scripts
source("Ivanka_Trump_by_Fuyu/00_getting_packages_iv.R")       # install and prepare the requiered packages
source("Ivanka_Trump_by_Fuyu/01_getting_data_iv.R")           # read and store the data
source("Ivanka_Trump_by_Fuyu/02_cleaning_data_iv.R")         # clean the data
source("Ivanka_Trump_by_Fuyu/03_sentiment_analysis_iv.R")     # conduct sentiment analysis
source("Ivanka_Trump_by_Fuyu/04_visualization_iv.R")          # create plot and graphs

rmarkdown::render(normalizePath("Trump_Twitter/Ivanka_Trump_by_Fuyu/text-mining-fuyu1.Rmd"), output_format = "html_document")



