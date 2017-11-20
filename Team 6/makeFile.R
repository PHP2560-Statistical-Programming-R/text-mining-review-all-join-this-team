

unlink("graph")
## run all scripts
## Scraped and cleaned data wrangling in the Data Wrangling File
## In confidentiality of personal credentials that file is not included on the public repository,
## However, the finished csv file is attached.

dir.create(file.path("graph"), showWarnings = FALSE)


source("check_packages.R")
source("YamSentimentAnalysis.R")     # General Sentiment Analysis of the different companies.
source("DCWordCloud.R")
source("LishaGraph.R")
source("DeAtleyDOWdata_visualization.R")
source("MichaelSTM.R")

##Knit the combination of all of the script files.
rmarkdown::render("Report of Beer Tweets.Rmd", output_format = "html_document")
