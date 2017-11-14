## clean all output from previous runs of scripts
## there is one final we create in the main directory (the processed dataset),
## but the rest of the output is all in the 'results' directory
unlink("data", recursive = TRUE) # where data is stored after clean
unlink("graph", recursive = TRUE) # where graphs are stores
unlink("data/rawdata", recursive = TRUE) # where the data after cleaning is stored
unlink("data/modifieddata", recursive = TRUE) # where the data after modifying is stored

# now re-create the results directory
dir.create(file.path("graph"), showWarnings = FALSE)
dir.create(file.path("data"), showWarnings = FALSE)
dir.create(file.path("data/rawdata"), showWarnings = FALSE)
dir.create(file.path("data/modifieddata"), showWarnings = FALSE)

## run all scripts
source("cleandata/01_get data.R")   # get data
source("cleandata/02_clean data.R")   # clean data
source("cleandata/03_clean data.R")   # clean data
source("question/01.R")  
source("question/02.R")  
source("question/03.R") 
source("question/04.R")  
source("question/05.R")  
source("question/06.R")  
source("question/07.R")  
source("question/08.R")
source("question/09.R")
rmarkdown::render("edit_paper.Rmd", output_format = "html_document")

