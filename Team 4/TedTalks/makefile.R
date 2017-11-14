#run files for installing and loading the necessary packages for this analysis
#clean and join data tables so it is in useable format for analysis
source("TedTalks/01_check_install.R") 
source("TedTalks/02_clean_ted.R")   

#generate html of final writeup
rmarkdown::render("TedTalks/final_paper.Rmd", output_format = "html_document")

