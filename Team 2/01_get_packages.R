
## This function checks the installed packages on local computer and installs if already not installed and loads them 

source("check_packages.R")
check_packages(c("dplyr","ggplot2","gutenbergr","stringr", "stringi", "tidytext", "tidyr", "gridExtra", "SnowballC",
                 "wordcloud", "RColorBrewer", "httr","XML","jsonlite"))

# dplyr for data manipulation
# ggplot2 for graphs
# gutenbergr to download gutenberg books
# stringr, stringi  for text analysis
# tidytext, tidyr for tidying the data
# gridExtra to arrange multiple plots in one page
# SnowballC for text stemming
# wordcloud word-cloud generator 
# RColorBrewer color palettes


