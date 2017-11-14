# This function takes the packages needed in the text mining.

source("check_package.R")
install_packages(c("devtools","tidytext","stringr","dplyr", 
                   "tidyr","ggplot2","wordcloud","reshape2",
                   "igraph","ggraph","topicmodels"))

# Gather data from github - Harry Potter Complete 7 Books text

devtools::install_github("bradleyboehmke/harrypotter")