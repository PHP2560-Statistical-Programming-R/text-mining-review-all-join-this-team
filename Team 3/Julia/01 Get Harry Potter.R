source("Julia/Check_Packages.R")
check_packages(c("tidytext","plyr","dplyr","stringr","ggplot2", "rebus"))

GetHPBooks<-function(){
  install.packages("devtools")
devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)
}

GetHPBooks()