## This function takes the packages that our program needs. 
## It makes sure you have them on your computer before proceeding.

source("checkpackages_harry.R")
check_packages(c("dplyr", 
                 "stringr", 
                 "tidytext", 
                 "ggplot2", 
                 "igraph", 
                 "reshape2", 
                 "tidyr",
                 "ggraph",
                 "wordcloud",
                 "widyr"))

#load all the packages required for this code
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)

#input the books
source("input_harry.R")


#Get the clean data from the philosophers_stone book
stone=harrypotter::philosophers_stone
str_length(stone)
text_df<-data_frame(line=1:17,text=stone)
stone_df<-text_df%>%unnest_tokens(word,text)
##remove stop word like "the","of".
tidy_stone<-stone_df%>%group_by(word)%>%anti_join(stop_words)%>%count(sort=T)%>%rename(freq=n)%>%ungroup()  

#Get the clean data from seven books and name it book
harry<-list(harrypotter::chamber_of_secrets,harrypotter::deathly_hallows,harrypotter::goblet_of_fire,harrypotter::half_blood_prince,harrypotter::order_of_the_phoenix,harrypotter::philosophers_stone,harrypotter::prisoner_of_azkaban)
titletag<-list("chamber_of_secrets","deathly_hallows","goblet_of_fire","half_blood_prince","order_of_the_phoenix","philosophers_stone","prisoner_of_azkaban")
harry_df<-vector("list",7)
tidy_harry<-vector("list",7)
for(i in 1:7){
  harry_df[[i]]=data_frame(chapter=1:length(harry[[i]]),text=harry[[i]])
  tidy_harry[[i]]=harry_df[[i]]%>%unnest_tokens(word,text)
  tidy_harry[[i]]=tidy_harry[[i]]%>%anti_join(stop_words)%>%group_by(word)%>%mutate(title=titletag[[i]],series=i)%>%arrange(chapter)
}
book<-do.call(rbind,tidy_harry)

