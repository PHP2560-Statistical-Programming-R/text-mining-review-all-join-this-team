# This function can tidy text and return a dataframe that 

harry<-list(harrypotter::philosophers_stone,harrypotter::chamber_of_secrets,harrypotter::prisoner_of_azkaban,harrypotter::goblet_of_fire,harrypotter::order_of_the_phoenix,harrypotter::half_blood_prince,harrypotter::deathly_hallows)

harry_df<-vector("list",7)
tidy_harry<-vector("list",7)

# Create the vector of the titles in the order of publication time
harry_title<-c("philosophers_stone","chamber_of_secrets","prisoner_of_azkaban","goblet_of_fire","order_of_the_phoenix","half_blood_prince","deathly_hallows")

for(i in 1:7) {
  harry_df[[i]]<-data_frame(chapter=1:length(harry[[i]]),text=harry[[i]])
  
  tidy_harry[[i]]<-harry_df[[i]]%>%unnest_tokens(word,text) # Break the text into individual tokens
  
  data(stop_words)
  tidy_harry[[i]]<-tidy_harry[[i]]%>%
    anti_join(stop_words)%>% # Remove stop words
    mutate(title=harry_title[i],series=i) # Mutate the title and series variable which indicate the order of the publication time
}

sevenbook<-do.call(rbind,tidy_harry) # Combine the 7 dataframes in the tidy_harry list
