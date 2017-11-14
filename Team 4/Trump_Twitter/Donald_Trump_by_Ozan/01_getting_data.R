# export the data manually from "http://www.trumptwitterarchive.com/archive" and save as a csv file

# read the csv file 
DT_all_tweets <- read.csv("data/Donald_Trump_twitter.csv")

# get the date and the time of the most recent tweet
most_recent_tweet <- DT_all_tweets[1, 3]

# print the date and the time of the most recent tweet
paste0("The date and the time of the most recent tweet: ", most_recent_tweet)
