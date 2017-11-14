# export the data manually from "http://www.trumptwitterarchive.com/archive/account/ivankatrump" and save as a csv file

# read the csv file
ivanka_trump_twitter <- read_csv("Data/ivanka_trump_twitter.csv")

# get the date and the time of the most recent tweet
most_recent_tweet <- ivanka_trump_twitter[1,3]

# print the date and the time of the most recent tweet
paste0("The date and the time of the most recent tweet:", most_recent_tweet)

