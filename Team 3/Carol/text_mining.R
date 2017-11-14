source("Carol/check_packages.R")
check_packages(c("devtools","plyr","dplyr","tidytext", "stringr", "stringi", "ggplot2", "harrypotter", "rebus", "tokenizers", "tm"))

dir.create("Carol/cleandata/", showWarnings = FALSE)

#Gather libraries
library(plyr)
library(dplyr)
library(tidytext)
library(stringr)
library(stringi)
library(ggplot2)
library(harrypotter)
library(rebus)
library(tokenizers)
library(tm)

#Gather data
book_names <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban, goblet_of_fire, order_of_the_phoenix, half_blood_prince, deathly_hallows)

names(book_names) <- c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows")

#divide books, put into datasets
books = vector(mode = "list", length = 7)
for(i in 1:length(books)){
  data <- data_frame(text = book_names[[i]]) #put into data_frame
  data <- mutate(data, chapter = c(1:nrow(data)), title = names(book_names)[i]) #put chapter and book names
  data <- data %>%
    unnest_tokens(word, text, to_lower = TRUE) #separate words
  books[[i]] <- data #store words into data_frame
  
  file = paste0("Carol/cleandata/book",i,".Rda")
  save(data, file = file)
}


#Carol will be working on Goblet of Fire:
#(1) How the sentiment changes across the entire book
bing <- get_sentiments("bing")
cs_book= books[[4]]
sentiment_4=dplyr::inner_join(cs_book, bing) #merge sentiments
percents <- sentiment_4 %>% dplyr::group_by(chapter) %>% dplyr::count(sentiment) %>% dplyr:: mutate(total_words=sum(n), percent=n/total_words) #get percentage of words 
q1<-percents

file = paste0("Carol/cleandata/q1.Rda")
save(q1, file = file)


#(2) What are the most common words by chapter
#create stop words
stop=c(stopwords("en"), 'said', 'mr', 'mrs', 'he', 'she', 'it', 'they', 'i', 'you', 'professor', 'The', 'I', 'He', 'She', 'It', 'will')

#filter out stop words and then get their percentage per chapter, use only words with more than 1% per chapter
comm_words<-cs_book %>% dplyr::filter(!word %in% stop) %>% dplyr::group_by(chapter) %>% dplyr::count(word) %>% dplyr::arrange(desc(n)) %>% 
  dplyr::rename(count=n)%>% dplyr::mutate(total=sum(count), percent=count/total) %>% dplyr::filter(percent>0.01)
q2<-comm_words

file = paste0("Carol/cleandata/q2.Rda")
save(q2, file = file)

#(3) Which characters appear the most 
books = vector(mode = "list", length = 7)
for(i in 1:length(books)){
  data <- data_frame(text = book_names[[i]])
  data <- mutate(data, chapter = c(1:nrow(data)), Title = str_to_title(str_replace_all(names(book_names)[i], "_", " ")))
  data <- data %>%
    unnest_tokens(word, text, to_lower = F)
  books[[i]] <- data
}
#capitalize first letter of each word
cap_stop=stri_trans_totitle(stop)
#create pattern to grab 
pattern1=START %R% UPPER %R% optional(zero_or_more(ANY_CHAR) %R% SPC %R% UPPER)
Char_4<-str_subset(books[[4]]$word, pattern=pattern1)
#create a list of characters and then reduce to unique list
char_list<-data_frame(word=Char_4) %>% filter(!word %in% cap_stop) %>% unique()
#join the characters, calculate percentage per character
char_list_2<-right_join(books[[4]], char_list) %>% dplyr::count(word)  %>% dplyr::arrange(desc(n)) %>% dplyr::filter(n>100)%>% dplyr::mutate(total=sum(n), percent=n/total)
q3<-char_list_2

file = paste0("Carol/cleandata/q3.Rda")
save(q3, file = file)


#(4) How the emotion anticipation changes throughtout the book
#get anticipation
nrc <- get_sentiments("nrc") %>% filter(sentiment=='anticipation')
cs_book= books[[4]]
anti_4=inner_join(cs_book, nrc)#join anticipation
percents_4 <- anti_4 %>% dplyr::group_by(chapter) %>% dplyr::count(sentiment)

#create y-axis marks for labeling
y_label_above=(percents_4$n+10)
y_label_below=(percents_4$n-5)
differences=c(0,diff(percents_4$n))
y_label=rep(0, length(differences))
choose_label=cbind(y_label_below, y_label_above, differences, y_label)
#choose the axis points: if the point is higher than the previous point then place label above the point, if not, then below the point:
y_label=sapply(1:nrow(choose_label), function(x,y) if(y[x,3]>=0){y[x,4]=y[x,2]}else{y[x,4]=y[x,1]}, y=choose_label) 
y_label=data_frame(y_label)
y_label=y_label %>% mutate(chapter=row_number())
percents_4=inner_join(percents_4, y_label) #join the points
q4<-percents_4

file = paste0("Carol/cleandata/q4.Rda")
save(q4, file = file)


#(5) Word count for each book
word_count=c(rep(0,7))
for(i in 1:7){
  word_count[i]<-books[[i]] %>% dplyr::summarise(total_words=n())
}
word_count=data_frame(word_count=unlist(word_count))
word_count=bind_cols(data_frame(Title=str_to_title(str_replace_all(names(book_names), "_", " "))), word_count)
q5<-word_count

file = paste0("Carol/cleandata/q5.Rda")
save(q5, file = file)

#(6) Individual Question: How many beasts in the Fantastic Beasts book were mentioned in all the Harry Potter books?

#list the beasts
beasts<-c('Acromantula'
          ,'Ashwinder' 
          ,'Augurey' 
          ,'Basilisk' 
          ,'Billywig'
          ,'Bowtruckle'
          ,'Bundimun'
          ,'Centaur'
          ,'Chimaera'
          ,'Chizpurfle'
          ,'Clabbert'
          ,'Crup'
          ,'Demiguise'
          ,'Diricawl'
          ,'Doxy'
          ,'Dragon'
          ,'Opaleye'
          ,'Fireball'
          ,'Hebridean'
          ,'Horntail'
          ,'Ridgeback'
          ,'Vipertooth'
          ,'Longhorn'
          ,'Short-Snout'
          ,'Ironbelly'
          ,'Dugbog'
          ,'Erkling'
          ,'Erumpent'
          ,'Fairy'
          ,'Crab'
          ,'Flobberworm'
          ,'Fwooper'
          ,'Ghoul'
          ,'Glumbumble'
          ,'Gnome'
          ,'Graphorn'
          ,'Griffin'
          ,'Grindylow'
          ,'Hidebehind'
          ,'Hippocampus'
          ,'Hippogriff'
          ,'Hodag'
          ,'Horklump'
          ,'Imp'
          ,'Jarvey'
          ,'Jobberknoll'
          ,'Kappa'
          ,'Kelpie'
          ,'Knarl'
          ,'Kneazle'
          ,'Leprechaun'
          ,'Lethifold'
          ,'Lobalug'
          ,'Malaclaw'
          ,'Manticore'
          ,'Merpeople'
          ,'Moke'
          ,'Mooncalf'
          ,'Murtlap'
          ,'Niffler'
          ,'Nogtail'
          ,'Nundu'
          ,'Occamy'
          ,'Phoenix'
          ,'Pixie'
          ,'Plimpy'
          ,'Pogrebin'
          ,'Porlock'
          ,'Puffskein'
          ,'Quintaped'
          ,'Ramora'
          ,'Red Cap'
          ,'Reem'
          ,'Runespoor'
          ,'Salamander'
          ,'Serpent'
          ,'Shrake'
          ,'Snallygaster'
          ,'Snidget'
          ,'Sphinx'
          ,'Streeler'
          ,'Tebo'
          ,'Thunderbird'
          ,'Troll'
          ,'Unicorn'
          ,'Wampus'
          ,'Werewolf'
          ,'Winged horse'
          ,'Yeti')
beasts=data_frame(word=beasts, beasts=beasts)

#join the beasts in each book
hpbeasts<-lapply(1:7, function(x) inner_join(books[[x]], beasts))
hpbeasts<-bind_rows(hpbeasts) #put the lists into one dataset
hpbeasts <- hpbeasts %>% dplyr::group_by(Title) %>% dplyr::count(beasts) %>% dplyr:: mutate(total_words=sum(n), percent=n/total_words, y_label=cumsum(percent))
q6<-hpbeasts

file = paste0("Carol/cleandata/q6.Rda")
save(q6, file = file)
