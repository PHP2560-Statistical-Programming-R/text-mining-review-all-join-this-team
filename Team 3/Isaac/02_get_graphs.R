dir.create("./Isaac/graphs/", showWarnings = FALSE)   # Create a directory to store graph files
load("./Isaac/data/POA_lower.Rda")   # Load data needed for questions 1-3

### Question 1: How does negative sentiment changes across Prisoner of Azkaban compared to positive sentiment?

POA_lower %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(chapter,sentiment) %>%
    dplyr::summarize(count=n()) %>%
    mutate(proportion=count/sum(count)) %>%

ggplot(aes(x=chapter,y=proportion,color=sentiment)) +
    geom_path() +
    scale_color_brewer(palette='Set1') +
    geom_smooth(se=FALSE,linetype = "dashed")

ggsave('./Isaac/graphs/Q1.png', width=7)

# ----------------------------------------------------------------------------- 

### Question 2: How does emotional anticipation change throughout Prisoner of Azkaban?
POA_lower %>%
    inner_join(get_sentiments("nrc"))%>%
    group_by(chapter,sentiment) %>%
    dplyr::count(word) %>%
    dplyr::summarize(word_occurence=sum(n))%>%
    mutate(total_words=sum(word_occurence)) %>%
    filter(sentiment=="anticipation") %>%
    mutate(proportion= word_occurence/total_words) %>%

ggplot(aes(x=chapter,y=proportion)) +
    geom_line(color="deeppink") +
    geom_smooth(se=FALSE,linetype = "dashed")

ggsave('./Isaac/graphs/Q2.png', width=7)

# ----------------------------------------------------------------------------- 

### Question 3: What are the top 10 most common words in Prisoner of Azkaban?

POA_lower %>%
    filter(!(word %in% c("He","I","The"))) %>%
    anti_join(stop_words)%>%
    group_by(word)%>%
    dplyr::summarize(count=n()) %>%
    arrange(desc(count)) %>%
    filter(row_number() %in% 1:10) %>%
    
ggplot(aes(x=reorder(word,count),y=count)) +   # Order from greatest to least
    geom_col(fill="red",color="darkred") +   # Color bars
    xlab("Words") +   # Label the x axis
    geom_text(aes(label = paste0(count)), nudge_y = 1) +   # Put frequency labels next to bars  
    scale_y_continuous(breaks = seq(from = 0, to = 2000, by= 250)) +   # Specify tick mark labels every 250 counts
    coord_flip()   # Flip x and y axis

ggsave('./Isaac/graphs/Q3.png',width=7)

# ----------------------------------------------------------------------------- 

### Question 4: Who are the 10 characters that appear the most in Prisoner of Azkaban?

load("./Isaac/data/POA_upper.Rda")   # Load data needed for question 4

POA_upper %>% 
    select(word) %>% 
    filter(str_detect(word, UPPER %R% ANY_CHAR %R% ANY_CHAR)) %>%   # regular expression to filter for character names
    # remove words that aren't character names
    filter(!(word %in% c(stop_words, "I'm","Black", "You", "And", "But", "The","They","What","There","It's","Then", "Harry's", "Professor", "Gryffindor", "Scabbers", "Fudge"))) %>%   
    # get frequency for most popular character names
    group_by(word) %>%
    dplyr::summarize(count=n()) %>%
    arrange(desc(count)) %>%
    filter(row_number() %in% 1:10) %>%

# Plotting for top 9 names
ggplot(aes(x=reorder(word,count),y=count)) + 
    geom_col(fill="cyan",color="blue") +
    xlab("Characters") +
    # Put frequency labels next to bars  
    geom_text(aes(label = paste0(count)), nudge_y = 1) +
    coord_flip()

ggsave('./Isaac/graphs/Q4.png',width=7)

# ----------------------------------------------------------------------------- 

load("./Isaac/data/books_lower.Rda")   # Load data needed for questions 5,6
ordered_books <- data.frame(title=c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows"),
                            order=c(1,2,3,4,5,6,7))   # Assigns order to titles for questions 5,6

### Question 5: What are the total number of words for each book?

books_lower_ordered <-
    books_lower %>%
    group_by(title) %>%
    dplyr::summarize(total_words=n()) %>%
    inner_join(ordered_books,by="title") %>%
    arrange(desc(order))

# Need title as a factor type to arrange graph in correct order
books_lower_ordered$title <- factor(books_lower_ordered$title, levels = books_lower_ordered$title)

ggplot(books_lower_ordered, aes(x=title,y=total_words,fill=title)) +
    geom_col() +
    xlab("Book Title") +
    scale_y_continuous(breaks = seq(from = 0, to = 300000, by= 50000)) +   # Specify tick mark labels every 50,000 counts
    ylab("Total Word Count") +
    # Removes legend
    theme(legend.position="none") + 
    geom_text(aes(label = paste0(total_words)), nudge_y = 1) +
    coord_flip()

ggsave('Isaac/graphs/Q5.png',width=7)

# ----------------------------------------------------------------------------- 

### Isaac's Unique Question 6: What are the total number of spells used in each book?

spells <- 
    c("accio","aguamenti","alohomora","aparecium","avada kedavra","avifors","avis","bombarda","colloportus","confringo","confundus","conjunctivitis","crucio",
      "deletrius","densaugeo","diffindo","dissendium","duro","enervate","engorgio","episkey","evanesco","expecto patronum","expelliarmus", 
      "fera verto","ferula","fidelius","finite incantatem","flagrate","flipendo","furnunculus","geminio","homorphus",
      "immobulus","impedimenta","imperio","impervius","incarcerous","incendio","legilimens","levicorpus","liberacorpus","locomotor mortis","lumos",
      "mobiliarbus","mobilicorpus","morsmordre","muffliato","nox","obliviate","orchideous","petrificus totalus","point me the four point spell","portus,prior incantato","protego", 
      "quietus","reducio","reducto","relashio","rennervate","reparo","repello","repello muggletum","revelio","rictusempra","riddikulus",
      "salvio hexia","scourgify","sectumsempra","serpensortia","silencio","sonorus","stupefy","tarantallegra","tergeo","waddiwasi","wingardium leviosa")

spell.df <- data.frame(spells)

book.spells <- 
    books_lower %>% 
    inner_join(spell.df, by=c("word" = "spells")) %>%
    group_by(title) %>%
    dplyr::summarize(count=n()) %>%
    # List books in original order    
    inner_join(ordered_books,by="title") %>%
    arrange(desc(order))

# Need title as a factor type to arrange graph in correct order
book.spells$title <- factor(book.spells$title, levels = book.spells$title)

ggplot(book.spells,aes(x=title,y=count,fill=title)) +
    geom_col()+
    xlab("Book Title") +
    # Removes legend
    theme(legend.position="none") +
    geom_text(aes(label = paste0(count)), nudge_y = 1) +
    coord_flip()

ggsave('Isaac/graphs/Q6.png',width=7)
