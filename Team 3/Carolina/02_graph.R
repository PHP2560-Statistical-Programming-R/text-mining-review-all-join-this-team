load("Carolina/clean_data/saga.rda")
load("Carolina/clean_data/book2.rda")
dir.create("graph/", showWarnings = FALSE)

library(tidytext)
library(dplyr)
library(stringr)
library(harrypotter)
library(ggplot2)
library(tidyr)
library(rebus)


########### Group Questions #############

# Question 1: What are the most common words by chapter in the book "Chamber of Secrets"?
common.words1 <- book2 %>% 
  # Remove stop words like articles
  anti_join(stop_words) %>%
  group_by(chapter) %>%
  # Use count to find out the 5 words used the most in each chapter
  count(word, sort=TRUE) %>%
  top_n(5) %>%
  arrange(chapter)

# Plot 
ggplot(common.words1, aes(x = reorder(word, n), y = n, fill = chapter)) +
  # Make a bar chart without a legend
  geom_col(show.legend=FALSE) +
  # Plot for each chapter 
  facet_wrap(~chapter, scales="free") + 
  # Flip the axes
  coord_flip() 

# Save plot
ggsave("Carolina/graph/plot1.png")


# Question 3: What are the most common words in the entire book "Chamber of Secrets"?
common.words2 <- book2 %>% 
  # Remove stop words like articles
  anti_join(stop_words) %>%
  # Use count to find out the 10 words used the most in the book
  count(word, sort=TRUE) %>%
  top_n(10)

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(common.words2, aes(x=reorder(word, n), y=n)) +
  # Make a bar chart with geom_col()
  geom_col(fill="salmon") +  
  # Flip the axes
  coord_flip() + 
  # Add a title
  ggtitle("Words Mentioned the Most in Chamber of Secrets") +
  # Center the title
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot
ggsave("Carolina/graph/plot2.png")


# Question 3: How does sentiment change across the entire book? *Using afinn lexicon*
score.per.chapter <- book2 %>%
  # Count by chapter and word
  count(chapter, word, sort = TRUE) %>%
  # Implement sentiment analysis with the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>% 
  group_by(chapter) %>%
  # Find the net score for each chapter 
  summarise(net.score = sum(score))

# Plot 
ggplot(score.per.chapter, aes(x = chapter, y = net.score)) +
  # Add a reference line y=0
  geom_hline(aes(yintercept = 0, color="Reference line y=0"), size = 0.4) +
  # Add the line of sentiment change
  geom_line(size=1, aes(color="Change of sentiment accross chapters")) +
  # Add lm smoothing
  geom_smooth(method = "lm", se = FALSE, lty = 2, size = 0.7, aes(color="Lm smoothing")) +
  # Add loess smoothing
  geom_smooth(method = "loess", se = FALSE, lty = 2, size = 0.7, aes(color="Loess smoothing")) +
  # Modify the breaks and add titles to the axes
  scale_x_continuous("Chapter", breaks=c(1:19)) +
  scale_y_continuous("Sentiment Score") +
  # Add a title
  ggtitle("Sentiment change accross Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot
ggsave("Carolina/graph/plot3.png")



# Question 4: Which 10 characters are mentioned the most? *Code using text mining*
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)


# First, we need to tidy the data again to make it a dataframe and keep upper cases. This will help us to identify proper names. 
saga.upper.case <- vector(mode = "list", length = 7)

for(i in 1:length(saga.upper.case)){
  # convert text to dataframe
  data <- data_frame(text = books[[i]])
  # add column for chapter numbers and title of book
  data <- mutate(data, chapter = c(1:nrow(data)), book = titles[i])
  # split by word and remove punctuation
  data <- data %>%
    # include upper case words  
    unnest_tokens(word, text, to_lower = FALSE)
  # store clean data to list
  saga.upper.case[[i]] <- data
}

# make one data frame from list of data frames
saga.upper.case <- plyr::ldply(saga.upper.case, data.frame)


# Filter data for Chamber of Secrets
book2.upper.case <- saga.upper.case %>%
  filter(book=="Chamber of Secrets")


characters2 <- book2.upper.case %>%
  select(word) %>%
  # regular expression for character names
  filter(str_detect(string = word, pattern = UPPER %R% ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR)) %>%
  # remove some common words that aren't character names
  filter(!word %in% c( "The","They","What","There","It's","Then")) %>%
  # get count of character names
  group_by(word) %>%
  summarise(n = n()) %>%
  top_n(10)


# Plot  
ggplot(characters2, aes(x=reorder(word, n), y=n)) +
  # Make a bar chart with geom_col()
  geom_col(fill="salmon") +
  # Change axes names
  labs(x="Characters", y="Number of appereances") +
  # Add a title
  ggtitle("Top 10 characters mentioned the most in Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add counts next to the columns
  geom_text(aes(label=paste0(n)), nudge_y=1) +
  # Flip axes
  coord_flip()

# Save plot
ggsave("Carolina/graph/plot4.png")



# Question 5: How does the emotion "anticipation" change throughtout the book?

anticipation <- book2 %>%
  # Implement sentiment analysis with the "ncr" lexicon
  inner_join(get_sentiments("nrc"))  %>%
  # Count the words by chapter
  group_by(chapter, sentiment) %>% 
  count(word) %>%
  # Get the total occurrences per sentiment
  summarise(word.occurence = sum(n)) %>%
  # Get the total occurrences of all sentiments by chapter 
  mutate(total.words = sum(word.occurence)) %>%
  # Filter the sentiment anticipation
  filter(sentiment == "anticipation") %>%
  # Compute the proportion of occurrence by chapter
  mutate(proportion = word.occurence/total.words)

# Plot the RELATIVE change of the sentiment anticipation accross chapters
ggplot(anticipation, aes(x=chapter,y=proportion)) + 
  geom_line(size=1, aes(color="Relative change of \n anticipation accross chapters")) +
  # Add lm smoothing
  geom_smooth(method = "lm", se = FALSE, lty = 2, size = 0.7, aes(color="Lm smoothing")) +
  # Add loess smoothing
  geom_smooth(method = "loess", se = FALSE, lty = 2, size = 0.7, aes(color="Loess smoothing")) +
  # Modify the breaks and add titles to the axes
  scale_x_continuous("Chapter", breaks=c(1:19)) +
  scale_y_continuous("Relative Anticipation") +
  # Add a title
  ggtitle("Relative anticipation change accross Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5))

# Save plot
ggsave("Carolina/graph/plot5.png")



########### Individual Questions #############

# Question 1: Which are the most common positive and negative words in the book "Chamber of Secrets"?
word_counts <- book2 %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(x=word, y=n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  # Make a plot for each sentiment: positive and negative
  facet_wrap(~sentiment, scales = "free") +  
  # Flip the axes
  coord_flip()

# Save plot
ggsave("Carolina/graph/plot6.png")



# Question 2: Which words are the most negative and positive of the book "Chamber of Secrets"?
negative <- book2 %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Select unique word and score columns
  select(word, score)%>%
  unique() %>%
  # Filter to only examine the scores that are negative
  filter(score<0) %>%
  arrange(score) %>%
  # Select the 10 words that contribute the most
  head(10)

# Use aes() to put words on the x-axis and score on the y-axis
p1 <- ggplot(negative, aes(x=reorder(word, -score), y=factor(score))) +
  # Make a bar chart with geom_col()
  geom_col(fill="lightblue") +
  # Add titles to axes and modify the breaks
  scale_y_discrete("Score", limits=c(0, -1, -2, -3, -4) ) +
  scale_x_discrete("Word") +
  # Flip the axes
  coord_flip() +   
  # Add score next to the column
  geom_text(aes(label=paste0(score)), nudge_y=0) +
  # Add a title
  ggtitle("10 Most Negative Words") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5))

positive <- book2 %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Select unique word and score columns
  select(word, score)%>%
  unique() %>%
  # Filter to only examine the scores that are positive
  filter(score>0) %>%
  arrange(desc(score)) %>%
  # Select the 10 words that contribute the most
  head(10)

# Use aes() to put words on the x-axis and score on the y-axis
p2 <- ggplot(positive, aes(x=reorder(word, score), y=factor(score))) +
  # Make a bar chart with geom_col()
  geom_col(fill="salmon") +
  # Add titles to axes and modify the breaks
  scale_y_discrete("Score", limits=c(0, 1, 2, 3, 4, 5)) +
  scale_x_discrete("Word") +
  # Flip the axes
  coord_flip() + 
  # Add score next to the column
  geom_text(aes(label=paste0(score)), nudge_y=0) +
  # Add a title
  ggtitle("10 Most Positive Words") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5))

# Display the two plots together
library(gridExtra)
grid.arrange(p1,p2,ncol=2)

# Save plot
ggsave("Carolina/graph/plot7.png")



# Question 3: What is the percentage of positive words for each chapter? 
sentiment_counts <- book2 %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count the number of words by title, type, and sentiment
  count(chapter, sentiment)

perc.words <- sentiment_counts %>%
  # Group by the chapter number
  group_by(chapter) %>%
  # Find the total number of words in each chapter
  mutate(total = sum(n),
         # Calculate the number of words divided by the total
         percent = n/total) %>%
  # Filter the results for only negative sentiment
  filter(sentiment=="positive") %>%
  arrange(desc(percent))

# Plot  
ggplot(perc.words, aes(x = chapter, y = percent)) +
  geom_line(color="purple") +
  # Modify the breaks and add titles to the axes
  scale_x_continuous("Chapter", breaks=c(1:19)) +
  scale_y_continuous("Percentage") +
  # Add a title
  ggtitle("Percentage of positive and negative words per \n chapter in Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) 

# Save plot
ggsave("Carolina/graph/plot8.png")



# Questions 4: How does sentiment change across the entire saga? 
score.per.book <- saga %>%
  # Count by chapter and word
  count(book, word, sort = TRUE) %>%
  # Implement sentiment analysis with the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>% 
  group_by(book) %>%
  # Find the net score for each chapter 
  summarise(net.score = sum(score))

# Plot 
# Setting group=1 in aes() ensures that all values are treated as one group
ggplot(score.per.book, aes(x = book, y = net.score, group=1, color=group)) +
  # Add the line of sentiment change
  geom_line(color="steelblue", size=1) +
  # Add lm smoothing
  geom_smooth(method = "lm", se = FALSE, lty = 2, size = 0.7, color="purple") +
  # Add loess smoothing
  geom_smooth(method = "loess", se = FALSE, lty = 2, size = 0.7, color="red") +
  # Add titles to the axes 
  scale_x_discrete("Book Title") +
  scale_y_continuous("Sentiment Score") +
  # Make the names in the x-axis appear vertically
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # Add a title
  ggtitle("Sentiment change accross the Harry Potter Saga") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) + 
  # Add legend
  scale_color_manual(name="Legend", values=c("Change of sentiment accross the saga" = "steelblue", "Lm smoothing" = "purple", "Loess smoothing" = "red"))

# Save plot
ggsave("Carolina/graph/plot9.png")



