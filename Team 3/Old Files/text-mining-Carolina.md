Text Analyis
================

Your mission
============

Perform text analysis.

Okay, I need more information
-----------------------------

Perform sentiment analysis or topic modeling using text analysis methods as demonstrated in the pre-class work and in the readings.

Okay, I need even more information.
-----------------------------------

Do the above. Can't think of a data source?

-   `gutenbergr`
-   `AssociatedPress` from the `topicmodels` package
-   `NYTimes` or `USCongress` from the `RTextTools` package
-   Harry Potter Complete 7 Books text \`\`\` if (packageVersion("devtools") &lt; 1.6) { install.packages("devtools") }

devtools::install\_github("bradleyboehmke/harrypotter") \`\``- [State of the Union speeches](https://pradeepadhokshaja.wordpress.com/2017/03/31/scraping-the-web-for-presdential-inaugural-addresses-using-rvest/) - Scrape tweets using [`twitteR\`\](<https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/>)

Analyze the text for sentiment OR topic. **You do not need to do both**. The datacamp courses and [Tidy Text Mining with R](http://tidytextmining.com/) are good starting points for templates to perform this type of analysis, but feel free to *expand beyond these examples*.

Timelines and Task
==================

We will spend the next 2 weeks working on analyzing textual data in R. You will do the following:

-   Start with some text based data.
-   Clean data and prepare it for analysis
-   Ask questions about the data
-   Answer these questions with the data using tables and graphics
-   Each group member must have their own unique question that they code the answer for.

``` r
library(dplyr)
devtools::install_github("bradleyboehmke/harrypotter")
```

Data cleaning:
==============

``` r
library(tidytext)
library(dplyr)
library(stringr)
library(harrypotter)
library(ggplot2)
library(tidyr)
library(rebus)
```

*Clean one book first and then generalize*

``` r
# Create tibble
data <- tibble(text = half_blood_prince) # column of text
data <- mutate(data, book = "Half Blood Prince") # define a new column for book name
data <- mutate(data, chapter = c(1:nrow(data))) # define a new column for chapter number

head(data)
```

    ## # A tibble: 6 x 3
    ##                                                                                 text
    ##                                                                                <chr>
    ## 1 <U+00A0>It was nearing midnight and the Prime Minister was sitting alone in his of
    ## 2 <U+00A0>Many miles away the chilly mist that had pressed against the Prime Ministe
    ## 3 <U+00A0>Harry Potter was snoring loudly. He had been sitting in a chair beside his
    ## 4 <U+00A0>Despite the fact that he had spent every waking moment of the past few day
    ## 5 <U+00A0>Harry and Dumbledore approached the back door of the Burrow, which was sur
    ## 6 <U+00A0>Harry remained within the confines of the Burrow's garden over the next fe
    ## # ... with 2 more variables: book <chr>, chapter <int>

``` r
data %>%
  unnest_tokens(word, text) # Transform the non-tidy text data to tidy text data
```

    ## # A tibble: 171,586 x 3
    ##                 book chapter     word
    ##                <chr>   <int>    <chr>
    ## 1  Half Blood Prince       1       it
    ## 2  Half Blood Prince       1      was
    ## 3  Half Blood Prince       1  nearing
    ## 4  Half Blood Prince       1 midnight
    ## 5  Half Blood Prince       1      and
    ## 6  Half Blood Prince       1      the
    ## 7  Half Blood Prince       1    prime
    ## 8  Half Blood Prince       1 minister
    ## 9  Half Blood Prince       1      was
    ## 10 Half Blood Prince       1  sitting
    ## # ... with 171,576 more rows

*Generalize this to all books to get dataset of the saga:*

``` r
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
           goblet_of_fire, order_of_the_phoenix, half_blood_prince,
           deathly_hallows)

saga <- tibble()

for (i in seq_along(titles)) { # create a tibble for each book
  data <- tibble(text = books[[i]]) %>% # add text
    mutate(book = titles[i]) %>% # define a new column for book name
    mutate(chapter = seq_along(books[[i]])) %>% # define a new column for chapter number
    group_by(book) %>% # group by book title
    unnest_tokens(word, text) %>%  # transform the non-tidy text data to tidy text data
    ungroup()
  
  saga <- rbind(saga, data) # bind rows of tibble of each book
}

saga
```

    ## # A tibble: 1,090,690 x 3
    ##                   book chapter    word
    ## *                <chr>   <int>   <chr>
    ## 1  Philosopher's Stone       1     the
    ## 2  Philosopher's Stone       1     boy
    ## 3  Philosopher's Stone       1     who
    ## 4  Philosopher's Stone       1   lived
    ## 5  Philosopher's Stone       1      mr
    ## 6  Philosopher's Stone       1     and
    ## 7  Philosopher's Stone       1     mrs
    ## 8  Philosopher's Stone       1 dursley
    ## 9  Philosopher's Stone       1      of
    ## 10 Philosopher's Stone       1  number
    ## # ... with 1,090,680 more rows

Book 2: Chamber of Secrets
==========================

I will focus my analysis on the second book of the saga called "Chamber of Secrets"

``` r
book2 <- saga %>%
  filter(book=="Chamber of Secrets")
```

Group questions:
----------------

1.  What is the word count for each book?

``` r
# Count the number of words per book
saga %>%
  group_by(book) %>%
  summarize(total.words = n())
```

    ## # A tibble: 7 x 2
    ##                   book total.words
    ##                  <chr>       <int>
    ## 1   Chamber of Secrets       85408
    ## 2      Deathly Hallows      199738
    ## 3       Goblet of Fire      191938
    ## 4    Half-Blood Prince      171586
    ## 5 Order of the Phoenix      258853
    ## 6  Philosopher's Stone       77876
    ## 7  Prisoner of Azkaban      105291

1.  What is the word count of each chapter of the book "Chamber of Secrets"?

``` r
book2 %>%
  # Count the number of words per chapter
  group_by(chapter) %>%
  count()
```

    ## # A tibble: 19 x 2
    ##    chapter     n
    ##      <int> <int>
    ## 1        1  2595
    ## 2        2  2894
    ## 3        3  4509
    ## 4        4  5814
    ## 5        5  5202
    ## 6        6  5018
    ## 7        7  3911
    ## 8        8  4681
    ## 9        9  5181
    ## 10      10  5325
    ## 11      11  6019
    ## 12      12  4510
    ## 13      13  3242
    ## 14      14  2394
    ## 15      15  4303
    ## 16      16  2550
    ## 17      17  5276
    ## 18      18  3416
    ## 19      19  8568

1.  What are the most common words by chapter in the book "Chamber of Secrets"?

``` r
common.words <- book2 %>% 
  # Remove stop words like articles
  anti_join(stop_words) %>%
  group_by(chapter) %>%
  # Use count to find out the 5 words used the most in each chapter
  count(word, sort=TRUE) %>%
  top_n(5) %>%
  arrange(chapter)
```

    ## Joining, by = "word"

    ## Selecting by n

``` r
# Plot 
ggplot(common.words, aes(x = reorder(word, n), y = n, fill = chapter)) +
  # Make a bar chart without a legend
  geom_col(show.legend=FALSE) +
  # Plot for each chapter 
  facet_wrap(~chapter, scales="free") + 
  # Flip the axes
  coord_flip() 
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-8-1.png)

1.  What are the most common words in the entire book "Chamber of Secrets"?

``` r
common.words <- book2 %>% 
  # Remove stop words like articles
  anti_join(stop_words) %>%
  # Use count to find out the 10 words used the most in the book
  count(word, sort=TRUE) %>%
  top_n(10)
```

    ## Joining, by = "word"

    ## Selecting by n

``` r
# Use aes() to put words on the x-axis and n on the y-axis
ggplot(common.words, aes(x=reorder(word, n), y=n)) +
  # Make a bar chart with geom_col()
  geom_col(fill="salmon") +  
  # Flip the axes
  coord_flip() + 
  # Add a title
  ggtitle("Words Mentioned the Most in Chamber of Secrets") +
  # Center the title
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-9-1.png)

1.  How does sentiment change across the entire book? *Using afinn lexicon*

``` r
score.per.chapter <- book2 %>%
  # Count by chapter and word
  count(chapter, word, sort = TRUE) %>%
  # Implement sentiment analysis with the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>% 
  group_by(chapter) %>%
  # Find the net score for each chapter 
  summarise(net.score = sum(score))
```

    ## Joining, by = "word"

``` r
score.per.chapter
```

    ## # A tibble: 19 x 2
    ##    chapter net.score
    ##      <int>     <int>
    ## 1        1        -9
    ## 2        2       -43
    ## 3        3        -7
    ## 4        4        31
    ## 5        5       -30
    ## 6        6        20
    ## 7        7         3
    ## 8        8         2
    ## 9        9       -70
    ## 10      10       -72
    ## 11      11       -60
    ## 12      12       -46
    ## 13      13       -14
    ## 14      14       -32
    ## 15      15       -44
    ## 16      16       -22
    ## 17      17       -64
    ## 18      18       -33
    ## 19      19       -65

``` r
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
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-10-1.png)

*Using bing lexicon*

``` r
sentiment.count <- book2 %>%  
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing")) %>% 
  # Count the sentiment by chapter and negative
  group_by(chapter,sentiment) %>% 
  count(chapter, sentiment)  
```

    ## Joining, by = "word"

``` r
sentiment.prop <-  sentiment.count %>%
  # Find the proportion of the total words that are positive and negative by chapter
  group_by(chapter) %>%
  mutate(total.words=sum(n), p = n/total.words)  

# Plot  
ggplot(sentiment.prop, aes(x = chapter, y = p, col = as.factor(sentiment))) +
  geom_line() +
  # Modify the breaks and add titles to the axes
  scale_x_continuous("Chapter", breaks=c(1:19)) +
  scale_y_continuous("Sentiment Score") +
  # Add a title
  ggtitle("Negative and positive sentiment change accross Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-11-1.png)

1.  Which 10 characters are mentioned the most? *Code using a vector of character names*

``` r
# List of characters (not exhaustive)
characters1 <- tibble(word=c("harry", "ron", "hermione", "voldemort", "riddle", "hagrid", "dumbledore", "draco", "lucius", "ginny", "percy", "fred", "george", "molly", "arthur", "vernon", "petunia", "dudley", "minerva", "snape", "lockhart", "sprout", "filch", "norris", "cornelius", "oliver", "colin", "neville", "goyle", "crabbe", "millicent", "peeves", "hedwig", "aragog", "fang", "myrtle", "nick", "mason", "dobby", "mafalda", "errol", "perkins", "celestina", "christmas", "mundungus", "mortlake", "cannons", "martin", "borgin", "witch", "granger", "filibuster", "bozo", "hetty", "angus", "colin", "creevey", "banshee", "ogden", "gudgeon", "veronica", "patrick", "ghost", "widow", "ghost", "skower", "gryffindor", "hufflepuff", "ravenclaw", "slytherin", "yeti", "vampire", "werewolf", "ernie", "fawcett", "sinistra", "fawkes", "penelope", "mabel", "dwarf", "dippet", "aragog", "mosag", "hornby"))


no.stop.words <- book2 %>% 
  # Remove stop words like articles
  anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
top.characters <- no.stop.words %>%
  # Semi-join no.stop.words and characters to sort only words that match with character names
  semi_join(characters1) %>%
  # Use count and top_n to find out the 10 characters that are mentioned the most
  count(word, sort=TRUE) %>%
  top_n(10) 
```

    ## Joining, by = "word"

    ## Selecting by n

``` r
# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top.characters, aes(x=reorder(word, n), y=n)) +
  # Make a bar chart with geom_col()
  geom_col(fill="salmon") +  
  # Change axes names
  labs(x="Characters", y="Number of appereances") +
  # Add count next to the bars
  geom_text(aes(label=paste0(n)), nudge_y=1) +
  # Add a title
  ggtitle("Top 10 characters mentioned the most in Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) +
  # Flip the axes
  coord_flip()
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-12-1.png)

*Code using text mining*

``` r
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
```

    ## Selecting by n

``` r
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
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-13-1.png)

1.  How does the emotion "anticipation" change throughtout the book?

``` r
anticipation1 <- book2 %>%
  # Implement sentiment analysis with the "nrc" lexicon
  inner_join(get_sentiments("nrc"))  %>%
  # Count the sentiment by chapter
  group_by(chapter) %>%
  count(sentiment) %>%
  # Filter sentiment anticipation
  filter(sentiment == "anticipation")
```

    ## Joining, by = "word"

``` r
# Plot the change of the sentiment anticipation accross chapters
ggplot(anticipation1, aes(x=chapter, y=n)) +
geom_col(fill = "orange", width  = 0.7)
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
anticipation2 <- book2 %>%
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
```

    ## Joining, by = "word"

``` r
# Plot the RELATIVE change of the sentiment anticipation accross chapters
ggplot(anticipation2, aes(x=chapter,y=proportion)) + 
  geom_line(size=1, aes(color="Relative change of anticipation accross chapters")) +
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
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-14-2.png)

Individual questions:
---------------------

1.  Which are the most common positive and negative words in the book "Chamber of Secrets"?

``` r
word_counts <- book2 %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)
```

    ## Joining, by = "word"

``` r
top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))
```

    ## Selecting by n

``` r
# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(x=word, y=n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  # Make a plot for each sentiment: positive and negative
  facet_wrap(~sentiment, scales = "free") +  
  # Flip the axes
  coord_flip()
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-15-1.png)

1.  Which words are the most negative and positive of the book "Chamber of Secrets"?

``` r
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
```

    ## Joining, by = "word"

``` r
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
  ggtitle("10 Most Negative Words in the Chamber of Secrets") +
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
```

    ## Joining, by = "word"

``` r
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
  ggtitle("10 Most Positive Words in the Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5))

# Display the two plots together
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
grid.arrange(p1,p2,ncol=2)
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-16-1.png)

1.  What is the percentage of positive words for each chapter?

``` r
sentiment_counts <- book2 %>%
    # Implement sentiment analysis using the "bing" lexicon
    inner_join(get_sentiments("bing")) %>%
    # Count the number of words by title, type, and sentiment
    count(chapter, sentiment)
```

    ## Joining, by = "word"

``` r
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
  geom_line() +
  # Modify the breaks and add titles to the axes
  scale_x_continuous("Chapter", breaks=c(1:19)) +
  scale_y_continuous("Percentage") +
  # Add a title
  ggtitle("Percentage of positive and negative words per chapter in Chamber of Secrets") +
  # Center de title
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-17-1.png)

1.  How does sentiment change across the entire saga?

``` r
score.per.book <- saga %>%
  # Count by chapter and word
  count(book, word, sort = TRUE) %>%
  # Implement sentiment analysis with the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>% 
  group_by(book) %>%
  # Find the net score for each chapter 
  summarise(net.score = sum(score))
```

    ## Joining, by = "word"

``` r
score.per.book
```

    ## # A tibble: 7 x 2
    ##                   book net.score
    ##                  <chr>     <int>
    ## 1   Chamber of Secrets      -285
    ## 2      Deathly Hallows      -403
    ## 3       Goblet of Fire      -367
    ## 4    Half-Blood Prince      -444
    ## 5 Order of the Phoenix      -467
    ## 6  Philosopher's Stone      -192
    ## 7  Prisoner of Azkaban      -306

``` r
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
```

![](text-mining-Carolina_files/figure-markdown_github/unnamed-chunk-18-1.png)
