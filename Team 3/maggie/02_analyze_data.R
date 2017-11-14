# remove stop words
books_no_stop <- anti_join(books, stop_words)

# create directory for plots
dir.create("./maggie/plots/", showWarnings = FALSE)


# sentiment over time
my_book <- filter(books, title == book_title)
afinn <- inner_join(my_book, get_sentiments("afinn"), by = "word")
scores <- afinn %>%
  group_by(chapter) %>%
  summarize(score = sum(score)) # sum sentiment score in each chapter

# create plot
png("maggie/plots/sentiment_over_time.png", width = 650, height = 480)

print(
  ggplot(scores, aes(x = chapter, y = score)) +
  geom_hline(yintercept = 0, col = "red") +
  geom_line(col = "#525252") +
  geom_smooth(method="loess", se = FALSE) + 
  labs(title = paste0("Sentiment Throughout ", book_title), x = "Chapter in series", y = "Sentiment Score") +
  ylim(c(-abs(max(abs(scores$score))), abs(max(abs(scores$score))))) # set y limits so the x-axis is centered
)

dev.off()


# most common words
most_common_words <- books_no_stop %>%
  count(word, sort = TRUE) %>% # count word frequency and sort
  head(10) # select first 10
# reorder factor for plotting in order
most_common_words$word <- factor(most_common_words$word, levels = most_common_words$word[order(-most_common_words$n)])

# plot
png("./maggie/plots/most_common_words.png", width = 650, height = 480)
print(
  ggplot(most_common_words, aes(word, n)) + 
  geom_col() +
  labs(y="frequency", title = paste0("Most Common Words in ", book_title))
)
dev.off()


# anticipation tracking
nrc <- inner_join(books_no_stop, get_sentiments("nrc")) %>%
  filter(title == book_title, sentiment == "anticipation") # filter for chosen book and words with anticipation sentiment only
percents <- group_by(nrc, chapter) %>%
  summarize(n_anticipation = n()) # count total anticipation words

total_words <- filter(books_no_stop, title == book_title) %>%
  group_by(chapter) %>%
  summarize(n_total = n()) # get total number of words per chapter

percents <- mutate(percents, n_total = total_words$n_total, percent = n_anticipation/n_total) # adjust for varying word count per chapter

# plot 
png("./maggie/plots/anticipation.png", width = 650, height = 480)
print(
  ggplot(percents, aes(x = chapter, y = percent)) +
    geom_line(col = "#525252") +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste0("Anticipation in ", book_title), x = "Chapter", y = "Percent of Words with Anticipation Sentiment")
)
dev.off()


# word count per chapter
png("./maggie/plots/word_count.png", width = 650, height = 480)
print(
  filter(books_no_stop, title == book_title) %>% # filter for chosen book
  group_by(chapter) %>%
  summarize(word_count = n()) %>% # count words per chapter
  ggplot(aes(chapter, word_count)) +
  geom_col()  +
  geom_smooth(se = FALSE) +
  labs(title = "Word Count per Chapter", x = "Chapter Number", y = "Word Count")
)
dev.off()

# common characters
books_upper <- anti_join(books_upper, stop_words) # remove stop words from data with uppercase preserved (only stop words in all lowercase will be removed)
my_book <- filter(books_upper, title == book_title) # filter for chosen book
upper_words <- str_subset(my_book$word, "^[A-Z]") # subset only the words that start with a capital letter
to_separate <- upper_words[grepl("[.]", upper_words)] # separate words that are two words separated by a period
upper_words <- upper_words[!grepl("[.]", upper_words)] # words that are valid (no period)
upper_words <- c(upper_words, unlist(strsplit(to_separate, "[.]")))  # split the words that need to be separated by .
upper_words <- tolower(upper_words) # convert all words to lower case
upper_words <- gsub("'.*$", "", upper_words) # remove any apostrophes and suffixes that follow them
upper_words <- data.frame(title = rep(book_title, length(upper_words)), word = upper_words, stringsAsFactors = FALSE) # combine title and words into dataframe
upper_words <- anti_join(upper_words, stop_words, by = "word") # remove stop words again now that everything is lower case

characters <- upper_words %>%
  count(word, sort = TRUE) %>%
  head(10) # get the top 10 most common words that start with a capital letter

# reorder factor for plotting in order
characters$word <- factor(characters$word, levels = characters$word[order(-characters$n)])

# plot
png("./maggie/plots/characters.png", width = 650, height = 480)
print(
  ggplot(characters, aes(word, n)) + 
    geom_col() +
    labs(x = "character", y="frequency", title = paste0("Most Common Characters in ", book_title))
)
dev.off()


# character relationships
character_pairs <- books_sentences %>%
  group_by(title, chapter) %>%
  summarize(harry_ron = sum(grepl("harry and ron", sentence), grepl("ron and harry", sentence)),
            harry_hermione = sum(grepl("harry and hermione", sentence), grepl("hermione and harry", sentence)),
            ron_hermione = sum(grepl("ron and hermione", sentence), grepl("hermione and ron", sentence))) # search for strings with names in both orders
character_pairs <- ungroup(character_pairs) # ungroup for mutate

character_pairs <- mutate(character_pairs, book_num = as.numeric(factor(character_pairs$title, levels = names(book_names), ordered = TRUE))) # create variable for book and chapter number
character_pairs <- mutate(character_pairs, book_ch = as.numeric(paste(book_num, ".", chapter, sep=""))) # convert book and chapter number to factor for easier format for plotting

cols <- c("hr"="red","hh"="blue","rh"="green") # set plot colors

# plot
png("./maggie/plots/char_pairs.png", width = 800, height = 480)
print(
  ggplot(character_pairs, aes(x = as.numeric(as.factor(book_ch)))) +
    geom_smooth(aes(y = harry_ron, col = "hr"), se = FALSE, show.legend =TRUE) +
    geom_smooth(aes(y = harry_hermione, col = "hh"), se = FALSE, show.legend =TRUE) +
    geom_smooth(aes(y = ron_hermione, col = "rh"), se = FALSE, show.legend =TRUE) +
    scale_color_manual("Character Pairs", labels = c("Harry & Ron", "Harry & Hermione", "Ron & Hermione"), values=cols) +
    labs(title = "Mention of Character Pairs in Harry Potter Series", x = "Chapter in series", y = "Mentions per Chapter")
)
dev.off()

