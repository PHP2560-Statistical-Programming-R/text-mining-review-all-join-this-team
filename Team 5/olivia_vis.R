#visualize Top 10 Negative words for not all-caps (FALSE) and all-caps (TRUE):
library(ggplot2)

#This is a function to create a graph of the top ten words for the emotion "sent"
top_10_words <- function(sent,data) {
  data %>%
    filter(sentiment==sent) %>%
    count(word,caps) %>%
    group_by(caps) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(paste(word, caps, sep = "__"), n)) %>%
    # Set up the plot with aes()
    ggplot(aes(x=word,y=n,fill=caps)) +
    geom_col(show.legend = FALSE) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    facet_wrap(~ caps, nrow = 2, scales = "free") +
    coord_flip()
}

top_10_sents <- function(data) {
  data %>%
    group_by(caps) %>%
    count(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    # Set up the plot with aes()
    ggplot(aes(x=sentiment,y=n,fill=caps)) +
    geom_col(show.legend = FALSE) +
    scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
    facet_wrap(~ caps, nrow = 2, scales = "free") +
    coord_flip()
}

load("data/nyt_data_sents-nrc.rda")
levels(sents.nrc$caps) <- c("Normal Capitalization", "All Caps")

top_10_words("negative",sents.nrc)
ggsave("graph/negative_words.png")
top_10_words("positive",sents.nrc)
ggsave("graph/positive_words.png")

### Top 10 SENTIMENTS for not all-caps (FALSE) and all-caps (TRUE):
top_10_sents(sents.nrc)
ggsave("graph/sentiments.png")