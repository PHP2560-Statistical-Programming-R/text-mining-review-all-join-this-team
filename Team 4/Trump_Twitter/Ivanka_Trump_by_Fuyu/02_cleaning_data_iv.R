bday <- str_split(ivanka_trump_twitter$created_at,pattern=" ",simplify=TRUE)

names(bday)[names(bday)=="1"] <- "date"
names(bday)[names(bday)=="2"] <- "time"

#swich the format of date to something R can read
date <- bday[,1]
date <- as.Date(date,"%m/%d/%y")

time <- bday[,2]

#separate year, month and day
pattern1 <- capture(one_or_more(DGT))  %R% "-" %R% capture(one_or_more(DGT)) %R% "-" %R% capture(DGT %R% DGT)
date1 <- str_match(date,pattern1)
colnames(date1) <- c("date","year","month","day")

# compose the useful data into a new data frame
library(dplyr)
other_col <- ivanka_trump_twitter %>% select(c(source,text,id_str,is_retweet))
data <- cbind(other_col,date1,time)

# function that cleans data
hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE) #The \K escape sequence resets the beginning of the match to the current position in the token list
    }
    return(text)
  }
  unname(sapply(text, hg))
}

cleanposts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

library(dplyr)
text_clean <- data %>% 
  select(text) %>%
  mutate(text_clean = cleanposts(text))

data_clean <- cbind(text_clean,data[,3:9],data[,1])
data_clean <- as_tibble(data_clean)
