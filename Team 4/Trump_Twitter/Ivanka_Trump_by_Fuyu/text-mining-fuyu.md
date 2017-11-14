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
-   Harry Potter Complete 7 Books text
-   [State of the Union speeches](https://pradeepadhokshaja.wordpress.com/2017/03/31/scraping-the-web-for-presdential-inaugural-addresses-using-rvest/)
-   Scrape tweets using [`twitteR`](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/)

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
library(stringr)
library(utils)
library(rebus)
```

    ## 
    ## Attaching package: 'rebus'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     regex

``` r
library("tidyverse")
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'tidyr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## alpha():  ggplot2, rebus
    ## filter(): dplyr, stats
    ## lag():    dplyr, stats
    ## regex():  stringr, rebus

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
#split date and time
ivanka_trump_twitter <- read.csv("Data/ivanka_trump_twitter.csv")
bday <- str_split(ivanka_trump_twitter$created_at,pattern=" ",simplify=TRUE)

names(bday)[names(bday)=="1"] <- "date"
names(bday)[names(bday)=="2"] <- "time"

#swich the format of date to someting R can read
date <- bday[,1]
date <- as.Date(date,"%m/%d/%y")

time <- bday[,2]

#separate year, month and day
pattern1 <- capture(one_or_more(DGT))  %R% "-" %R% capture(one_or_more(DGT)) %R% "-" %R% capture(DGT %R% DGT)
date1 <- str_match(date,pattern1)
colnames(date1) <- c("date","year","month","day")
```

``` r
# compose the useful data into a new data frame
library(dplyr)
other_col <- ivanka_trump_twitter %>% select(c(source,text,id_str,is_retweet))
data <- cbind(other_col,date1,time)
```

``` r
# the number of twitter each year
n_tweet <- data %>%
  group_by(year,month) %>%
  mutate(timestamp=ymd(date))
```

    ## Warning: All formats failed to parse. No formats found.

``` r
ggplot(n_tweet,aes(x = timestamp,fill=year)) +
  geom_histogram(position = "identity",bins =20,show.legend=FALSE) +
  xlab("")
```

    ## Warning: Removed 7 rows containing non-finite values (stat_bin).

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
# function that cleans data
hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE)
    }
    return(text)
  }
  unname(sapply(text, hg))
}

cleanposts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}
```

``` r
library(dplyr)
text_clean <- data %>% 
  select(text) %>%
  mutate(text_clean = cleanposts(text))
#remove stop words like "the","of","to"

library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 3.4.2

``` r
tidy_text <- text_clean %>%
  mutate(linenumber=row_number())%>%
  unnest_tokens(word,text_clean) %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
data_clean <- cbind(text_clean,data[,3:9],data[,1])
data_clean <- as_tibble(data_clean)
data(stop_words)
data_tidy <- data_clean %>%
  select(-text) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word,text_clean)%>%
  anti_join(stop_words)
```

    ## Joining, by = "word"

``` r
#the most used words
library(ggplot2)
tidy_text %>%
  count(word,sort=TRUE) %>%
  filter(n>500)%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
```

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

``` r
names(data_clean)
```

    ##  [1] "text"       "text_clean" "id_str"     "is_retweet" "date"      
    ##  [6] "year"       "month"      "day"        "time"       "data[, 1]"

``` r
#words frequency by month

dir.create("graph/",showWarnings = FALSE)
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
library(dplyr)


par(mfrow = c(3,3))
for(i in seq(2009,2017)){
frequency <- data_tidy %>%
  group_by(year,word) %>%
  filter(year == i) %>%
  count(year,word,sort = T) %>%
  with(wordcloud(word,n,max.words =90,random.order = F, random.color = F, colors=brewer.pal(8, "Dark2")))
}
```

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : reading could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : collection could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : international could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : special could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : barnes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : beautiful could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : favorite could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : preorder could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : start could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sunday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : travel could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : view could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : airport could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : apprentice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : business could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : central could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congrats could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dress could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : excited could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : holidays could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : join could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : learn could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : meeting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : nytimes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : observer could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : people could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : real could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shopping could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : street could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : support could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : terrific could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wait could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watching could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : apprentice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebrity could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : facebook could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : week could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : collection could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : page could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : excited could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : friends could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heading could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ready could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : episode could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : gorgeous could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sunday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congrats could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : season could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : boardroom could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : husband could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : office could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : headed could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : home could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : favorite could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hope could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonights could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enjoy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jewels could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : launch could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watching could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tomorrow could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : weekend could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : agree could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : bracelet could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : vote could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : pm could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : copy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : finished could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : recipe could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : world could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : arrived could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celeb could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : diamond could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enjoyed could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : event could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : follow could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hilarious could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jewelers could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tower could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : twitter could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wearing could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : apprentice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : trump could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : happy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : amazing could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : excited could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jewelry could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : arabella could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dinner could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : home could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jared could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : gorgeous could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heading could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : week could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ready could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : handbag could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watch could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congrats could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : footwear could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : beach could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : perfect could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : visit could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : baby could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : book could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celeb could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : line could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : birthday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : headed could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : office could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : palm could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : beautiful could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enjoy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enter could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : movie could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shoes could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watching could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wedding could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : flight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : forward could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hope could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : incredible could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : launch could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : airport could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : backstage could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congratulations could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heres could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : page could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sunday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sweeps could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : team could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tomorrow could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tune could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : valentines could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : view could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : weekend could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : exciting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : father could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : finale could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : finished could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : husband could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : kuwait could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : park could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shoe could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : vegas could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : snow could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summer could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : apprentice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : arabella could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : collection could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : excited could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heading could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebapprentice could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : fashion could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : home could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tower could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebrity could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : office could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ready could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : beautiful could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : chicago could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : post could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : youre could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congratulations could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : gorgeous could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : paris could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : visit could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watching could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : birthday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jared could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : national could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : app could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congrats could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : finale could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : glad could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : launch could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : chance could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dont could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enter could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : episode could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : father could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : incredible could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jewelry could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : romney could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sandy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : spring could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sweet could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : backstage could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : boardroom could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : collections could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dress could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : favorite could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : maralago could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : mexico could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : nice could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shoes could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shopping could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : soho could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : spon could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tomorrow could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : view could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watch could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : apprentice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : incredible could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congrats could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : launch could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : quote could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : world could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dinner could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hotel could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : team could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : agree could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebapprentice could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : pinterest could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shoes could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tower could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : days could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : fascinating could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : home could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : maralago could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : quotes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : enjoy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : girl could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : kids could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : vegas could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : brooklyn could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : club could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : girls could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : gorgeous could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jared could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : office could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : read could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ready could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : season could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : view could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : visit could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : cover could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : finale could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jewelry could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : lchat could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : macys could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : mothers could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : party could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : people could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : proud could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sweet could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : video could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : beautiful could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : arabella could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : people could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : congratulations could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : holiday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : regram could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watch could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : fashion could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : photo could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : tonight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : home could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summer could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : words could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : quotes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shoes could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : sunday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wise could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : perfect could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dress could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : mom could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : spring could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : building could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heels could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : incredible could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : live could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : party could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : pumps could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : season could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : start could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : trumpdoral could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebrating could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ideas could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : park could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : baby could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : boots could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : chat could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : chic could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : club could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : pink could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : top could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : weve could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : womenwhowork could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hotmama could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : amazing could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : design could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : parenting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : interview could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : nutrition could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summerfriday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : interntips could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : questions could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : fallstyle could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : entrepreneurinresidence could not be fit on page. It
    ## will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : fashion could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : recipes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : secrets could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : creative could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : youre could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ideas could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : collection could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : intern could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : week could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : interns could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebrity could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : challenge could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : happy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : personal could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : polished could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wanderlusty could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : careeradvice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : womenwhowork could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : inspiration could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : twisewords could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wisewords could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : quote could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summer could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : advice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : shares could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : ivankas could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : founder could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : favorite could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : check could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : recipes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : workwear could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summerstyle could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : weekend could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : parenting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : amazing could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : businessadvice could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : pieces could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : quotes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : worktips could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : entrepreneurinresidence could not be fit on page. It
    ## will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : straight could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hacks could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : content could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : learn could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : productivity could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : psychologist could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : delivered could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : inboxsign could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : wardrobe could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : healthy could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : words could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : mothers could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : winter could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : career could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jessica could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : negotiate could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : nutrition could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : share could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dinner could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : dresses could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : month could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : play could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : springstyle could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : trump could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : baby could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : womens could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : workforce could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : support could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : business could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : forward could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : discuss could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : honored could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : million could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : amazing could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : entrepreneurs could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : global could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : trafficking could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : child could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : computer could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : leaders could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : commitment could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : discussion could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : incredible could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : people could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : time could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : book could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : meeting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : military could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : proud could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : science could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : united could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : education could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : human could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : leave could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : stem could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : visit could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : access could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : americans could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : birthday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : development could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : empower could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : entrepreneurship could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : future could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : jobs could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : president could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : america could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : economic could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : heroes could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : key could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : kids could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : continue could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : economy could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : expanding could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : hurricane could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : im could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : importance could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : love could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : minister could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : prayers could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : summit could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : administration could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : celebrate could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : check could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : eclipse could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : leadership could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : national could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : nations could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : paid could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : spouses could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : supporting could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(word, n, max.words = 90, random.order = F,
    ## random.color = F, : watch could not be fit on page. It will not be plotted.

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
#sentiment analysis
library(tidytext)
library(tidyr)
text_s <- data_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(year,index = linenumber %/% 100, sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(sentiment = positive - negative)
```

    ## Joining, by = "word"

``` r
library(ggplot2)
ggplot(text_s, aes(index,sentiment,fill=year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year,ncol=3,scales = "free_x")
```

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
# Most common positive and negative words
bing_word_counts <- data_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()
```

    ## Joining, by = "word"

``` r
bing_word_counts
```

    ## # A tibble: 1,070 x 3
    ##           word sentiment     n
    ##          <chr>     <chr> <int>
    ##  1       trump  positive   733
    ##  2        love  positive   405
    ##  3     amazing  positive   317
    ##  4    favorite  positive   278
    ##  5       skill  positive   263
    ##  6        hack  negative   257
    ##  7       happy  positive   216
    ##  8 inspiration  positive   192
    ##  9        fall  negative   190
    ## 10     excited  positive   182
    ## # ... with 1,060 more rows

``` r
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10)%>%
  ungroup %>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
```

    ## Selecting by n

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

``` r
# Changes in word use
library(lubridate)

word_by_time <- data_tidy %>%
  mutate(timestamp=ymd(date))%>%
  mutate(time_floor = floor_date(timestamp,unit = "1 month")) %>%
  count(time_floor,word) %>%
  ungroup() %>%
  group_by(time_floor)%>%
  mutate(time_total = sum(n)) %>%
  group_by(word)%>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 400)

as_tibble(word_by_time)
```

    ## # A tibble: 610 x 5
    ##    time_floor   word count time_total word_total
    ##        <date>  <chr> <int>      <int>      <int>
    ##  1 2009-07-01  check     1        256        433
    ##  2 2009-07-01    day     2        256        540
    ##  3 2009-07-01 ivanka     1        256        786
    ##  4 2009-07-01  trump     7        256        733
    ##  5 2009-08-01    day     2        275        540
    ##  6 2009-08-01   love     3        275        405
    ##  7 2009-08-01  trump     3        275        733
    ##  8 2009-09-01  check     3        439        433
    ##  9 2009-09-01 ivanka     1        439        786
    ## 10 2009-09-01   love     2        439        405
    ## # ... with 600 more rows

``` r
nested_data <- word_by_time %>%
  nest(-word)

library(purrr)
nested_models <- nested_data %>%
  mutate(models = map(data,~glm(cbind(count,time_total) ~ time_floor, ., family = "binomial")))

library(broom)
slopes <- nested_models %>%
  unnest(map(models,tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>%
  filter(adjusted.p.value < 0.1)

word_by_time %>% 
  inner_join(top_slopes,by="word") %>%
  ggplot(aes(time_floor,count/time_total,color=word),show.legend = FALSE) +
  geom_line(size = 0.8) +
  labs(x = NULL, y = "Word Frequency")
```

    ## Warning: Removed 3 rows containing missing values (geom_path).

![](text-mining-fuyu_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)
