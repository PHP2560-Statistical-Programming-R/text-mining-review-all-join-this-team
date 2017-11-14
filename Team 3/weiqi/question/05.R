load("data/rawdata/HP6.Rda")

Book6Count <- series %>%
  filter(book == "order_of_the_phoenix") %>%
  group_by(chapter) 
nrow(Book6Count)

