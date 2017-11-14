spells <- 
c("accio","aguamenti","alohomora","aparecium","avada kedavra","avifors","avis","bombarda","colloportus","confringo","confundus","conjunctivitis","crucio",
"deletrius","densaugeo","diffindo","dissendium","duro","enervate","engorgio","episkey","evanesco","expecto patronum","expelliarmus", 
"fera verto","ferula","fidelius","finite incantatem","flagrate","flipendo","furnunculus","geminio","homorphus",
"immobulus","impedimenta","imperio","impervius","incarcerous","incendio","legilimens","levicorpus","liberacorpus","locomotor mortis","lumos",
"mobiliarbus","mobilicorpus","morsmordre","muffliato","nox","obliviate","orchideous","petrificus totalus","point me the four point spell","portus,prior incantato","protego", 
"quietus","reducio","reducto","relashio","rennervate","reparo","repello","repello muggletum","revelio","rictusempra","riddikulus",
"salvio hexia","scourgify","sectumsempra","serpensortia","silencio","sonorus","stupefy","tarantallegra","tergeo","waddiwasi","wingardium leviosa")

spell.df <- data.frame(spells)

ordered_books <- data.frame(title=c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", "half_blood_prince", "deathly_hallows"),
                            order=c(1,2,3,4,5,6,7))

book.spells <- inner_join(books, spell.df, by=c("word" = "spells")) %>%
    group_by(title) %>%
    dplyr::summarize(count=n()) %>%
# List books in original order    
    inner_join(ordered_books,by="title") %>%
    arrange(order)

# Need title as a factor type to arrange graph in correct order
book.spells$title <- factor(book.spells$title, levels = book.spells$title)

ggplot(book.spells,aes(x=title,y=count,fill=title)) +
    geom_col()+
    ggtitle("Total Number of Spells Used for Each Book") +
    # Rotates x axis labels vertically
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    # Removes legend
    theme(legend.position="none")














POA <- filter(books,title=="prisoner_of_azkaban")
    