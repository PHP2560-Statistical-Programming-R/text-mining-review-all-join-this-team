
nrc.binary <- filter(nrc.sents, sentiment %in% c("positive", "negative")) #just looking at positive/negative
compare <- inner_join(bing.sents, nrc.binary, 
                      by=c("word", "Article_ID"), suffix = c(".bing", ".nrc")) #words that match both
agreement = mean(compare$sentiment.bing==compare$sentiment.nrc)


  
#logistic model for topic
  
  #get most frequent words
  most.freq <- words$word %>%
    table()%>%
    as_tibble() %>%
    arrange(desc(n)) %>%
    slice(1:30)
  model.words <- most.freq$.
  
  #create feature that counts word appearances
  model.data = NYTimes
  for(i in 1:length(model.words)){
    feature.word = model.words[i]
    for(j in 1:dim(NYTimes)[1]){
      model.data[[feature.word]][j]=str_count(NYTimes$Title[j], 
                                              (START%|%SPC) %R% feature.word %R% (END%|%SPC))
    }
  }  

  #train classifier  
  model <- multinom(as.factor(Topic.Code) ~ . - Article_ID - Date - Title - Subject, data = model.data)
  
  #training set accuracy
  accuracy=mean(predict(model, newdata = model.data)==model.data$Topic.Code)