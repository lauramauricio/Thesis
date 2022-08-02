library("tidytext")
#cleaning text other version
tweets_EN$clean<-gsub("@\\w+", "", text)
  
  
  # Remove mentions, urls, emojis, numbers, punctuations, etc.
  text <- 
text <- gsub("https?://.+", "", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("#\\w+", "", text)
text <- gsub("[^\x01-\x7F]", "", text)
text <- gsub("[[:punct:]]", " ", text)
# Remove spaces and newlines
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)
# Put the data to a new column
data_fix["fix_text"] <- text
head(data_fix$fix_text, 10)