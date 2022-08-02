engtweets<-tweets_EN_CH$transformed_text
#Tweets into matrix for wordcloud and sentiment
cleanset<-TermDocumentMatrix(tweets_EN_CH$transformed_text)
cleanset<- as.matrix(cleanset)

plot<-rowSums(cleanset)
plot <- subset(plot, plot>=5)
barplot(plot)
#inspect frequent words
freq.terms <- findFreqTerms(cleanset, lowfreq =  0)

termFreq <- rowSums(as.matrix(cleanset))
termFreq <- subset(termFreq, termFreq >=10)
df <- data.frame(term = names(termFreq), freq = termFreq)

freqword<- ggplot(df,aes(x = reorder(df$term, +df$freq), y = freq, fill=df$freq)) + geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + xlab("Terms") + ylab("Count") + coord_flip()
freqword
#word counts with unnest_token and tidyverse


#word cloud
library(wordcloud)
cloud<-sort(rowSums(cleanset), decreasing = TRUE)
c<-wordcloud(words = names(plot),
          freq = plot,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

engtweets<- (tweets_EN_CH$transformed_text)
unnest(word)
#sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#tokenning the text
token<- unlist(engtweets)


sentiment<-get_nrc_sentiment(as.character(tweets_EN_CH$transformed_text))
barplot(colSums(sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')
