#Tweets German
tweets_DE <-subset(twitter, lang == "de")
tweets_DE$transformed_text <- iconv(tweets_DE$text)
tweets_DE$transformed_text <- Corpus(VectorSource(tweets_DE$text))

# Convert the text to lower case
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, content_transformer(tolower))
# Remove punctuations
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, removePunctuation)
# Remove numbers
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, removeNumbers)

#remove URL, hashtags, mentions, URLs
Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub("https?://.+", "", x)
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub("#\\w+", "", x)
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
  gsub('')
}
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text,Textprocessing)
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, content_transformer(removeURL))
#remove whitespaces left
tweets_DE$transformed_text<-tm_map(tweets_DE$transformed_text, stripWhitespace)
# Remove english common stopwords
ownstopwords<-c(stopwords("german"))
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, removeWords, ownstopwords)

# Stemming
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, stemDocument)
# Eliminate extra white spaces
tweets_DE$transformed_text <- tm_map(tweets_DE$transformed_text, stripWhitespace)

#german analysis
cleansetde<-TermDocumentMatrix(tweets_DE$transformed_text)
cleansetde<- as.matrix(cleansetde)

plotde  <-rowSums(cleansetde)
plotde   <- subset(plotde , plotde >=200)
barplot(plotde)
#inspect frequent words
freqterms <- findFreqTerms(cleansetde, lowfreq =  0)
termFreq <- rowSums(as.matrix(cleansetde))
termFreq <- subset(termFreq, termFreq >=200)
df <- data.frame(term = names(termFreq), freq = termFreq)

freqword<- ggplot(df,aes(x = reorder(df$term, +df$freq), y = freq, fill=df$freq)) + geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + xlab("Terms") + ylab("Count") + coord_flip()
freqword
#word counts with unnest_token and tidyverse


#word cloud
library(wordcloud)
cloud<-sort(rowSums(cleansetde), decreasing = TRUE)
c<-wordcloud(words = names(plotde ),
             freq = plotde  ,
             max.words = 500,
             random.order = F,
             min.freq = 5,
             colors = brewer.pal(8, 'Dark2'),
             scale = c(5, 0.3),
             rot.per = 0.7)
#sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

sentimentde<-get_nrc_sentiment(as.character(tweets_DE$transformed_text))
barplot(colSums(sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')