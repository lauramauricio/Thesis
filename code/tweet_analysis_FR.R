#data cleaning French tweets
transformed_text <- iconv(tweets_FR$text)
transformed_text <- Corpus(VectorSource(tweets_FR$text))

# Convert the text to lower case
transformed_text <- tm_map(transformed_text, content_transformer(tolower))
# Remove punctuations
transformed_text <- tm_map(transformed_text, removePunctuation)
# Remove numbers
transformed_text <- tm_map(transformed_text, removeNumbers)

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
  gsub("'", "",x)
  gsub("•\\s*", "",x)
  gsub("•\\s*", "", x)
  gsub("’", "", x)
  }
transformed_text <- tm_map(transformed_text,Textprocessing)
transformed_text <- tm_map(transformed_text, content_transformer(removeURL))
#remove whitespaces left
transformed_text<-tm_map(transformed_text, stripWhitespace)
# Remove french common stopwords
ownstopwords<-c(stopwords("french"), "cest","tout","plus", "être", "dune", "aussi","ça", "mars","encore")
transformed_text <- tm_map(transformed_text, removeWords, ownstopwords)

# Stemming
transformed_text <- tm_map(transformed_text, stemDocument)
# Eliminate extra white spaces
transformed_text <- tm_map(transformed_text, stripWhitespace)

#french analysis
cleansetfr<-TermDocumentMatrix(transformed_text)
cleansetfr<- as.matrix(cleansetfr)

plotfr  <-rowSums(cleansetfr)
plotfr   <- subset(plotfr , plotfr >=100)
barplot(plotfr)
#inspect frequent words
freqterms <- findFreqTerms(cleansetfr)
termFreq <- rowSums(as.matrix(cleansetfr))
termFreq <- subset(termFreq, termFreq >=100)
df <- data.frame(term = names(termFreq), freq = termFreq)

freqword<- ggplot(df,aes(x = reorder(df$term, +df$freq), y = freq, fill=df$freq)) + 
  geom_bar(stat = "identity", fill="#008000")+
  xlab("Terms") + 
  ylab("Count") + 
  coord_flip()+theme_bw()
freqword

#word cloud
library(wordcloud)
cloud<-sort(rowSums(cleansetfr), decreasing = TRUE)
c<-wordcloud(words = names(plotfr ),
             freq = plotfr  ,
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

# sentimentfr<-get_nrc_sentiment(as.character(transformed_text))
# tweets_FR$sentiment <-tweets_FR%>%
#   mutate()
# barplot(colSums(sentimentfr),
#         las = 2,
#         col = rainbow(10),
#         ylab = 'Count',
#         main = 'Sentiment Scores Tweets')

# Add transformed text as column
tweets_FR$t_text <- transformed_text$content

# Add sentiment score
init_sentiment.ai(model="multi.large")
tweets_FR$sentiment <- sentiment_score(tweets_FR$t_text,  model="multi.large")

#plot sentiment over time
sentfr<- tweets_FR%>%
  group_by(date)%>%
  dplyr::summarise(n=n(), sentavg= mean(sentiment))
#prepare date format for graph
sentfr$date<-as.Date(sentfr$date)
#plot sentiment over time
coeff<-100
p<- ggplot(sentfr, aes(x=date)) + 
  geom_line(aes(y=sentavg), group=1, colour="#008000")+
  geom_point(aes(y=n/coeff))+
  scale_y_continuous(
    name = "Sentiment Score",
    sec.axis = sec_axis(~.*coeff, name="Tweets"))+
  scale_x_date(date_labels = "%b/%y",breaks = "1 month")+
  theme(axis.text.x = element_text(angle=45, hjust = 1, size=12))
p
#boxplot sentiment
b<-ggplot(sentfr, aes(y=sentavg))+
  geom_boxplot()
b
