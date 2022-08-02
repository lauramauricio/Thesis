library(tidyverse)
library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(SnowballC)
library(sentiment.ai)
library(ggplot2)

source("tweets_functions.R")

twitter<- read.csv("D:\\Switchdrive\\Private\\Cours IHEID\\Thèse\\Coding\\Tweets\\data\\data\\tweets_all.csv", encoding = "UTF-8", na.strings=c(""," "))
#Removing countries with country codes other than Switzerland
twitter<-subset(twitter, is.na(geo.country)| geo.country=="Schweiz")
#changing date format
twitter$created_at<-strptime(twitter$created_at, format="%Y-%m-%dT%H:%M:%S")
twitter$date<-strftime(twitter$created_at, format="%Y-%m-%d")
#subset twitter data by languages
tweets_FR<- subset(twitter, lang == "fr")
tweets_EN_CH <- subset(twitter, lang == "en")
#subset by author location
location<-c("Suisse","Schweiz","Svizzera","Switzerland", "CH", "Zurich", "Geneva","Swiss", "Lausanne", "Bern")
tweets_EN_CH$author.location<-tolower(tweets_EN_CH$author.location)
tweets_EN_CH <- tweets_EN_CH %>%
  filter(geo.country== "Schweiz"|str_detect(author.location, "suisse|schweiz|svizzera|switzerland|zurich|zürich|genev|genève|swiss|lausanne|bern|neuchâtel|aarau|basel|fribourg|luzern|winterthur|st.gallen|lugano|biel|thun"))

tweets_DE <-subset(twitter, lang == "de")

tweetFR_CSV<- tweets_FR%>%
  select(text, created_at, author.location)%>%
  distinct(text, .keep_all = TRUE)
write.csv(tweetFR_CSV, file = "Tweets_FR.csv")
write_xlsx(tweetFR_CSV, "Tweets_FR.xlsx")

#building corpus
transformed_text <- iconv(tweets_EN_CH$text)
transformed_text <- Corpus(VectorSource(tweets_EN_CH$text))

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
}
transformed_text <- tm_map(transformed_text,Textprocessing)
transformed_text <- tm_map(transformed_text, content_transformer(removeURL))
#remove whitespaces left
transformed_text<-tm_map(transformed_text, stripWhitespace)
# Remove english common stopwords
ownstopwords<-c(stopwords("english"), "manan", "momentsnkyysonvootwithpaniasmanannviacomstudio","bbcstudiosdgtl","directorshot","nititaylor")
transformed_text <- tm_map(transformed_text, removeWords, ownstopwords)

# Stemming
transformed_text <- tm_map(transformed_text, stemDocument)
# Eliminate extra white spaces
transformed_text <- tm_map(transformed_text, stripWhitespace)

# Add transformed text as column
tweets_EN_CH$t_text <- transformed_text$content


# Add sentiment score
init_sentiment.ai(model="en.large")
tweets_EN_CH$sentiment <- sentiment_score(tweets_EN_CH$t_text,  model="en.large")
