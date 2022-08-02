# URL removal
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}
# Mention removal
removeMention <- function(x){
  gsub("@\\w+", "", x)
}
# Hashtag removal
removeHashtag <- function(x){
  gsub("#\\S+", "", x)
}
# Carriage removal
removeCarriage <- function(x){
  gsub("[\r\n]", "", x)
}
# Emoticon removal
removeEmoticon <- function(x){
  gsub("[^\x01-\x7F]", "", x)
}
# Retweet removal
removeRT <- function(x){
  gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
}
# Invoice removal
removeInvoice <- function(x){
  gsub("inv/[0-9]+/+[xvi]+/[xvi]+/[0-9]+", "", x, ignore.case = T)
}
# HTML removal
unescapeHTML <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# Text stemming (reduces words to their root form)
# library(“SnowballC”)
# docs <- tm_map(docs, stemDocument)
# # Remove additional stopwords
# docs <- tm_map(docs, removeWords, c(”, “”, “hrod”))
 