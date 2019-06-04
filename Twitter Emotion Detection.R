library(tidytext)
library(twitteR)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(tokenizers)
library(stopwords)

# Authonitical keys
consumer_key <- '7wnAdbsRTKH3u2MtjyAsxPoEU'
consumer_secret <- 'rsDQbMrcc9Ai2vHCbq5Vu3hjEc26xdCYw2PzbJrmPQUoAFPhKX'
access_token <- '1089990631584342016-6UaGdd3Tae2eQsZvkLY7Oii3PtnPCJ'
access_secret <- 'jxYdYKXkPPhAS2gpQIJU7ZYWAJC6hWrtaRpOHS7QozBhV'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Search tweets
tweets <- searchTwitter("#Fear", n = 1000, lang = "en")
tweets_fear <- twListToDF(tweets)
head(tweets_fear)
write.csv(tweets_fear, file = 'Fear.csv',row.names = F)

# Clean tweets
Fear_text <- tweets_fear$text
Fear_text <- tolower(Fear_text) # convert all text to lower case
Fear_text <- gsub("rt", "", Fear_text) # Replace blank space ("rt")
Fear_text <- gsub("@\\w+", "", Fear_text) # Replace @Username
Fear_text <- gsub("[[:punct:]]", "", Fear_text) # remove puntuations
Fear_text <- gsub("http\\w+", "", Fear_text) # Remove links
Fear_text <- gsub("https\\w+", "", Fear_text) 
Fear_text <- gsub("[ |\t]{2,}", "", Fear_text) # Remove tabs
Fear_text <- gsub("^ ", "", Fear_text) # Remove blank space at the beginning
Fear_text <- gsub(" $", "", Fear_text) # Remove blank space at the end
#Fear_text <- gsub("#.*", "", Fear_text) # Replace hashtag
head(Fear_text[906])

# Tokenization
Sent <- tokenize_sentences(Fear_text)
head(Sent)
Sent_words <- tokenize_word_stems(Sent[1], stopwords = stopwords::stopwords("en"))
head(Sent_words)

# Emotion
Sent_words_char <- as.character(Sent_words)
emotion <- get_nrc_sentiment(Sent_words_char)
emotion_text <- cbind(Fear_text[1], emotion)
head(emotion_text)
 