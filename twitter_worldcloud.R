

#install.packages("twitteR", dependencies=TRUE)
#install.packages("RCurl")
#install.packages('bitops')
#install.packages('base64enc')
#install.packages('httpuv')
#install.packages('tm')
#install.packages('wordcloud')
#install.packages("stringr")
library(twitteR)
library(RCurl)
library(bitops)
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(stringr)

options(stringsAsFactors = FALSE)     #Strings will not be treated as Factors.

create_file="twitterOauth.txt"
oauthCreds = read.table(create_file,header=T)
setup_twitter_oauth("3xCZZDb4QFuqYLed4GfNOSnA4",
                    "89NVcqaw3zOD6tJiisz9s4zvGOBZ5sI7a0jOIs0C1OhC5eeaQl",
                    "1183003655915462662-EK5jjiQXUPYvaW6BZ9y0d869UXC8w7",
                    "gEpO9CZ8NfgXlIBUm8AFBQQPWzrc2OLHvX8QYuELSwieX") 

searchTerms= c("India + vs + Sa" )
numberOfTweets=300
tweets_list = searchTwitter(searchTerms,lang="en",n=numberOfTweets,resultType="recent")
class(tweets_list)
tweets_list[[1]]      #first tweet in a list.


tweets_text = sapply(tweets_list, function(x) x$getText())    #Conversion to TEXT
class(tweets_text)
tweets_text[1]

tweets_corpus = Corpus(VectorSource(tweets_text))
class(tweets_corpus)
inspect(tweets_corpus[1:3])

#ORDER IS VERY IMPORATNT
tweets_corpus_clean = tm_map(tweets_corpus, removePunctuation)
tweets_corpus_clean = tm_map(tweets_corpus_clean, stripWhitespace)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeNumbers)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, stopwords("english"))
tweets_corpus_clean = tm_map(tweets_corpus_clean, content_transformer(tolower))
toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x))
tweets_corpus_clean = tm_map(tweets_corpus_clean, toSpace,"https*|youtu*")
#tweets_corpus_clean = tm_map(tweets_corpus_clean, stemDocument)




tweets_tdm = TermDocumentMatrix(tweets_corpus_clean)
str(tweets_tdm)
class(tweets_tdm)

tweets_tdm = as.matrix(tweets_tdm)
str(tweets_tdm)
class(tweets_tdm)

tdm_term_freq_sort = sort(rowSums(tweets_tdm), decreasing=TRUE)
tdm_term_freq_sort_inc = sort(rowSums(tweets_tdm), decreasing=FALSE)
tdm_term_freq_df = data.frame(word = names(tdm_term_freq_sort),freq = tdm_term_freq_sort)
str(tdm_term_freq_df)
head(tdm_term_freq_df,10)




wordcloud(words = tdm_term_freq_df$word,
          freq= tdm_term_freq_df$freq,
          min.freq=10,
          max.words=300,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8,'Dark2'),
          scale=c(3,0.5))

