library(rtweet)
library(tidytext)
library(magrittr)
library(dplyr)
library(tm)
library(ggplot2)
library(syuzhet)
library(plotly)
library(wordcloud)

# twitter api authoirsation 
# create_token(
# app = "sona2019_analysis",
# consumer_key = "xxx",
# consumer_secret = "xxx",
# access_token = "xxx",
# access_secret = "xxx")


# search twitter for relevant tweets
# tweets <- search_tweets(
#       "#SONA2019", n = 18000, include_rts = FALSE, lang = "en", retryonratelimit = TRUE)

# get text only from tweets
# tweets.df <- tweets %>% 
#  select(text)

# write.csv(tweets.df, file = "sona_tweets.csv")

tweets.df <- read.csv("sona2019.csv", stringsAsFactors = FALSE, row.names = 1)

# write a function to clean the tweets
clean.tweets <- function(doc) {
  doc <- gsub("http.*","",  doc)
  doc <- gsub("https.*","", doc)
  doc <- gsub("#.*","",doc)
  doc <- gsub("@.*","",doc)
  doc <- gsub("[[:punct:]]", "", doc)
  doc <- gsub("rt", "", doc)
  doc <- gsub("^ ", "", doc)
  doc <- iconv(doc, "UTF-8", "ASCII", sub="")
  return(doc)
}



# apply function
tweets.df <- sapply(tweets.df, clean.tweets)

# Perform sentiment analysis using the syuzhet library
allemotions <- get_nrc_sentiment(tweets.df)








# Create variable with a spectrum of emotions 
sona_emotions <- allemotions %>%
                 select("trust", "anticipation", "fear", "joy", "anger", "sadness", "surprise", "disgust")
# Create a dataframe that summarises sentiment in descending order
emo_bar <- colSums(sona_emotions) 
emo_sum <- data.frame(count=emo_bar, sona_emotions=names(emo_bar))
emo_sum$sona_emotions <- factor(emo_sum$sona_emotions, levels=emo_sum$sona_emotions[order(emo_sum$count, decreasing = TRUE)])

# visualise the emotions from NRC sentiments

library(plotly)

emotion_plot <- plot_ly(emo_sum,
             x=~sona_emotions, 
             y=~count, 
             type="bar", 
             color=~sona_emotions) %>%
             layout (
                xaxis = list(
                     title=""), showlegend=FALSE,
             title="Emotion Type for hashtag: #SONA2019")

emotion_plot

# Create dataframe of the words in 
# word.df <- as.vector(tweets.df)
# emotion.df <- get_nrc_sentiment(word.df)
# emotion.df2 <- cbind(tweets.df, emotion.df)

# head(emotion.df)


# Create wordcloud showing key positive and negative words

# Wordcloud data

sona_posneg = c(
  paste(tweets.df[allemotions$positive > 0], collapse=" "),
  paste(tweets.df[allemotions$negative > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(sona_posneg))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) <- c("positive", "negative")
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("blue", "red"),
                 title.size=1,
                 max.words=250, 
                 scale=c(2.5, 0.4),
                 rot.per=0.4)



# Lets look at trust and fear

sona_trustfear = c(
  paste(tweets.df[allemotions$trust > 0], collapse=" "),
  paste(tweets.df[allemotions$fear > 0], collapse=" ")
)

# create corpus
corpus2 = Corpus(VectorSource(sona_trustfear))

# remove punctuation, convert every word in lower case and remove stop words

corpus2 = tm_map(corpus, tolower)
corpus2 = tm_map(corpus, removePunctuation)
corpus2 = tm_map(corpus, removeWords, c(stopwords("english")))

# create document term matrix

tdm2 = TermDocumentMatrix(corpus2)

# convert as matrix
tdm2 = as.matrix(tdm)
tdmnew2 <- tdm2[nchar(rownames(tdm2)) < 11,]

# column name binding
colnames(tdm2) <- c("trust", "fear")
colnames(tdmnew2) <- colnames(tdm2)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("purple", "black"),
                 title.size=,
                 max.words=250, 
                 scale=c(2.5, 0.4),
                 rot.per=0.4)

# Term Document Matrix 




# Text mining 






# load stopword data from tidytext
#data("stop_words")

# create list of undesireable words 
#undesirable_words <- c("south", "africa", "sona2019", "ramaphosa", "president", 
#                      "cyril", "eff", "anc", "da", "cope", "sona19", "speech", 
#                      "zuma", "malema", "2019")

# remove stop words and undesirable words
#sona_tweets <- tweets.df %>%
#  dplyr::select(text) %>%
#  unnest_tokens(word, text) %>%
#  anti_join(stop_words) %>%
#  distinct() %>%  
#  filter(!word %in% undesirable_words) %>%
#  filter(nchar(word) > 3)

