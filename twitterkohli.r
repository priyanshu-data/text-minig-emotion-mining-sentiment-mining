install.packages("twitteR")
library("twitteR")

install.packages("httpuv")
library("httpuv")

install.packages("ROAuth")
library("ROAuth")

install.packages("base64enc")
library("base64enc")

#sorry access key and token are left blank,as it has my personal key.
cred <- OAuthFactory$new(consumerKey='          ',
                         consumerSecret='           ',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="kohli.Rdata")
load("kohli.Rdata")

setup_twitter_oauth("", 
                    "",
                    "", # Access token
                    "")  # Access token secret key


Tweets<-userTimeline('imVkohli',n=500)

TweetsDF<-twListToDF(Tweets)
write.csv(TweetsDF,"Tweetskohli.csv")
##search with a key word

virat_tweets<-searchTwitter('worldcup',n=100,lang = "en",resultType = "recent")

TweetsDF<-twListToDF(virat_tweets)
________________________________________________________________________________________________
View(TweetsDF)

tweetsdf1<-TweetsDF[c(-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16)]
View(tweetsdf1)
__________________________________________________________________________________________________
str(tweetsdf1)
#it contains a lot of twitter handles and hashtag,needed to remove.



tweets.df2 <- gsub("http.*","",tweetsdf1$text)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)

View(tweets.df2)

_________________________________________________________________________________________________

#creating the corpus
install.packages("NLP")
library(tm)
corpus<-Corpus(VectorSource(tweets.df2))
inspect(corpus)
_______________________________________________________________________________________________
#cleaning the corpus
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,stripWhitespace)

stopwdrds = readLines(file.choose())
corpus1<-tm_map(corpus,removeWords,stopwdrds)
inspect(corpus1)

poswords<-readLines(file.choose())
str(poswords)
View(poswords)
matchcorpus1<-match(corpus1,poswords)
str(matchcorpus1)
negwords<-readLines(file.choose())
str(negwords)
match1corpus<-match(corpus1,negwords)
str(match1corpus)

#there are no positive and negative words matching.
_________________________________________________________________________________________________

#creating tdm 
tdm1<-TermDocumentMatrix(corpus1,control = list(wordlengths=c(3,Inf)))
tdm1
tdm1<-as.matrix(removeSparseTerms(tdm1, .99))
tdm1
#here we have lot's of zero terms.
tdm1=as.matrix(tdm1)
termFreq=rowSums(as.matrix(tdm1))
tdm

_________________________________________________________________________________________________
#emotion mining
###EMOTION MINING
install.packages("syuzhet")
library(syuzhet)
library(scales)
library(dplyr)
library(reshape2)
library(lubridate,ggplot2)
library(ggplot2)

txt <- iconv(TweetsDF, "UTF-8") #unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character 
example<-get_sentences(txt)
nrc_data<-get_nrc_sentiment(example)
x <- get_nrc_sentiment(txt)
head(x,n=6)
#here we have poinys for sadness and neagtive,which can infer that the tweets are going in
#negative direction.

get_nrc_sentiment('happy')
get_nrc_sentiment('worry')
##each sentences by eight######
get_sentiment('happy',method="afinn")
get_sentiment('worry',method="afinn")

_________________________________________________________________________________________________
#bar plot for emotion mining.
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count',xlab = 'emotion', main = 'Emotion scores')
sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_nrc<-get_sentiment(example,method="nrc")
sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)

#anticipation ,sadness and negative.

________________________________________________________________________________________________
windows()
plot(sentiment_vector,type='l',main = ='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,color='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)


##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="l", 
  main ="tweets transformed values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)


_______________________________________________________________________________________________

library(wordcloud)
library(RColorBrewer)
set.seed(123)

wordcloud(corpus,colors=brewer.pal(8, "Dark2"))
#the tweets have words mostly related to cup,worldcup,group,and so on ,from this we can infer 
#it is related to world cup tweets.
























