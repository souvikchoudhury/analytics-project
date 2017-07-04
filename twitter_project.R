library (httr)
myapp <- oauth_app("twitter", 
                   key = "uKSJYzlhhhvzQQZRuCNY1xTeG", 
                   secret ="ETxMFGH9ky9TRePA1UgZmHlnRPGP4u8cTv3smRvkX26VZCTJP3")
sig <-  sign_oauth1.0(myapp,
                      token = "735515871167254528-R0SHckKWWf7FyZMWwqhioxo6T8RFuQX",
                      token_secret = "qRuDWJHOmbuxa4xl0xgoYz2choHv9lhHg5p5F6H2WSYsn")

homeTL <- GET ("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
json1 <- content(homeTL)

library(jsonlite)
json2 <- fromJSON(toJSON(json1))
head(json2)
a <-json2[1:5,"text"]
a
json2[1:3,1:5]

FollowerList <- GET ("https://api.twitter.com/1.1/followers/list.json",sig)
json3 <- content(FollowerList)
json4 <- fromJSON(toJSON(json3))
df <- as.data.frame(json4) 
df [1:5,1:5]

library(twitteR)

consumerKey <- "uKSJYzlhhhvzQQZRuCNY1xTeG"
consumerSecret <- "ETxMFGH9ky9TRePA1UgZmHlnRPGP4u8cTv3smRvkX26VZCTJP3"
accessToken <- "735515871167254528-R0SHckKWWf7FyZMWwqhioxo6T8RFuQX"
accessTokenSecret <- "qRuDWJHOmbuxa4xl0xgoYz2choHv9lhHg5p5F6H2WSYsn"

setup_twitter_oauth (consumerKey, consumerSecret, accessToken, accessTokenSecret)



# Use the searchTwitter function to only get tweets within 50 miles of Los Angeles
tweets_geolocated <- searchTwitter("#PokemonGo", n=100, lang="en", 
                                   geocode="20.3588,85.8333°,124.274mi",
                                   since="2016-11-08")
tweets_geolocated.df <- twListToDF(tweets_geolocated)
names(tweets_geolocated.df)
tweets_geolocated.df[1:5,c(1,5,8,10,11)]

#######
tweets <- searchTwitter("#PokemonGo", n=100, lang = "en") # top 300 tweets that contain search term
tweet_txt1<-twListToDF(tweets)
tweet_txt2<- tweet_txt1[,"text"]

#tweet_txt = lapply(tweets, function(x) x$getText())

head(tweet_txt2)

clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  some_txt <- strsplit(some_txt," ")
  return(some_txt)
}

tweet_clean <- clean.text(tweet_txt2)

head(tweet_clean,100)

positive <- scan(file.choose(),what='character',comment.char=';')
negative <- scan(file.choose(),what='character',comment.char=';')

positive[20:30]

negative[500:510]

#Additional words can be added or removed from the dictionaries.
positive <- c(positive,"cloud")
negative <- negative[negative!="cloud"]

#for counting the positive matching words.
returnpscore <- function(t) {
  pos.match <- match(t,positive)
  pos.match <- !is.na(pos.match)
  pos.score <- sum(pos.match)
  return(pos.score)
}
positive.score <- lapply(tweet_clean,function(x) returnpscore(x))

head(positive.score)

pcount=0
for (i in 1:length(positive.score)) { 
  pcount<-  pcount + positive.score[[i]]
}
pcount

#for counting the negative matching words.
returnnscore=function(twet) {
  neg.match=match(twet,negative)
  neg.match=!is.na(neg.match)
  neg.score=sum(neg.match)
  return(neg.score)
}
negative.score=lapply(tweet_clean,function(x) returnnscore(x))

ncount=0
for (i in 1:length(negative.score)) {
  ncount=ncount+negative.score[[i]]
}
ncount

poswords <- function(t){
  pmatch <- match(t,positive)
  posw <- positive[pmatch]
  posw <- posw[!is.na(posw)]
  return(posw)
}

negwords=function(t){
  nmatch=match(t,negative)
  negw=negative[nmatch]
  negw=negw[!is.na(negw)]
  return(negw)
}

words <- NULL
pdatamart <- data.frame(words)

for (t in tweet_clean) {
  pdatamart=c(pdatamart,poswords(t))
}
head(pdatamart,10)
#length(pdatamart)

words <- NULL
ndatamart <- data.frame(words)

for (t in tweet_clean) {
  ndatamart <-c(negwords(t),ndatamart)
}
head(ndatamart,10)

pwords <- unlist(pdatamart)
nwords <- unlist(ndatamart)

dpwords <- data.frame(table(pwords))
dnwords <- data.frame(table(nwords))

library(dplyr)
## positive
dpwords <- dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>3)

## negative 
dnwords <- dnwords%>%
  mutate(nwords=as.character(nwords))%>%
  filter(Freq>3)


library(ggplot2)
ggplot(dpwords,aes(pwords,Freq))+
  geom_bar(stat="identity",fill="lightgreen")+
  theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurence",
       title=paste("Major Positive Words and Occurence in \n '","Demonetization","' twitter feeds, n =",300))+
  geom_text(aes(1,5,label=paste("Total Positive Words :",pcount)),size=4,hjust=0)+
  theme(axis.text.x=element_text(angle=45))

## negative plot

ggplot(dnwords,aes(nwords,Freq))+
  geom_bar(stat="identity",fill="lightblue")+
  theme_bw()+
  geom_text(aes(nwords,Freq,label=Freq),size=4)+
  labs(x="Major Negative Words", y="Frequency of Occurence",
       title=paste("Major Negative Words and Occurence in \n '","PokemonGo","' twitter feeds, n =",300))+
  geom_text(aes(1,5,label=paste("Total Negative Words :",ncount)),size=4,hjust=0)+
  theme(axis.text.x=element_text(angle=45))


library(tm)

tweetscorpus <- Corpus(VectorSource(tweet_clean))
inspect(tweetscorpus[1:4])
tweetscorpus <- tm_map(tweetscorpus,removeWords,stopwords("english"))

library(wordcloud)                    
wordcloud(tweetscorpus,
          random.order = F,rot.per = 0.20,
          colors = brewer.pal(8,"Accent"),
          max.words = 100)

dtm <- DocumentTermMatrix(tweetscorpus)

# #removing sparse terms
dtms<- removeSparseTerms(dtm,.99)

freq<- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
#get some more frequent terms
findFreqTerms(dtm,lowfreq=20)

wf <- data.frame(word=names(freq),freq=freq)
wfh=wf%>%
  filter(freq>=20,!word==tolower("Demonetization"))

ggplot(wfh,aes(word,freq))+
  geom_bar(stat="identity",fill='lightgreen')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(aes(word,freq,label=freq),size=2)+
  labs(x="High Frequency Words ",y="Number of Occurences", 
       title=paste("High Frequency Words and Occurence in \n '","PokemonGo","' twitter feeds, n =",300))+
  geom_text(aes(1,max(freq)-20,
                label=paste(" Positive Words:",pcount,"\n","Negative Words:",ncount,"\n",
                            "The Sentiment for PokemonGo is positive in the ratio of",
                            round(pcount/ncount,2),":", 1)),size=5, hjust=0)

