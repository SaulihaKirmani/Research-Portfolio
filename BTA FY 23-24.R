#Installing relevant packages

install.packages("tm")
install.packages("NLP")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("httr")
install.packages("sentimentr")
install.packages("syuzhet")
install.packages("RCurl")


#File Upload

cname=file.path("C:","Budget Speech")
cname
docs=VCorpus(DirSource(cname))
docs


#Data Cleaning

inspect(docs)
docs=tm_map(docs,removePunctuation)
docs
docs=tm_map(docs,removeNumbers)
docs
docs=tm_map(docs,tolower)
docs
docs=tm_map(docs,removeWords,stopwords("c("and" 
                                        "the", "have", "was", "with", "for", "from", "which",
                                        "the", "that", "than"))")
docs


#Creating Wordcloud

filenames=list.files(getwd(),pattern=".txt")
filenames
getwd()
setwd("C:/Budget Speech")
files=lapply(filenames,readLines)
files
install.packages("corpus")
library(corpus)
articles.corpus= Corpus(VectorSource(files))
articles.corpus
articles.corpus=tm_map(articles.corpus, removeWords, c("and",
                                                     "the", "have", "was", "with"))
articles.corpus=tm_map(articles.corpus, stemDocument)
set.seed(1234)
wordcloud(articles.corpus)


#Creating a Term Document Matrix TDM

tdm=TermDocumentMatrix(articles.corpus,control = list(wordLength=3))
tdm
tdm=as.matrix(tdm)
tdm
termFreq=rowSums(as.matrix(tdm))
termFreq

#Plotting TDM

termFreqsubset=subset(termFreq,termFreq>=5)
termFreqsubset
# Creating a data frame
install.packages("ggplot2")
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
termFreq=rowSums(as.matrix(tdm))
termFreq
termFreqsubset=subset(termFreq,termFreq>=18)
termFreqsubset
tdmdf=data.frame(term=names(termFreqsubset),freq=termFreqsubset)
tdmdf
library(wordcloud2)
tdmplot=ggplot(tdmdf,aes(x=term,y=freq))+
 geom_bar(stat = "identity")+xlab("terms")+ylab("count")+coord_flip()+
 theme(axis.text = element_text(size = 6))
tdmplot


#Creating a wordcloud of the Term Document Matrix

wordcloud(tdm)
wc=as.matrix(tdm)
wordfreq=sort(rowSums(wc),decreasing=T)
wordfreq
# colors(green)
pal=brewer.pal(9,"BuGn")[-(1:4)]
# different words for different types
# different color combination - the command find the best color
pal
nwc=wordcloud(words=names(wordfreq),freq=wordfreq,min.freq = 3,
 random.order = F,colors = pal)
nwc


#Sentiment Analysis

library(sentimentr)
library(syuzhet)
class(articles.corpus)
a=as.character(articles.corpus)
class(a)
mysentiment=get_nrc_sentiment(a)
mysentiment
SentimentScores=data.frame(colSums(mysentiment[,]))
SentimentScores
names(SentimentScores) = "Score"
SentimentScores
SentimentScores= cbind("sentiment" = rownames(SentimentScores), 
 SentimentScores)
SentimentScores
rownames(SentimentScores) = NULL
SentimentScores
n #Plotting the sentiment Scores
sentiment_plot=ggplot(SentimentScores, aes(x = sentiment, y = Score)) +
                                       geom_bar(aes(fill = sentiment), stat = "identity") +
                                       theme(legend.position = "none") +
                                       xlab("Sentiment") + ylab("Score") +
                                       ggtitle("Total Sentiment Score")
sentiment_plot


#Topic Modelling (using Latent Dirichlet Allocation)

articleDtm=DocumentTermMatrix(articles.corpus, control= list(minWordLength = 3))
k = 4
SEED = 1234
article.lda= LDA(articleDtm, k, method= "Gibbs", control = list(seed = SEED))
lda.topics= as.matrix(topics(article.lda))
lda.topics
lda.terms= terms(article.lda)
lda.terms





