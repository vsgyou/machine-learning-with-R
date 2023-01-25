# load file 
data=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\sms_spam.csv", stringsAsFactors = F)
str(data)   # data is 5559, 2 variables
data$type   # "ham" or "spam"
data$type=factor(data$type)
str(data$type) # type is factor("ham","spam")


# package "tm"
install.packages("tm")
library("tm")
data_corpus=VCorpus(VectorSource(data$text)) # VCorpus is read document
print(data_corpus)
inspect(data_corpus[1:2])  # summarize text data

# transform text
data_corpus_clean=tm_map(data_corpus,content_transformer(tolower))   # transform lower word
as.character(data_corpus[[1]])
as.character(data_corpus_clean[[1]])
data_corpus_clean_num=tm_map(data_corpus_clean,removeNumbers) # remove numbers
data_corpus_clean_word=tm_map(data_corpus_clean_num,removeWords,stopwords())
data_corpus_clean_dot=tm_map(data_corpus_clean_word,removePunctuation)  # remove ".",","
removePunctuation("hello...world")  # helloworld
install.packages("SnowballC")
library(SnowballC)
data_corpus_clean=tm_map(data_corpus_clean_dot,stemDocument)   #"helloworld" -> "hello","world"
data_corpus_clean=tm_map(data_corpus_clean,stripWhitespace) 
as.character(data_corpus_clean[[1]])

# message tokenizatin
data_dtm=DocumentTermMatrix(data_corpus_clean)
data_dtm2=DocumentTermMatrix(data_corpus,control=list(
   tolower=T,removeNumbers=T,stopwords=T,removePunctuation=T,stemming=T
))

# train, test set
data_dtm_train=data_dtm[1:4169,]
data_dtm_test=data_dtm[4170:5559,]
data_dtm_train_label=data[1:4169,]$type
data_dtm_test_label=data[4170:5559,]$type
prop.table(table(data_dtm_train_label))   # spam 13%
prop.table(table(data_dtm_test_label)) # spam 13%

# word cloud
install.packages("wordcloud")
library(wordcloud)
wordcloud(data_corpus_clean,min.freq=50,random.order=F)  
spam=subset(data,type=="spam")
ham=subset(data,type=="ham")
wordcloud(spam$text,max.words = 40,scale=c(3,0.5))
wordcloud(ham$text,max.words=40,scale=c(3,0.5))
data_freq_words=findFreqTerms(data_dtm_train,5) # 5회 이상 나타나는 단어

data_dtm_freq_train=data_dtm_train[,data_freq_words]
data_dtm_freq_test=data_dtm_test[,data_freq_words]

convert_count=function(x) {   # 횟수를 Yes/No 문자열로 변환하는 함수
   x=ifelse(x>0,"Yes","No")
}

data_train=apply(data_dtm_freq_train,MARGIN=2,convert_count)
data_test=apply(data_dtm_freq_test,MARGIN=2,convert_count)

# naive bayes classifier
install.packages("e1071")
library(e1071)
data_classifier=naiveBayes(data_train,data_dtm_train_label)
data_test_pred=predict(data_classifier,data_test)
library(gmodels)
CrossTable(data_test_pred,data_dtm_test_label,prop.chisq=F
           ,prop.c=F,prop.r=F,dnn=c('predicted','actual'))  # 36/1390 정확도

# 모델 성능 개선
data_classifier2=naiveBayes(data_train,data_dtm_train_label,laplace=1)
data_test_pred2=predict(data_classifier2,data_test)
CrossTable(data_test_pred2,data_dtm_test_label,prop.chisq=F,prop.c=F,
           prop.r=F,dnn=c('predicted','actual'))   # 34/1390 정확도
