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
data_corpus_clean=tm_map(data_corpus_clean_dot,stemDocument)   #"helloword" -> "hello","world"
data_corpus_clean=tm_map(data_corpus_clean,stripWhitespace) 
as.character(data_corpus_clean[[1]])
