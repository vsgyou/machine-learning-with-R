# load file 
data=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\wisc_bc_data.csv",header=T, stringsAsFactors = F)
# This data is breast cancer data.
str(data)   # data is 569, 32 variables
data=data[-1]  # remove id variable
table(data$diagnosis)   # positive/negative=357/212
data$diagnosis=factor(data$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))

# normalize
normalize=function(x) {
   return((x-min(x))/(max(x)-min(x)))
}
data_n=as.data.frame(lapply(data[2:31],normalize)) #max min normalize
summary(data_n)

# divide data (train,test)
data_train=data_n[1:469,]
data_test=data_n[470:569,]
data_train_labels=data[1:469,1]
data_test_labels=data[470:569,1]

# k-nn package
install.packages("class")
library(class)
data_pred=knn(train=data_train,test=data_test,cl=data_train_labels,k=3)
# gmodels package ( cross table )
install.packages("gmodels")
library(gmodels)
CrossTable(x=data_test_labels,y=data_pred,prop.chisq=F)  # accuracy = 97%

# other normalize method

data_z=as.data.frame(scale(data[-1]))# z-score normalize
data_z_train=data_z[1:469,]
data_z_test=data_z[470:569,]
data_z_train_labels=data[1:469,1]
data_z_test_labels=data[470:569,1]
data_z_pred=knn(train=data_z_train,test=data_z_test,cl=data_z_train_labels,k=21)
CrossTable(x=data_z_test_labels,y=data_z_pred,prop.chisq=F) # accuracy = 95%

# The first one is better in terms of accuracy.