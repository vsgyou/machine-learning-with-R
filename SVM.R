# 데이터 수집
# 20000개의 데이터가 17개의 특징으로 이루어져있다.
letters=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\letterdata.csv")
str(letters)

# 훈련용, 테스트용 나누기
letters_train=letters[1:16000,]
letters_test=letters[16001:20000,]

# 모델 훈련
install.packages("kernlab")
library(kernlab)
letter_classifier=ksvm(letter~.,data=letters_train,kernel="vanilladot")

# 오류가 떠 진행되지 않는다, 데이터의 letter 변수를 factor 형태로 바꾸어 준다.
letters$letter=as.factor(letters$letter)
letters_train=letters[1:16000,]
letters_test=letters[16001:20000,]
letter_classifier=ksvm(letter~.,data=letters_train,kernel="vanilladot")

# 모델 성능 평가
letter_predictions=predict(letter_classifier,letters_test)
head(letter_predictions)
table(letter_predictions,letters_test$letter)

agreement=letter_predictions==letters_test$letter
table(agreement)  # 예측한 결과와 실제 결과가 3357개가 같았고, 643개가 다르게 나타났다.
prop.table(table(agreement))  # 정확도는 약 0.83925이다.

# 모델 성능 향상(커널 함수 변경)
letter_classifier_rdf=ksvm(letter~.,data=letters_train,kernel="rbfdot")
letter_predictions_rbf=predict(letter_classifier_rdf,letters_test)
agreement_rbf=letter_predictions_rbf==letters_test$letter
table(agreement_rbf) # 3723개의 성공과 277개의 실패로 정확도가 좋아짐.
prop.table(table(agreement_rbf)) # 커널 함수의 변화로 정확도가 약 0.93가 되었다.

# 모델 성능 향상(최적 svm 비용 파라미터 알아내기)
cost_values=c(1,seq(from=5,to=40,by=5))   # 비용상수를 (1,5,10,15,20,25,30,35,40)로 바꿔가며 모델 적합
accuracy_values=sapply(cost_values,function(x){
   set.seed(12345)
   m=ksvm(letter~.,data=letters_train,kernel="rbfdot",C=x)
   pred=predict(m,letters_test)
   agree=ifelse(pred==letters_test$letter,1,0)
   accuracy=sum(agree)/nrow(letters_test)
   return(accuracy)
})
plot(cost_values,accuracy_values,type="b")

# 비용상수가 커질수록 정확도가 좋아지는 것을 알 수 있는데, 10 이상부터는 큰 변화가 없음을 알 수 있다.
