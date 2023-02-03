# 데이터 가져오기
# 데이터는 1000개의 대출 예시 및 대출과 대출 신청자의 특성을 나타내는 일련의 수치와 특징
credit=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\credit.csv",stringsAsFactors = T)
str(credit) # 1000개의 관측치와 17가지 변수 
table(credit$checking_balance)   # 신청자 수표 계좌 잔고(DM : 독일 통화)
table(credit$savings_balance)    # 저축 계좌 잔고
summary(credit$months_loan_duration)   # 대출기간
summary(credit$amount)  # 대출 금액
table(credit$default)   # yes : 금액 상환 완료, no : 채무 불이행

# 훈련 및 테스트 데이터셋 생성
# 1000 개의 데이터 중 90%를 훈련, 10%를 테스트 데이터로 나눔
set.seed(123)
train_sample=sample(1000,900) 
train_sample
credit_train=credit[train_sample,]
credit_test=credit[-train_sample,]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# 훈련 및 테스트 데이터 셋은 모두 30$의 채무 불이행을 가지므로 잘 나누어 짐

# 모델 훈련 (C5.0 알고리즘)
install.packages("C50")
library(C50)
credit_model=C5.0(credit_train[-17],credit_train$default)
credit_model
summary(credit_model)   # 11%의 오분류율

# 모델 성능 평가
credit_pred=predict(credit_model,credit_test)
library(gmodels)
CrossTable(credit_test$default,credit_pred,prop.chisq=F,prop.c=F,prop.r=F,dnn=c('actual default',
                                                                                'predicted default'))
# 70%의 정확도

# 모델 성능 개선
credit_boost10=C5.0(credit_train[-17],credit_train$default,trials=10)
credit_boost10    #트리의 크기가 기존보다 작아짐
summary(credit_boost10) # 오분류율이 낮아짐
credit_boost_pred10=predict(credit_boost10,credit_test)
CrossTable(credit_test$default,credit_boost_pred10,prop.chisq=F,preop.c=F,prop.r=F,dnn=c('actual default',
                                                                                         'predicted default'))
# 70%의 정확도

# 비용행렬 추가 (cost)
matrix_dimensions=list(c("no","yes"),c("no","yes"))
names(matrix_dimensions)=c("predicted","actual")
error_cost=matrix(c(0,1,4,0),nrow=2,dimnames = matrix_dimensions) 
# 각 항목에 비용을 추가함, (no,no)=0, (yes,no)=1,(no,yes)=4,(yes,yes)=0
credit_cost=C5.0(credit_train[-17],credit_train$default,costs=error_cost)
credit_cost_pred=predict(credit_cost,credit_test)
CrossTable(credit_test$default,credit_cost_pred,prop.chisq=F,prop.c=F,prop.r=F,
           dnn=c('actual default','predicted default'))
# 정확도는 낮아졌으나, 거짓 긍정을 증가시킨 대가로 거짓 부정을 줄임
