# 데이터 준비
# 1,338명의 의료ㅛ보허에 등록된 수익자 예시 데이터이다. 
# 환자의 특성과 해당 연도에 의룝험에 청구된 전체 의료비를 나타내는 특징으로 구성돼 있다.
# age, sex, bmi, children, smoker, region 의 특징
insurance=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\insurance.csv",stringsAsFactors = T)
str(insurance)
# 데이터 분포 확인
hist(insurance$expenses)
# 오른쪽으로 꼬리가 긴 분포를 보여주며, 대다수의 사람들이 0~15,000사이에 있다.
table(insurance$region)
# 네 지역에 고르게 분포되어 있음(324, 325, 364, 325)
cor(insurance[c("age","bmi","children","expenses")])
# 네 변수끼리의 상관관계를 보아 대부분 강하지 않은 상관관계를 가지고 있다.

# 산포도 행렬 시각화
pairs(insurance[c("age","bmi","children","expenses")])
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","expenses")])

# 모델 훈련
ins_model=lm(expenses~.,data=insurance)
# expenses=-11941.6 + 256.8*age + -131.4*sexmale + 339.3*bmi + 475.7*children + 23847.5*smokeryes+ -352.8*west+ -1035.6*east+ -959.3*west
# 남성의 경우 여성에 비해 131.4만큼 적게들고, 북동 지역의 평균 비용이 가장 높은 경향이 있음을 알 수 있다.

# 모델 성능 평가
summary(ins_model)
# 수정된 R-제곱 값이 75%로 모델이 expenses의 변화량의 약 75%를 설명하고, p-value가 매우 작은것으로 보아 모델이 유의하다.

# 모델 성능 개선
insurance$age2=insurance$age^2   # 비선형 관계 추가 : y=a+bx -> y=a+bx^2
insurance$bmi30=ifelse(insurance$bmi>=30,1,0)   # 변환 : 수치변수를 이진변수로, bmi가 30 이상이면 1, 아니면 0
ins_model2=lm(expenses~age+age2+children+bmi+sex+bmi30*smoker+region,data=insurance)
summary(ins_model2)
# 수정된 R-제곱 값이 75%에서 86%로 개선되었다. p-value를 보아 새로 추가된 변수 age2와 bmi30은 유의한 변수이다.

# 회귀 모델로 예측
insurance$pred=predict(ins_model2,insurance)
cor(insurance$pred,insurance$expenses) #상관계수가 0.93으로 매우 강한 선형 관계가 있음, 모델이 상당히 정확함
plot(insurance$pred,insurance$expenses)
predict(ins_model2,data.frame(age=30,age2=30^2,children=2,bmi=30,sex="male",bmi30=1,smoker="no"
                              ,region="northeast"))
# 위의 값을 대입했을때 예상 비용은 5973이 나온다.
predict(ins_model2,data.frame(age=30,age2=30^2,children=2,bmi=30,sex="female",bmi30=1,smoker="no"
                              ,region="northeast"))
# 동일 조건에서 여성의 경우 6470이 나오는데, 이는 모델의 회귀계수인 -496.7690으로 알 수 있다.
