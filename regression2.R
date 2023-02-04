# 데이터 준비
# 데이터는 4,898개의 와인 샘플과 11가지 화학 속성에 대한 정보가 들어있다.
# acidity(산도), sugar content(당 함유량), chloriides(염화물), sulfur(황), alcohol(알코올), pH, density(밀도)
# regression1 과 다르게 의사 결정  트리를 이용해 예측한다.
wine=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\whitewines.csv")
str(wine)
hist(wine$quality)   # 품질 평가는 6을 기준으로 정규적인 분포를 띄는 것 같다
wine_train=wine[1:3750,]
wine_test=wine[3751:4898,]
# 훈련용 75%와 테스트 25%로 나눔
# 모델 훈련 cart 함수
install.packages("rpart")
library(rpart)
m.rpart=rpart(quality~.,data=wine_train)
# 의사 결정 트리 시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart,digits=3)  # digits (소숫점 3자리에서 반올림)
rpart.plot(m.rpart,digits=4,fallen.leaves = T,type=3,extra=101)   # type, extra는 다이어그램의 출력 형식 조정

# 성능평가
p.rpart=predict(m.rpart,wine_test)
summary(p.rpart)  # 실제 값은 3~9로 분포되어있지만, 예측값은 더 좁은 범위를 가짐
MAE=function(actual,predicted){
   mean(abs(actual-predicted))
}
MAE(p.rpart,wine_test$quality)   # MAE : 평균 절대 오차 , 오차들의 절댓값의 합을 갯수로 나눔
# MAE의 값이 약0.59로, 평균적으로 모델의 예측과 실제 품질 점수 간의 차가 약 0.59라는 것을 의미

# 모델 성능 개선 (큐비스트 알고리즘)
install.packages("Cubist")
library(Cubist)
m.cubist=cubist(x=wine_train[-12],y=wine_train$quality)
m.cubist    # 25개의 규칙을 생성함
summary(m.cubist)
p.cubist=predict(m.cubist,wine_test)
summary(p.cubist) # 범위가 3.677에서 7.393으로 이전보다 커졌다.
cor(p.cubist,wine_test$quality)  # 예측값과 실제 데이터의 상관관계가 0.6으로 증가했다.
