# 데이터 수집
# 이 데이터셋은 1,030개의 콘크리트 예시를 포함하며, 혼합에 사용된 구성 요소를 설명하는 여덟 개의 특징을 갖는다.
# compressive strength(최종 내압 강도), aging time(숙성 시간), cement(시멘트), slag(슬래그), ash(재), water(물), superplasticizer(고성능 감수제),
# coarse aggregate(굵은 골재), fine aggregate(작은 골재)의 특징을 포함한다.

concrete=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\concrete.csv")
str(concrete)
# 특징들의 범위가 다양한 것을 알 수 있다. 이에 대해 정규화를 진행한다.
normalize=function(x) {
   return((x-min(x))/(max(x)-min(x)))
}  # 최대최소 정규화 (0부터 1까지로 정규화됨)

concrete_norm=as.data.frame(lapply(concrete,normalize))
str(concrete_norm)   # 자료들의 범위가 0부터 1로 정규화 됨
summary(concrete_norm$strength)  
summary(concrete$strength) # 정규화 이전의 최대 최소값은 2.33, 82.6

# 훈련데이터, 테스트데이터 나눔
concrete_train=concrete_norm[1:773,]
concrete_test=concrete_norm[774:1030,]

# 신경망 모형 적용
install.packages("neuralnet")
library(neuralnet)
concrete_model=neuralnet(strength~.,data=concrete_norm)  # 은닉노드가 1개
plot(concrete_model) # SSE : 6.91851

concrete_model2=neuralnet(strength~.,data=concrete_norm,hidden=2)  # 은닉노드가 2개
plot(concrete_model2) # SSE : 3.645706

concrete_model3=neuralnet(strength~.,data=concrete_norm,hidden=3)  # 은닉노드가 3개
plot(concrete_model3) # SSE : 3.211063 

# 은닉노드가 늘어날수록 SSE가 줄어드는 것을 알 수 있다. 은닉노드가 2개일때와 3개일 때의 차이가 작으므로 2개가 적당하다고 생각된다.

# 모델성능평가
model_results=compute(concrete_model,concrete_test[1:8]) # predict함수와 달리 계층별로의 뉴런과 예측값을 알려준다.
predict_strength=model_results$net.result
cor(predict_strength,concrete_test$strength) # 분류문제가 아니므로 혼동행렬(CrossMatrix를 사용하지 않고 상과계수를 본다)
# 0.8077178 로 매우 강한 상관관계를 보이므로 좋은 성능임을 알 수 있다.

# 모델 개선(은닉노드의 수를 늘려 성능을 개선해본다.)
concrete_model_2=neuralnet(strength~.,data=concrete_train,hidden=5)
plot(concrete_model_2)  # SSE : 1.903098로 많은 변화가 생겼다.
model_results_2=compute(concrete_model_2,concrete_test[1:8])
predict_strength_2=model_results_2$net.result
cor(predict_strength_2,concrete_test$strength)
# 상관계수가 0.93으로 많은 개선이 이루어졌다. 

# 모델 개선 2 (활성화 함수를 ReLU를 이용함. ReLU와 유사한 소프트 플러스 사용)
softplus=function(x){log(1+exp(x))}
concrete_model_3=neuralnet(strength~.,data=concrete_train,hidden=5,act.fct=softplus) 
plot(concrete_model_3)  # SSE : 1.755165
model_result_3=compute(concrete_model_3,concrete_test[1:8])
predict_strength_3=model_result_3$net.result
cor(predict_strength_3,concrete_test$strength)
# 상관계수가 0.93으로 좋은 성능을 보인다.

# 정리
strengths=data.frame(actual=concrete$strength[774:1030],pred=predict_strength_3)
head(strengths,n=3)
cor(strengths$pred,strengths$actual)
# 상관계수는 0.933으로 정규화를 하기 전과 한 후는 상관계수가 동일함을 알 수 있다.
