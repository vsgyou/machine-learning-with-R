# 데이터 소개
# 주름진 버섯23종, 8,124개의 버섯 샘플에 대한 정보
# type 변수는 (식용, 독이있는, 독이 있을 가능성이 있으며 먹을 것을 권장하지 않음)으로 이루어짐
# 갓 모양, 갓 색상, 냄새, 주름 크기와 색, 줄기모양, 서식지와 같은 변수가 있음
mushrooms=read.csv("C:\\Users\\USER\\Desktop\\머신러닝책\\mushrooms.csv", stringsAsFactors = T)
str(mushrooms)
mushrooms$veil_type=NULL  # 모든 값이 'partial'로 동일하므로 분석에서 제외
table(mushrooms$type)   # 4208개의 식용과 3916개의 독성을 가진 버섯

# 1R 분류기를 적용( 한가지 규칙으로만 분류 )
install.packages("OneR")
library(OneR)
mushroom_1R=OneR(type~.,data=mushrooms)
# odor(냄새)에 따라 creosote, fishy, foul, musty. ppungent, spicy 인경우 독이 있는 것으로 분류
mushroom_1R_pred=predict(mushroom_1R,mushrooms)
table(actual=mushrooms$type,predicted=mushroom_1R_pred)  # 독이 있음에도 식용이라고 예측한 120개의 오분류가 발생

# 리퍼규칙 학습 알고리즘 사용
install.packages("RWeka")
library(RWeka)
mushroom_JRip=JRip(type~.,data=mushrooms)
# 냄새가 취야하면 독버섯이다
# 주름 크기가 좁고, 주름색이 담황색이면 독버섯이다.
# 주름의 크기가 좁고, 톡쏘는 냄새가 나면 독버섯이다.
# 등의 9가지 규칙이 만들어졌고, 오분류된 경우는 없다.
