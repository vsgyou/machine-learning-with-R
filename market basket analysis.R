# 이 데이터는 9,835건의 거래나 일별 약 327건의 거래를 포함한다. 169종류의 아이템이 있다.
# 169 종류의 아이템에 대해 희소행렬을 만들어주기 위해 read.csv 를 사용하지 않고 arules 패키지를 사용
# 행렬의 각 셀은 아이템이 해달 거래에서 구매된 것이면 1 아니면 0이 입력된다.
install.packages("arules")
library(arules)
groceries=read.transactions("C:\\Users\\USER\\Desktop\\머신러닝책\\groceries.csv",sep=",")
summary(groceries)
# 가장 많이 구매된 상품은 whole milk, other vegetables, rolls/buns, soda, yogurt 이다.
# 1개의 상품만을 구매한 건수는 2159, 32개의 상품을 구매한 건수는 1건임을 알 수 있고, 평균 4.4개를 구매하는 것을 알 수 있다.
inspect(groceries[1:5]) # inspect 함수를 통해 희소행렬의 내용을 볼 수 있다.

# 아이템 지지도 시각화
itemFrequencyPlot(groceries,support=0.1)  # 최소 10% 지지도를 갖는 아이템들이 히스토그램에 나타난다.
# bottled water, other vegetables, rolls/buns, soda, tropical fruit, whole milk, yogurt가 있다.
itemFrequencyPlot(groceries,topN=20)   # 상위 20개의 아이템이 내림차순으로 나타난다.
# 희소행렬 도표화
image(groceries[1:5]) # 아이템들의 분포를 그래프상으로 확인 할 수 있다.
# 오른쪽에 보면 3번째와 5번째, 2번째와 4번째가 같은 아이템을 갖는 다는 것을 알 수 있다.

# 모델 훈련
groceryrules=apriori(groceries,parameter=list(support=0.006,confidence=0.25,minlen=2))
summary(groceryrules)
inspect(groceryrules[1:5])
# potted plants를 산 후 whole milk 를 살 확률이 40%로 의미있는 확률이지만 논리적으로 옳지 않음을 알 수 있다.
a=inspect(groceryrules)
?order
dec=order(a$confidence,decreasing=T)[1:5]
a[dec,]  # 신뢰도가 높은 5가지 규칙이다.

# 모델 성능 개선
inspect(sort(groceryrules,by="lift")[1:5])   # 지지도가 높은 5개의 규칙을 찾았다.
# 허브를 산 사람이 뿌리채소를 살 가능성이 뿌리 채소를 산 일반 고객보다 4배가까이 높다.
# 부분 규칙
berryrules=subset(groceryrules,items %in% "berries")
inspect(berryrules)
# 베리는 휘핑크림뿐 아니라 요거트와도 많이 삼을 알 수 있다.
