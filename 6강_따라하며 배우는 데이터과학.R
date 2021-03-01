#####따라하며 배우는 데이터 과학#####
###6장 통계학습

library(gapminder)
library(tidyverse)
data("sleep")
str(sleep)
view(sleep)
#sleep 함수로 간단하게 데이터 통계량 공부 - 1, 2 그룹으로 나누어진 데이터
#문제를 간단하게 하기 위해 1번 약의 효과만 확인해보자
y <- sleep %>% filter(group == 1) %>% select(extra)
str(y)
y <- as.vector(y)#벡터를 집어 넣었으나 벡터화가 되지 않았다.
is.vector(y) #벡터가 아니라고 한다
is.list(y) #왜 리스트?, 리스트라서 sd 표준편차의 계산이 안된다.
y <- unlist(y) #리스트 화를 풀고 표준편차 계산하기
summary(y) #1번 약을 먹은 사람들의 수면량의 분포는 -1.6에서 3.7까지 이며 중앙값은 0.35이며 중간값은 0.75이다
boxplot(y) #중앙값과 중간값의 차이가 커서 이상치가 있는 줄 알았는데 이상치는 없다.
sd(y) #평균은 0.75 표준편차는 1.789 이다

#화면을 나눠서 시각화 그리기
par(mfrow=c(2,2))
hist(y) #수량형이니까
boxplot(y) #이상치 검증
qqnorm(y);qqline(y) #정규분포 함수에 정규분포 선 추가
hist(y, prob = T) #히스토그램
lines(density(y),lty=2) # 라인 추가해서 확률밀도 함수 라인 추가

#시각화로 정규분포와 비슷하다, 두개의 정상이 있는것 처럼 보인다 그 외의 특이사항이나 이상치는 없다
#평균 0.75 이며 표준편차는 1.8 이며 10명 중 4명은 수면시간이 줄었고 6명은 늘었다... 그래서 ??

#1. 이 수면제는 효과가 있는가?(가설검증)
#2. 이 수면제의 효과는 어느 정도인가?(신뢰구간)
#3. 다른 사람이 먹는 다면 효과는 어느 정도로 나타날것인가?(예측)

t.test(y) #t value = 1.3257 p-value 0.2176
t.test(y, alternative = "greater") #단측 검정으로 신뢰구간과 p-value의 값이 달라짐

#가설검정 - 증가 시간의 평균이 0 이면 효과는 없다, 증가시간의 평균은 0보다 크다를 확인하고 싶다
#H0 : mean = 0, H1 : mean > 0(단측검정),[ H1 : mean != 0 (양측검정)]
#평균은 0.75시간이며, 수면제 효과가 없는데(H0) 0보다 큰 0.75시간이 관측될 확률은 11%이다. - (P 값)
#평균 증가시간의 95%신뢰구간은 -0.53 ~ 2.03 까지이다

##p값?
#부트스트랩 방법으로 p값을 확인해보자
set.seed(1606)  #책의 seed 동일하게 구현
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
sds_star <- rep(NA,B)
ts_star <- rep(NA, B)
for(b in 1:B){
  y_star <- rnorm(n,0, 1.789)
  m <- mean(y_star)
  s <- sd(y_star)
  xbars_star[b] <- m
  sds_star[b] <- s
  ts_star[b] <- m/(s/sqrt(n))
}
  
opar <- par(mfrow=c(2,2))
hist(xbars_star, nclass = 100)
abline(v=0.75, col = 'red')
hist(sds_star, nclass = 100)
abline(v=1.789, col='red')
hist(ts_star,nclass = 100)
abline(v=1.3257, col='red')
qqnorm(ts_star) ; qqline(ts_star)
par(opar)
  
  
#마지막의 qq그림은 직선의 가까울수록 정규분포에 가까우나 양 꼬리부분이 정규분포에서 벗어난다
#이것이 t 분포의 특성- 자유도 즉 표본의 크기가 커질수록 정규분포에 근접해 간다(꼬리쪽도 정규분포에 들어간다)
#일변량의 t검정이 유효하려면
#1.각 관측치가 독립, 2. 동일한 분포를 다르며, 3.그분포는 정규붙포를 따른다

#P-value 는 귀무가설하에서, 관찰된 통계량 만큼의 값이 관찰된 확률
#위 수면제의 p 값 0.10888은 수면제 증가의 평균이 0 일때, 관찰된 0.75시간이 나올 확률은 10.88%이다
#그럼 10.88%sㄴ 큰 값은가? 작은 값은가? - 표본의 크기가 클수록 낮은 p값 획득 가능

##신뢰구간 ?
#95% 신뢰구간은 무엇인가? p-119차레




  






