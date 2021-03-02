#####따라하며 배우는 데이터 과학#####
###7장 데이터종류에 따른 분석 기법 - 80%의 실제 문제는 20%정도의 통계 기법으로 처리 가능
#선형모형, 일반화 선형, 고차원통계학습 -라쏘, 랜덤포레스트
#다양한 머신러닝 제외한다.

#모든 데이터프레임 데이터에 tbl_df 처리 이후
#0.모든데이터  - glimpse, summary, plot, pairs
#1.수량형변수 - hist, boxplot, density, mean, median, t.test 
#2.범주형변수 - table, xtabs, barplot, binom.test
#3.수량x, 수량y - polt, cor, lm, lqs(로버스트회귀), 비모수회귀
#4.범주x, 수량y - boxplot, ANOVA
#5.수량X, 범주Y(성공실패) - plot, boxplot, glm(family='binom')

library(tidyverse)
mpg <- tbl_df(mpg)
is.data.frame(mpg)
mpg
dplyr::glimpse(mpg)
pairs(mpg %>% sample_n(10)) #숫자가 아닌 변수가 포함되어 그림이 안된다
summary(mpg)
mpg %>% select(manufacturer) %>% distinct()

#수량형변수 -hist, boxplot, density, mean, median, t.test 
#시각화 - ggplot + geom_(histogram, density), hist, boxplot 사용 추천
#데이터 정규성 검증 - qqplot, qqline 로 정규분포 확인
#이상치 찾기

mpg$hwy
summary(mpg$hwy)
quantile(mpg$hwy)

opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy) ; qqline(mpg$hwy) #양 끝부분이 정규분포에서 벗어난다 -> t 분포와 비슷하다
opar


#평균이 22.9보다 크다를 확인하고 싶다
hwy <- mpg$hwy
mu0 <- 22.9  #H0 : mean <=22.9 H1 : means >=22.9 귀무가설을 평균은 22.9보가 같거나 작다
t.test(hwy, mu=mu0, alternative = "greater")
#t=1.3877, p-vaiue=0.08328 
#평균이 22.9일때 우리가 관찰할 23.44가 평균으로 관찰될 확률은 8.3%이다.
#유의수준이 10%일때는 연비가 22.9보다 크다고 말할수 있으나
#유의수준이 5%일때는 연비가 22.9보다 크다고 말하기 어렵다.

t.test(hwy) #귀무가설 없이 신뢰구간 확인
#t=60.216m p-value=2.2e-16 95%신뢰구간 22.67~24.21
diff(t.test(hwy)$conf.int) #신뢰구간의 길이는 1.533이다

mpg %>% filter(hwy>40)
mpg %>% filter(model == "new beetle") %>%group_by(year) %>% summarise(mean(hwy))
mpg %>% filter(model == "new beetle")

#일단 hwy가 40이 넘는 것은 이상치로 확인된다. by boxplot로 


