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


#범주형변수(성공과실패) - table, xtabs, barplot, binom.test
#여론조사의 찬성과 반대, 웹의 클릭 또는 클릭하지 않음
#요약통계 - table, xtabs, prop.table로 상대도수
#시각화 barplot가 요용, 신뢰구간은 binom.test로 성공률의 신뢰구간 과 검정 가능

set.seed(1606) #책과 동일한 값 추출을 위해 동일 랜덤변수 설정
n <- 100 #100개의 샘플
p <- 0.5 #지지율 50%로 설정
x <- rbinom(n,1,p) #이항정구분포에 따라 100개 샘플 생성하고 1의 확률은 50%로 설정
x <- factor(x, levels = c(0,1), labels = c("no","yes")) # factor로 설정하고 0과1 레벨설정하고 이름은 N, Y
x
table(x) #x의 도수분포
prop.table(table(x)) #x의 상대도수
barplot(table(x)) #x의 시각화

#실제 지지율 50%으로 설정했지만 모른다고 가정...
#H0 : p = 0.5, H1 : p != 0.5

binom.test(x=length(x[x=="yes"]),n=length(x),p=0.5,alternative = "two.sided")
binom.test(54,n=length(x),p=0.5,alternative = "two.sided") #성공횟수, 시도횟수, 검증하고 싶은 성공의 확률
#p-value : 0.4841로 H0를 기각할 증거는 희박하다, P=0.5이다
(0.64-0.44)/2 # 0.1 즉 10%가 오차한계이다
#표본이 100개 늘어나면 오차한계는 1/10로 줄어 들었다. - sqrt(n)의 범위로 오차한계가 줄어든다.



#두 변수의 분석 
#수량ㅎ




