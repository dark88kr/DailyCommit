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
#수량형x, 수량형 y - plot, ggplot2 :: geom_point + jitter, alpha= 
#상관관계 계산 , 선형모형에 적합도 표기, 로버스트 회귀분석, 비선형에는 LOESS 사용

ggplot(mpg,aes(cty,hwy)) + geom_jitter() + geom_smooth(method = "lm") #산점도 그리기

#상관관계 계산시 사용하는 cor 함수는 피어슨상관계수가 기본 - 선형관계의 강도를 -1~1사이의 숫자로 표기
#상관관계 계산시 이상치의 영향을 많이 받으므로 캔달의 타우나 스피어만의 로로 계산 가능
cor(mpg$cty,mpg$hwy)
with(mpg,cor(cty,hwy))
with(mpg,cor(cty,hwy,method = "kendall"))
with(mpg,cor(cty,hwy,method = "spearman"))

#선형회귀 분석시 lm모형에 사용되는 것은 최소제곱법으로 추정한다(잔차의 제곱합을 최소화 하는 방법)
hwy_lm <- lm(hwy~cty,data = mpg)
summary(hwy_lm)

#선형모형을 구했으면, 이 모형이 적합한지 확인하기
#multiple R-squared : 0.9138, Adjusted R-squared : 0.9134
#R^2의 값은 총 변동 중, 회귀선으로 설명가능한 값 - 0.9138이면 회귀식이 전체 변동의 91.38% 설명한다.
#설명변수가 추가 될때마다  R^2의 값은 항상 상승  -> Adjusted R^2이 등장
#F-statistic은 HO : 설명변수가 반응변수에 효과가 없다, - P-value가 작기 때문에 설명변수가 효과 있다 의미.

predict(hwy_lm) #기존데이터를 사용하기 때문에 다른 데이터 사용시 newdata 옵션 사용
resid(hwy_lm) #잔차 확ㅇ
predict(hwy_lm, newdata = data.frame(cty=c(10,20,30)))

#위 선형회귀식에 cty의 값이 10,20,30 일때 14.27, 27.64, 41.02로 hwy값 예측함.

#그럼 위에 생성한 회귀식이 맞는 것인가? - 4가지 조건에 충족되는가?
#회귀분석 진단 - 중요
opar <- par(mfrow = c(2,2))
plot(hwy_lm,las=1)
opar

#___________3월 5일에 다시 _____

#1.잔차의 정규성 - 잔차의 분포가 정규분포를 따른다 - Shapiro-Wilk test 와 Kolmogorov-Smirnov test
#표본이 2000개 미만 - shapiro, 2000개이상 kolmogorov
#shapiro.test에 회귀식의 잔차항 넣는다 H0 : 표본의 모집단이 정규분포를 따른다, H1 : 정규분포 아님
shapiro.test(hwy_lm$residuals) 
#p-value : 0.02945, 0.05 보다 작으므로 5%유의수준에서 잔차가 정규분포를 따른다를 기각한다.

#2.잔차의 독립성 - Durbin-Watson 검증
car::durbinWatsonTest(hwy_lm)

install.packages("car")
lmtest :: dwtest(hwy_lm)



