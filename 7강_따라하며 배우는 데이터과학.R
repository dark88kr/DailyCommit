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

#일단 진도 나가고, 회귀진단에 관한 내용은 따로 공부할 예정
#https://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html - 참고 예정

#선형회귀에 최소제곱방법은 이상치에 민감하게 반응한다.
#로버스트 회귀분석은 이상치에 민감하지 않다.

install.packages("MASS")
library(MASS)
set.seed(123) #책과 동일한 결과를 위해서
lqs(stack.loss ~ .,data = stackloss)

lm(stack.loss ~.,data = stackloss)

#선형관계의 강도만 측정, 비선형 모형의 경우 비선형회귀를 사용하거나, 다항회귀분석 사용
#이러한 모형보다 비선형의 경우, 아무런 가정도 하지 않는 평활법(smoothing) 사용
#그 중에서도 국소회귀방법 LOESS가 가장 많이 사용된다
plot(hwy ~ displ, data = mpg)
mpg_lo <- loess(hwy~displ,data = mpg)
mpg_lo
summary(mpg_lo)

ggplot(mpg,aes(displ,hwy)) + 
  geom_point()+
  geom_smooth()



#두 변수의 분석 
#범주형x, 수량형 y - 병렬상자그림을 사용하여 데이터를 시각화 한다
#집단간에 평균, 중앙값의 차이가 존재하는지 각 집단의 분산은 유사한지등
#lm함수로 ANOVA선형모형적합
#plot.lm으로 잔차의 분포를 살펴본다

#ANOVA(분산분석)
#x가 범주형이고 y가 수량형인 경우, 분산분석을 사용한다.(집단개수 2개 two-sample-t.test는 특별한 경우)
#분산분석도 선형회귀분석처럼 수학적으로는 동일한 모형이다.

mpg %>% ggplot(aes(class,hwy)) + geom_boxplot()
#분산분석 실행
hwy_lm2 <- lm(hwy ~ class,data = mpg)
summary(hwy_lm2)

#classrk pickup 이면 연비가 -7.92만큼 감소한다.
#표준오차 1.617이면 95%신뢰수간은 -7.92+-1.96*162 = [-11.1,-4.74]로 신뢰구간
#자유도가 227로 t값은 z값과 유사하기 때문에 z분포의 95%값 1.96 사용
#H0 : 평균 0, H1 : 평균 1, p-value가 유효하므로 HO 기각
#평균 연비 차이가 다른 Class와 차이가 없을때, -7.92감소로 관측될 확률은  0 에 가깝다.H0 기각

#분산분석 역시 3가지 가정 필요 - 해당 분석검정에 대해서는 따로 학습 하겠음
#1.잔차의 분포가 독립
#2.잔차의 분산이 동일
#3.잔차의분포가 전규분포를 따른다

#두 변수의 분석 
#수량형x, 범주형 y - x와 y의 산점도를 그려본다, y변수 그룹별로 x변수를 병렬상자그림 그려본다
#y값에 따른 x의 분포의 차이가 있는지. 이상치, 표본로그오즈와 x의 산점도에 선형 패턴이 있는지
#glm함수로 일반화 선형모형 적합
#plot/gim으로 잔차의 분포를 살펴본다


#범주형 변수의 일반화 선형모형 로짓,로지스틱 함수
#y의 값이 0과 1 이항분포로는 선형모형 불능, 로짓변환시 무한대의 값으로 변경 가능 - link함수 사용
#최대우도법으로 계산하여 베타값을 구하고 무한대의 선형 추정값을 확률변수로 변환하기 위해 로짓의 역함수 사용
#이항분포와 로짓링크 함수를 사용한 GLM모형을 로지스틱 회귀모형이라 한다

ch<- read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv')
ch <- tbl_df(ch)
glimpse(ch)
ch %>% select(distress_ct) %>% distinct()

#첼린저호 분석, 6개의 o_ring 중 몇개의 링이 실패했는지(distress_ct)
#x는 온도, y는 distress의 산점도

ch %>% ggplot(aes(temperature,distress_ct)) + geom_point() #산점도
ch %>% ggplot(aes(factor(distress_ct),temperature)) + geom_boxplot() #병렬상자


#glm에서 y가 0과 1 사이 숫자면 y=성공/시도횟수, 성공이 0또는 1이면 1의 값을 성공으로 확인
#y값이 0-1 숫자형 벡터이면 0은 실패, 1은 성공
#y값이 2차원 매트릭스 (2개의 컬럼) 일때 첫열을 성공회수, 뒷열은 실패횟수로 인식
#실패한 o_ring을 성공으로 설정, 0_ring - distress_ct를 실패횟수로 인식

ch_lm <- glm(cbind(distress_ct,o_ring_ct-distress_ct) ~
               temperature, data = ch, family = "binomial")
ch_lm
summary(ch_lm)
#온도의 계수는 -0.17949는 어떻게 해석? - 온도가 1도 상승하면, 성공비율이 0.179만큼 감소한다.


#마지막 줄은 모형의 적합도
#null deviance 20.706 on 22자유도 - 모형적합하기 전 deviance
#Residual deviance 9.527 on 21자유도 - 모형적합후 deviance
#둘 사이가 "충분히" 줄어다면 이 모형은 적합하다고 판단 
#두 deviance의 차이는  HO 가정아래 카이스퀘어 분포를 따르며 - H0:적합하지 않다, H1:적합 
1-pchisq(11.2,1) # 두 deviance 차이는 11.2 그리고 자유도는 1인 카이스퀘어 테스트(22-21)
#p-value는 0.0008179733로 0에 가까우므로 H0기각, 모형이 적합하다고 판단 가능 하다/


#적합하다고 판단할수 있는 모형에 30도를 집어 넣으면?
predict(ch_lm,data.frame(temperature=30))
#8.82 -0.179*30의 값은 3.432159이다, 이렇게 넣어서 계산하든가-> exp(3.43)/(exp(3.43)+1)

predict(ch_lm, data.frame(temperature=30),type="response")
#96.87로 성공한다 -> 96.87%로 oring이 파괴된다


#7강 종료
