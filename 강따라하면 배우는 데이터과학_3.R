#####따라하며 배우는 데이터 과학#####
###3장 dplyr 패키지

install.packages("gapminder")
library(gapminder)
library(tidyverse)

str(gapminder)
gapminder %>% select(country) %>% distinct()

gapminder %>% filter(country == "Korea, Rep.") %>% select("pop", "gdpPercap")#행과 열 선택

#요약통계량 계산
summary(gapminder)

iris_tbl <- tbl_df(iris)
iris_tbl
str(iris_tbl)
glimpse(iris_tbl)

gapminder %>% arrange(year,country)

#샘플추출하는 방법 - 두 방법 모두 비복원추출이 기본값, set.seed 함수로 동일 샘플 설정 가능
#replace = T 옵션으로 복원추출 설정, weight 옵션으로 가중치 지정
gapminder %>% sample_n(10) #gapminder의 데이터에서 10개만 뽑아라 
gapminder %>% sample_frac(0.01) #gapminder의 데이터에서 0.01 즉 1%만큼의 샘플을 추출하라


#고유한 행 찾기
gapminder %>% select(year) %>% distinct() #year 열의 고유 값 확인

#group_by와 summarize 로 그룹별 통계량 구하기

gapminder %>% 
  filter(year == 2007) %>%   # year 값이 2007인 행만 골라서
  group_by(continent) %>%    # continent 별로 그룹을 지어서
  summarise(median(lifeExp)) # lifeExp 열의 중간값을 구하라

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(mean(lifeExp))

#2007년의 대륙별 평균 기대수명과 평균수명 중간값의 차이가 크지 않다는 것은 이상치가 없다는 뜻

gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarise(life = median(lifeExp)) %>% 
  arrange(-life)  # 2007년의 대륙별 기대수명 중간값을 life로 저장하고 큰수부터 정렬하라


###dplyr에서 사용하는 조인 함수
df1 <- dplyr::data_frame(x=c(1,2),y=2:1)
df1
df2 <- dplyr::data_frame(x=c(1,3),a=10,b="a")
df2

#df1의 x를 기준으로 df1의 x의 행과 df2의 x의 행 값이 맞는 것과 추출 - df1과 df2의 x값이 안맞으면 추출 x
df1 %>% inner_join(df2) 

#df1의 x를 기준으로 df1의 전체 데이터와 df1의 x와 동일한 값의 df2의 x값이 있는 경우 추출, 나머지는 na처리 
df1 %>% left_join(df2)

#df2를 기준으로 하며 left 조인과 동일함
df1 %>% right_join(df2)

### 4장 데이터 시각화
#변수를 확인하는 방법

gapminder %>% select(gdpPercap,lifeExp) # 1700개 가까이 되는 데이터라서 패턴? 모양 찾기 어려움
gapminder %>% select(gdpPercap,lifeExp) %>% summary() #간단하게 산술평균으로 확인은 가능
cor(gapminder$gdpPercap,gapminder$lifeExp) #두 변수간의 상관관계도 확인 가능 0.58

#그래서 더 쉽게 확인 하기 위해 시각화를 한다.

oper = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass = 50)
hist(log10(gapminder$gdpPercap), nclass = 50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=0.5)
per(oper)

#상관관계는 method = "pearson"(피어슨 상관계수) 가 기본값
#변수간의 관계가 비선형인 경우 "kendall" 또는 "spearman" 사용하여 비모수방법인 "캔달" "스피어만" 사용
cor(log10(gapminder$gdpPercap),gapminder$lifeExp) #평균소득에 로그하여 정규분포에 맞게 설계, 상관계수 증가

#그래프를 그려서 변수간의 관계 모양을 확인하는 것도 시각화의 중요한 역활 중 하나.

oper = par(mfrow=c(2,2))
gapminder %>% ggplot(aes(x=lifeExp)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() +scale_x_log10() + geom_smooth()

#베이스 그래프와 비교하여, ggplot는 다양한 플룻 타입을 하나의 개념으로 처리하고, 다변량데이터에 효율적


#변수의 종류에 따른 데이터 시각화 기법 - diamonds 와 mpg 데이터 사용

#수량형 - 구체적인 수치를 가지는 변수 gpdPercap, lifeExp, 키, 몸무게
#범주현 - 소수의 가능한 값을 가지는 변수 국가, 성별, 학점(순서형)

#하나의 수량형 변수 - 도수 히스토그램
#시각화 하여 1.이상점 2.전반적분포 3.어떤변환하면 정규분포와 가까울지 4.히스토그램 binwidth값 조정
#gdpPercap 수량형 변수 사용

gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() 
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly() +scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_density() +scale_x_log10()






