#책실습하기 
#R까기2
#example 6. 전국연령별 평균 월급조사 (기술통계)

library(tidyverse)
library(ggplot2)
library(ggthemes)

df<-read.csv("example_salary.csv", stringsAsFactors = T, na="-")
#"-"표시는 -로 표기된데이터를 NA 처리 한다는 뜻이다
str(df)
head(df,10)
colnames(df)
colnames(df) <-c("age","salary","special_salary","working_time","number_of_worker","career","sex")
#데이터 살펴 보고, 컬럼값을 편집하기 쉽게 영문으로 표기
attach(df)

means<-mean(salary,na.rm = T)
means
#평균은 2,171,578원
mids<-median(salary, na.rm = T)
mids
#중간값은 2,120,345원
ran<-range(salary, na.rm = T)
#점위는 111만원에서 406만원
#도데체 406만원은 누가 받는건지?
which(salary == 4064286)
df[48,]
#which로 몇번째 행인지 확인 후 df[48,]로 48번째 줄의 전체 컬럼값 확인
#50~54세 사이의10년 이상의 남성의 경우 406만원 받는다

mean(working_time,na.rm = T)
median(working_time, na.rm = T)

#406만원 받는 집단의 일하는 시간이 더 긴것이 아닌지 확인해 봤는데... 평균이다


qnt<-quantile(salary, na.rm = T)
qnt
boxplot(qnt, horizontal = T)
#그림으로 그려봄... 75%가 250만원 미만으로 받고 있구나... 그렇구나..

salary_list<-list("mean_salary"=means, "median_salary"=mids,"range_salary"=ran,"quantile_salary"=qnt)
salary_list

# 보고싶은 값이 있다면 이렇게 리스트로 담아서 출력하면 한눈에 쉽게 확인 가능.


#example 7. 그룹별 평균 구하기 (기술통계)
#sex 항목을 이용하여 성별 평균 월급 구하기

library(tidyverse)
library(ggplot2)
library(ggthemes)
str(df)

#subset으로 각 성별로 항목 만들어서 평균 구하기보다는 tapply 함수 사용
#tapply는 변수를 기준으로 그룹만들고 그룹별로 함수 적용, 첫번째 인자를 두번째 인자를 기준으로 세번째 함수 사용

tem <- tapply(df$salary,df$sex,mean, na.rm = T) #na.rm = T 가 없어서 NA 값 나옴
df %>% group_by(sex) %>% summarize(mean(salary, na.rm = T))
#같은 결과 값, 다른 함수 사용

#성별 표준편차
tapply(df$salary,df$sex,sd,na.rm=T)
df %>% group_by(sex) %>% summarize(sd(salary,na.rm = T))

tapply(df$salary,df$sex,range,na.rm=T)


#경력별 평균월급
tem <- tapply(df$salary,df$career,mean, na.rm = T)
tapply(df$salary, df$career,sd,na.rm=T)
tapply(df$salary, df$career,range,na.rm=T)
a1 <- df[which(df$salary == 1117605),]
a2 <- df[which(df$salary == 1172399),]
a3 <- df[which(df$salary == 1245540),]
a4 <- df[which(df$salary == 1548036),]
a5 <- df[which(df$salary == 1685204),]
salay_list_by_career <- list(a1,a2,a3,a4,a5)
salay_list_by_career 


#list 입력 및 tapply 연습 그리고 dplyr 연습.



