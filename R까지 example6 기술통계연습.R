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

