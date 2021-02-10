######책실습하기 
######R까기2
###확률파트의 실전예제 19번 10년치 편의점 데이터 분석하기
library(tidyverse)
library(data.table)
library(stringr)

#데이터 불러오기
#엄청 큰 데이터라서 data.table의 fread함수로 불러와야 하며 data.table = f 기능으로 data.frame으로 나타남

system.time(df <- fread("example_conveniencestore.csv",
                        encoding = "UTF-8",
                        data.table = F))
df <- fread("example_conveniencestore.csv",
      encoding = "UTF-8",
      data.table = F)
df #습관처럼 불러오려 했으나 데이터가 너무 크다

#샘플함수로 표본을 추출한다
s <- df[sample(nrow(df), 5000),]
str(s)
#5개의 변수와 5000개의 데이터 존재
colnames(s) #5개 이름

#매일 매일 가장 많이 팔리는 상품, 그 상품이 시간당 파리리는 개수를 비율로 표시  - 목표!!

#가장먼저 날짜변수를 시간단위로 바꾸고 시간을 그룹으로 값을 summarise 
#1. 가장 많이 팔리는 상품은 뭔가?

desc(sort(-table(s$sellproduct)))
#가장 많이 팔리는 상품은 가스큐팩 1.6리터(P) 이다
#저 상품이 시간당 몇개씩 팔리는지

head(s$Date,10)
#stringr 공부 해야하는데... 좀 있다 하고 오늘은 일단 찢자

s$time <- str_split_fixed(s$Date," ",2)[,2]
s$hour <- str_split_fixed(s$time,":",3)[,1]
colnames(s)
head(s,5)

s2 <- s %>% filter(sellproduct == "가스큐팩 1.6리터(P)")
freq <- table(s2$hour)
freq

#tbl_dt가 뭐지?
s_copy <- s
s_copy <- tbl_df(s_copy)
s3 <- s_copy %>% filter(sellproduct == "가스큐팩 1.6리터(P)")
freq <- table(s3$hour)
freq

remove(s_copy) 

####책에 오류가 너무 많았다..## pdf보고 내일 다시 할 예ㅈ

