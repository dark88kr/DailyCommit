#책실습하기 
#R까기2
#example 2.대장암 환자 자료 분석
#보건의료빅데이터개방시스템에서 자료 다운 가능
library(tidyverse)
DF <-read.csv("example_cancer.csv")
str(DF)
#대장암데이터, 성별,나이,키,몸무게,수술날짜,대장암단계,데이터호스피탈리즘?,병코드? 가 있음
DF %>% select(cancerStaging) %>% table()
#1기 4073, 2기 5237, 3기 6571, 4기 2169, 기록없음 260

#나이에 따른 환자수 보기
#DegreeOfAge에 나이별 분포로 나누기위해 table와 cut함수 사용하고 1:11*10으로 나눔
DegreeOfAge<-table(cut(DF$age, breaks = (1:11)*10))
rownames(DegreeOfAge) <-c("10s","20s","30s","40s","50s","60s","70s","80s","90s","100s")
DegreeOfAge
#그래프그리기
library(ggplot2)
require(ggthemes)
install.packages("ggthemes")
ggplot(data = DF, aes(x=age)) + geom_freqpoly(binwidth=10,size=1.4,colour="orange") + theme_wsj()

#ggplot 페키지 사용시 굳이 도수분포표 안만들어 된다는 교훈이다..


#example 3.전국 키피숍 폐업/영업 상황 살펴보기
#공공데이터포털(http://data.go.kr/)에서 다운가능
#data.table 패키지는 data.frame과 비슷하지만 속도가 더 빠름
install.packages("data.table")
library(data.table)
library(tidyverse)
library(ggplot2)
#data.table의 fread 함수는 한글이 많아 데이터 가 큰 경우 사용하기 좋ㅇ
DF2 <-fread("example_coffee.csv", header = T, stringsAsFactors = T, data.table = F)

#데이터 살펴보기
str(DF2)
glimpse(DF2)
#data.frame에 tbl_df를 적용하여 tbl_df 속성을 가지게 되고 스크린에 표시될 정도만 행과 열 출력
DF2 <-tbl_df(DF2)
str(DF2)

#불필요한 변수 제외 하기

DF2_mo <-subset(DF2, select = c(-adress,-adressBystreet,-dateOfclosure, -startdateOfcessation,
                                -duedateOfcessation, -dateOfreOpen, -zip))
str(DF2_mo)




