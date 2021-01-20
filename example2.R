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
#최초의 커피숍 찾기 , yearofstart column 이용
#range는 수치형 변수의 최소, 최대값 나타내는데 na.rm = T는결측치를 제외하고 계산한다
range(DF2_mo$yearOfStart, na.rm = T)
subset(DF2_mo,subset = (DF2_mo$yearOfStart == 1964))
#가장 오래된 거피숍은 망했다.... 살아 있는 가장 오랜된 커피숍 찾아봅시다
DF2_MO_live <- DF2_mo %>% filter(stateOfbusiness == "운영중")
DF2_MO_live
range(DF2_MO_live$yearOfStart, na.rm = T)
DF2_mo %>% filter(yearOfStart == "1967" & stateOfbusiness == "운영중")
#현재 운영중인 가장 오래된 커피숍은 1967년 시작한 왕관커피숍, 학커피숍이다


#이번에는 해마다 오픈하는 커피숍 숫자 확인 하기
#yearofstart column 사용하기

table(DF2_mo$yearOfStart)
#많다, 그림그려야 것다
#qplot는 ggplot2의 패키지 함수로 간단하게 그릴때 사용하는 함수
qplot(DF2_mo$yearOfStart, data = DF2_mo, geom = "bar")

#자세하게 영업상태 및 연도에 따른 분활표
FREQ <- table(DF2_mo$stateOfbusiness, DF2_mo$yearOfStart)
FREQ

#그래프에서 보듯이 2000년 이후 기록만 검토합시다
#which는 값이 벡터중 어디에 있는 값인지 알려준다
which(colnames(FREQ)== "2000")
#2000년은 33번째부터 있다, 그럼 마지막은?
which.max(colnames(FREQ))
#마지막은 48이다 , 그럼 33번부터 48까지 저장하면 2000년 이후 기록만 저장 가능하다

str(FREQ)
head(FREQ)
is.list(FREQ)
#FREQ <- FREQ[0,C(33:48)] 대문자 c 사용, row에 0 삽입하여 행 없음...
#82번 코드는 Error in C(33:48) : object not interpretable as a factor 처럼 에러발생
#사유는 대문자 C를 써서 그럼...... row 자리에 0도 지움...
FREQ <- FREQ[,c(33:48)]
#2000년도 이후 기록만 저장됨
FREQ
str(FREQ)
#테이블로 저장됨
#prop.table로 100분율로 저장하되 margin 2를 사용하여 각 column당 비교 가능
PFREQ<-prop.table(FREQ,margin = 2)
PFREQ


NEWDF<-data.frame(colnames(FREQ),FREQ[1,],FREQ[2,],PFREQ[1,],PFREQ[2,])
NEWDF
str(NEWDF)

rownames(NEWDF)<-NULL #년도가 변수로 되어 있어서 이름을 삭제
colnames(NEWDF) <- c("Year","Open","Close","POpen","PClose")

library(ggplot2)
library(ggthemes)

#폐업은 파란색 점크기 1, 창업은 빨간색 점크기 6
ggplot(NEWDF,aes(x=factor(Year), y=Close, group=1)) + geom_line(colour="steelblue1", size=1) +
geom_point(colour="steelblue", size=3) + geom_line(aes(y=Open),colour = "tomato2",size=1) +
geom_point(aes(y=Open),colour="red",size=6) + theme_bw()
  

#2010년부터 폐업보다 창업이 많아지고 있다.

