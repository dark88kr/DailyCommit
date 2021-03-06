#책실습하기 
#R까기2
#example 5.전국 인구조사 자료 정리하기(전처리 연습)
#필요한 패키지 부착하기
library(tidyverse)
install.packages("stringr")
library(stringr)
library(ggplot2)
library(ggthemes)

df<-read.csv("example_population.csv",stringsAsFactors = F)
str(df)

#stringsAsFactors = F 는 인구수에 , 가 들어가는데 이거때문에 해당 변수가 factor로 인식되기 때문
#이번 예제에서는 factor보다는 문자열이 다루기 편함.

head(df)
#city의 값이 "서울특별시 종로구 (1111000000)" 이렇게 되어 있어 
#'서울특별시' , '종로구'로 생성하고 숫자는 삭제 예정

#문자열을 측정 문자를 기준으로 나누는 패키지는 stringr
#'(' 기준으로 나눌 예정, str_split_fixed(문자열, 분리할 기준문자, 분리할 개수)
#R 기본 함수strsplit도 있지만 리스트로 변환하기 때문에 패키지 함수 사용
#( 괄호 기준의 정규식 표현은 '\\(' 이다.

city<-str_split_fixed(df[,1],"\\(",2)
head(city)
newcity<-str_split_fixed(city[,1]," ",2)
colnames(newcity) <-c("provinces","city")
head(newcity)

df<-data.frame(newcity,df[,c(2:7)])
head(df)
#city변수 값을 변경하고 한글만 때서 시와 구로 나누어서 기존 데이터와 합침

df[df==" "]<-NA
#새롭게 생성한 city 함수의 빈칸은 NA 값으로 치환
df<-tbl_df(df)
df<-df[complete.cases(df),]
#NA값 삭제
glimpse(df)

#for i 구문으로 , 삭제하고 숫자열 만드는건 다음에

#for i 구문으로 변수를 변환해준다


for(i in 3:8){
  df[,i] <-sapply(df[,i], function(x) gsub(",","",x))
  df[,i] <-as.numeric(df[,i])
}
str(df)

#sapply 함수로 3:8번까지 컬럼 선택 후, gsub함수로 ,를 빈칸으로 치환했다
#수치형 변경 및 콤마 제거하는 방법
#전처리 완료


#경기도 인구수 추출
#tapply 함수 사용 , tapply (적용변수, 그룹지을 변수, 적용할 함수)
pro_popul<-tapply(df$Population,df$provinces,sum)
pro_popul
tbl_df(pro_popul)
#도 구역별로 인구수를 더하라는 함수

gra<-ggplot(df,aes(x=df$provinces,y=df$Population, fill=df$provinces)) +
  geom_bar(stat="identity") + 
  theme_wsj()
gra

#그래프 그리기까지 완성 
