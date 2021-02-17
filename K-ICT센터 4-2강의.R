##########k-ICT 빅데이터 센터 R 강의 따라하기

#####4강 분석과정 모델링과 데이터탐색
###4-2. 데이터셋 로딩 및 데이터기본 탐색

install.packages("data.table")
install.packages("psych")
install.packages("Hmisc")
library(tidyverse)
library(data.table)
library(psych)
library(Hmisc)

my <- fread(input = 'tour.csv',
            header = T, sep = ',',
            stringsAsFactors = F,
            strip.white = T,
            na.strings = c('.','?','NA'))
#데이터 확인
str(my)
headtail(my,5,5)
dim(my) #차원 확인, 1차원의 행과 열
glimpse(my) #str과 비슷함
summary(my)
Hmisc::describe(my) #summary보다 보기 좋은 기초 통계

round(cor(my, use = 'pairwise.complete.obs'),2)
round(cor(my, use = 'complete.obs'),2) # 이건 왜 NaN로 표기 되는가?

temp_my_edu_expence <- my %>% select(edu,expense)
Hmisc::describe(temp_my_edu_expence)

cor(temp_my_edu_expence, use = 'complete.obs') #교육수준과 금액의 관계 없다
cor(my$age,my$gender, method = "spearman", use = 'complete.obs') #성별과 나이대의 관계 없음

#범주형 데이터 끼리 연관성 분석을 위해서 "spearman" 넣었다

plot(my$gender,my$age)








