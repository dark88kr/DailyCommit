######책실습하기 
######R까기2
###데이터 전처리 하기 p415
###데이터 나누고 처리하기 - 기본함수

library(tidyverse)
df <- read.csv("example_studentlist.csv",fileEncoding = "EUC-KR")
df
lapply(df[,7:8],mean)

#이 데이터의 혈액형 별로 키의 평균 구하고 싶다.

aggregate(df$height~df$bloodtype,df,mean) #data.frame -> data.frame

tapply(df$height,df$bloodtype,mean) #vector, matrixm array 가능

by(df$height,df$bloodtype,mean) #결과를 list로 반영

lapply(split(df$height,df$bloodtype),mean) #list

#각 성별의 혈액형 별 키의 평균
aggregate(height ~ bloodtype + sex, df, mean) #aggregate (y~ x1+x2, data, fun)

#혈액형 별로 키와 몸무게의 평균
aggregate(cbind(height,weight)~bloodtype,df,mean)

#성별 키와 몸무게의 회귀분석하여 리스트로 출력

by(df,df["sex"],function(x) lm(weight~height, data = x))

#by는 list로 데이터를 변환 하지만 입력된 데이터를 그대로 사용해서 data.frame 객체 다룰때 유용


###데이터 나누고 처리하기 - dplyr 패키지
#dplyr는 데이터프레임을 다루기 위해 개발된 패키지
#우리가 다루는 데이터 중에서도 데이터프레임형태가 많기 때문에 유용할것이다.
#중요## dplyr패키지를 사용하기 위해서는 기존 데이터 프레임이 아니라 dplyr용으로 데이터프레임 만들어야 한다

df <- read.csv("example_studentlist2.csv", fileEncoding = "EUC-KR")
df <- tbl_df(df)
df
#새로운 tbl_df와 tbl 의 클래스가 추가됨
#select, filter, mutate, group_by, summarise 함수가 주로 사용됨

#df에서 sex, height, weight 변수 선택하고 height는 170 이상인 값만 보여달라
df %>% select(sex,height,weight) %>% filter(height >= 170)

#키가 170 이상의 남자의 자료만 보여달라
df %>% filter(sex == "남자" & height >= 170 )

#키가 170 이상의 남자의 자료를 키순서대로 정렬하라
df %>% filter(sex == "남자" & height >= 170 ) %>% arrange(desc(height))


#키가 170 이상의 남자의 자료를 혈액형과 키순서대로 정렬하라
df %>% filter(sex == "남자" & height >= 170 ) %>% arrange(bloodtype,desc(height))

#BMI 지수 추가
df %>% mutate(BMI = weight/(height/100)^2)



