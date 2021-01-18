#책실습하기 
#R까기2
#example 1. 20만건 넘는 관찰치 도수분포표 만들기

install.packages("hflights")
library(hflights)
library(tidyverse)
summary(hflights)
str(hflights)

#목적지 확인 - 목적지는 Dest column 에 있음

dest<-table(dest)
str(dest)
range(dest)
dest[dest==1]
dest[dest==9820]
famous.dest <- dest[dest > 6000]
famous.dest

barplot(famous.dest)
######Example 1 종료########
#버벅거림 - 앞서 table 함수로 변수 저장한걸 보지 못하고 range 함수 의 결과값이 나오지 않아서 버버