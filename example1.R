#책실습하기 
#R까기2
#example 1. 20만건 넘는 관찰치 도수분포표 만들기

install.packages("hflights")
library(hflights)
library(tidyverse)
summary(hflights)
str(hflights)

#목적지 확인 - 목적지는 Dest column 에 있음

dest<-hflights$Dest
str(dest)
table(dest)
S

