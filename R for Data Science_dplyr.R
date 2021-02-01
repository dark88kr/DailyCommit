############################R for Data Science 책 실습 
######Chapter 3 Data Transformation with dplyr

library(nycflights13)
library(tidyverse)
flights
str(flights)
head(flights)


###dplyr패키지 중, filter, arrange, select, mutate, summarize and group_by
###6개의 함수의 사용법 익힌다.

##filter 함수, 특정 row 값을 찾는다
##filter 함수는 >,>=,==,<=,<, =="같다" ,!="같지 않다", &"and", | "or", !"부정"
##등의 조건값을 함께 사용한다

#11월과 12월에 비행기록이 있는 모든 비행기 찾기
nov_dec <- filter(flights, month == 11 | month == 12)

#실제 month에 11월 과 12월만 있는지 확인
nov_dec %>% select(month) %>% distinct()

#11월과 12월에 비행기록이 있는 모든 비행기 찾기, 또 다른 코드
#month의 11과 12가 포함된 row를 nov_dec_1에 넣어라
nov_dec_1 <- filter(flights, month %in% c(11,12))

#실제 month에 11월 과 12월만 있는지 확인
nov_dec_1 %>% select(month) %>% distinct()

#둘다 동일한 코드
delay_t <- filter(flights, !(arr_delay>120 | dep_delay>120))
delay_t_1 <- filter(flights,arr_delay <= 120, dep_delay<= 120)

str(flights)

ggplot(data = flights, mapping =aes(x=dest)) +
  geom_bar()

#그림한번 그려 봤는데 너무 많다....

filter(flights, dest == "IAH")
#목적지가 "IAH" 인곳 찾기

range(flights$distance)
#거리가 가장 가까운곳과 가장 먼 곳을 확인

dis_min <- filter(flights,distance == 17 | distance == 4983)
view(dis_min)
#가장 먼곳과 가장 가까운곳 확인


###arrange 함수로 정렬하기
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

####select는 다음에 p.51까지 함

