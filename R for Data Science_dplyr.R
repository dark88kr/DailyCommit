############################R for Data Science 책 실습 
######Chapter 3 Data Transformation with dplyr

library(nycflights13)
library(tidyverse)
flights
str(flights)
head(flights)


###dplyr패키지 중, filter, arrange, select, mutate, summarize and group_by
###6개의 함수의 사용법 익힌다.

##1. filter 함수, 특정 row 값을 찾는다
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


###2. arrange 함수로 정렬하기
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

####select는 다음에 p.51까지 함

###3. select는 컬럼 선택

select(flights, year, month, day)
#3개의 컬럼만 선택

select(flights, year,month:day)
#year 컬럼과 year부터 day까지의 모든 컬럼 선택

select(flights,-(year:day))
#year부터 day까지의 컬럼을 제외한 컬럼 선택


select(flights, time_hour, air_time,everything())
#time_hour과 air_time 컬럼부터 보여주되 나머지도 데이터 프레임에 포함할것



###4. mutate는 신규 컬럼을 생성하는 함수

flights_sml <- select(flights, year:day, ends_with("delay"),
                      distance, air_time)
flights_sml
#year부터 day까지 컬럼과 delay가 포함된 모든 컬럼 그리고 
#distance와 air_time 컬럼까지 선택하여 flights_sml 데이터프레임으로 저장


mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time *60)
#gain은 arr_delay에서 dep_delay 뺀값으로 저장하고
#speed는 distance를 air_time으로 나눠서 60곱한 값으로 저장하여
#flight_sml에 붙여라

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time *60,
       hours = air_time / 60,
       gain_per_hour = gain/hours)
#방금 생성한 컬럼도 바로 다음에 참고하는 컬럼으로 사용 가능하다


#새롭게 생성한 컬럼을 기존 데이터 프레임이 아닌 새로운 데이터로 저장
###4-1. transmute 함수 사용

transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hours = gain / hours)

#여러가지 수학 기호 사용 가능


###5. summarize 종합계산 및 집계하는 함수

summarise(flights, delay = mean(dep_delay, na.rm = T))
#flights의 dep_delay의 평균 값을 공백을 제거하고 난 뒤 구하고 delay라 명명

#group_by와 pipe로 함께 사용

delays <- flights %>% group_by(year,month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- flights %>% group_by(year,month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm=T))
#na.rm은 계산시 공백은 제외하고 계산한다

not_cancelled <- flights %>% filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled %>% 
  group_by(year,month,day) %>%
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay=mean(arr_delay))


ggplot(data = delays, mapping = aes(x=delay)) +
  geom_freqpoly(binwidth =10)
####p63까지



