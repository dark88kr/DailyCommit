#Tidymodels.org 의 설명글 따라 하기
#2.Preprocess your data with recipes
#recipes

library(tidymodels) 
library(nycflights13)   
library(skimr)   

#뉴욕 비행 데이터로 어떤 비행기가 30분 늦게 도착하는지 예측해보자

glimpse(flights)

flights %>% colnames()

weather

set.seed(123)
flight <- flights %>% mutate(
  arr_delay = ifelse(arr_delay >=30 , "late", "on_time"),
  arr_delay = factor(arr_delay), 
  date = as.Date(time_hour)
) %>% 
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  select(dep_time,flight,origin,dest,arr_time,distance,carrier,date,arr_delay,time_hour) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor) #character의 변수를 factor로 변환  

glimpse(flight)
str(flight)
colnames(flight)


flight %>% 
  count(arr_delay) %>%  #각 범주가 몇개인지 범주명과 n을 표시한다. 
  mutate(prop = n/sum(n)) #prop라는 변수를 생성하고 전체 n에서 각 n을 나눠서 %로 표시 할수 있게 한다.
#전체의 16%가 늦었다.



