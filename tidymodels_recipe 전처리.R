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

flight %>% skimr ::skim(dest,carrier)

#로지스틱 모델을 생성할때, dest와 carrier의 함수는 더미 변수로 사용될 예정이며 model.matrix로변경해여한다.
#더미 변수가 너무 많아지나, 중요하게 자주 사용될 변수가 아니므로 모델이 복잡해질수 있다.
#recipe 함수에서 처리하는 방법 설명
#하우스 데이터의 우편번호와 같은 역활을 할수 있다.
#기존의 테스트 트레인 데이터를 나누는 것도 간단하게 rsample 패키지로 가능하다
?rsample


set.seed(555) #홈페이지와 동일하게 랜덤 난수 설정
data_split <- initial_split(flight,prop = 3/4) #3/4을 training으로 입력

train_data <- training(data_split) #트레인데이터
test_data <- testing(data_split)  #테스트 데이터




