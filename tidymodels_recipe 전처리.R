#Tidymodels.org 의 설명글 따라 하기
#2.Preprocess your data with recipes
#recipes

library(tidymodels) 
library(tidyverse)
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


flight_rec <- recipe(arr_delay ~. , data = train_data)
#위에 사용한 함수는 ~표시 왼쪽은 y, 오른쪽은 x이며 .의 의미는 전체 변수를 의미한다
#앞서 이야기 했던 flight 와 time_hour는 예측에는 사용하지 않으나 
#특정값을 식별하기 위해서 남겨둔 데이터 이며 이는 예측에 사용되지 않는다
#예측에서 제외하기 위해서 는 해당 변수를 제외하고 x 자리에 입력하면 되나
#recipe 함수의 role 함수로 업데이트 한다

flight_rec <- 
  recipe(arr_delay ~ . , data = train_data) %>%
  update_role(flight, time_hour, new_role = "ID") #두 변수를 ID로 업데이트 

summary(flight_rec)
#확인하면 role에 각 컬럼의 역활이 지정되어 있다
#ID, predictor

flight %>% 
  dplyr::distinct(date) %>% 
  mutate(numeric_date = as.numeric(date))

#날짜를 숫자(numeric)으로 변경하면 엑셀처럼 숫자가 나온다
#날짜를 숫자로 변경하면 선형회귀로 결과 도출할때 선형 추세의 형태를 가지게 되므로 이점이 될수 있다
#하지만 날짜 자체에서 의미있는 변수를 추출할수도 있다
#요일이나 달, 그리고 날짜가 공휴일인지 등의 의미를 추출할수 잇다.

#let's do all three of these by adding to our recipe
#preprocess for date column with recipe 

flight_rec <- 
  recipe(arr_delay ~ . , data = train_data ) %>%  #y와 x의 컬럼 지정 
  update_role(flight, time_hour, new_role = "ID") %>%  #필요없는 변수를 x에서 제외
  step_date(date, features = c("dow","month")) %>%  #월과 요일 컬럼 생성
  #날짜 기준으로 timeDate의 패키지에서 미국 휴일에 포함되는지에 대한 이진 컬럼생성
  step_holiday(date, holidays = timeDate ::listHolidays("US")) %>%  
  step_rm(date) #모델에서 날짜 변수를 삭제

#turn our attention to the variable types of our predictors
#in logistic regression model need numeric preditors, not factor predictors.

#recipe 함수로 전처리 완료 추후 업데이트 예정 - 작업은 R studio에서 함.

