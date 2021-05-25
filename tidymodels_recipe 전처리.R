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

#turn our attention to the variable types of our predictors
#in logistic regression model need numeric preditors, not factor predictors.
#factor 요인인 "dest"와 "origin"의 경우 정상적인 절차로는 여러 컬럼의 이진더미변수로 생성
#recie 함수에 factor를 더미변수로 생성하지 않게 알려줘여 하는데
#많은 모델에는 더미변수로 전화된 숫자 변수가 많이 필요 없고, 항상 선호되는건 아니다

#recipe에서는 더미변수 변화보다 더 잘 작동하는 모델링 외부 목적으로 사용 가능


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(),-all_outcomes())
#개별 변수에 단계적으로 접근하지 않고 여러변수를 한번에 적용했다
#all_nominal은 요인, 또는 문자인 모든 변수 선택
#all_outcomes는 모든 결과 변수를 제거
#둘을 한번에 적용하면 결과변수가 아닌 모든 요인 또는 문자열에 대한 더미변수 생성
#이 식에서는 "origin", "dest", and "carrier", date_dow" ,"date_month" 변수 포함

#recipe에서는 각 변수별로 각각 함수를 적용할 필요가 없다.
#이말은 recipe 함수가 각 변의 타입과 role을 알고있어서
#알아서 변수를 선택한다.

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors())
#학습데이터가 단일값을 가질 경우, 열에서 제거하므로 
#step_dummy단계이후 recipe에서 추가 가능하다.

#recipe 전처리 끝

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")


flight_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)
flight_wflow

#모델 마다 다른 레시피가 필요한 경우, workflow 로 모델과 레시피로 적합시킨다


flights_fit <- 
  flight_wflow %>% 
  fit(data = train_data)

#fit 함수로 위에서 설정한 workflow 함수에 train data로 학습시킨다
#적합시킨 데이터로 확인하기

flights_fit %>%
  pull_workflow_fit() %>% 
  tidy()

#변수별 추정치와 통계값 p.value 확인 가능


#처음으로 돌아가서, 우리는 30분 이상 늦은 비행기기를 예측하는 것이 목표
#훈련된 workflow로 예측하기
#1. lr_mod로 모델을 준비했고
#2. flights_rec로 데이터 전처리 및 요인 전처리
#3. flights_wflow 로 모델과 전처리를 적합
#4. fit 함수로 훈련 시킴

predict(flights_fit, test_data)
#마지막으로 테스트 데이터로 예측
#outcome로 지정된 컬럼이 factor로 되어 있기 때문에 predict로 예측된 결과는
#처음 지정한 "late", "on_time" 두가지 값으로 표현된다.
#확률변수와 함께 지정하기 위해서 

flights_pred <- 
  predict(flights_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred  

#어떻게 모델의 선능을 평가할까?
#roc_curve, roc_auc 로 사용

flights_pred %>% 
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()

flights_pred %>% 
  roc_auc(truth = arr_delay, .pred_late)
#0.765 값이 나왔다, 나쁘지 않다.

#recipe로 전처리하고 이전에 배운 모델로 확인 완료




#recipe 함수로 전처리 완료 추후 업데이트 예정 - 작업은 R studio에서 함.
#recipe 함수로 완료 
#3편 차례
#3편완료, 4편 시작 

#4편 거의 완료 및 rmarkdown 학습

#포트폴리오 전략, 1.선형 단항, 다항회귀, 2. 로지스틱, 분류분석 3. open API. - 결과 만들기
#위 3개 이상의 포트폴리오 R markdown으로 pdf 파일 생성.

#복사해서 올ㄹ




