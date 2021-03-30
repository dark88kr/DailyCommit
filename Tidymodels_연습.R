#Tidymodels.org 의 설명글 따라 하기
#receipe 패키지 공부하려다가 여기서 부터 시작함
install.packages("tidymodels")
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
install.packages("broom.mixed") # for converting bayesian models to tidy tibbles
install.packages("dotwhisker")  # for visualizing regression results
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

urchins <-  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
# Change the names to be a little more verbose
setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

urchins

ggplot(urchins,
       aes(x=initial_volume,
           y=width,
           group = food_regime,
           col = food_regime)) +
  geom_point()+
  geom_smooth(method = lm, se=F)
  
linear_reg() %>% set_engine("lm")
#> Linear Regression Model Specification (regression)

lm_mod <- linear_reg() %>% set_engine("lm") #lm 모형을 설정하고 lm_mod에 저장해 두었다

lm_fit <- lm_mod %>% fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit

#오늘은 배운거 노트에 정리하기 210318
summary(lm_fit) #summary함수가 각 x의 추정치와 기울기를 제공하지만, 결과를 다루기 힘든 모양으로 제공한다
tidy(lm_fit) #요약결과를 예측가능하고 유용한 형식으로 보여주는 tidy함수 사용 한다

tidy(lm_fit) %>% 
  dwplot(dot_args = list(size=3,color='blue'),
         whisker_args = list(color = 'red'),
         vline = geom_vline(xintercept = 0,colour = 'grey50', linetype=1))


new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points) #predict로 평균값 예측
mean_pred


conf_int_pred <- predict(lm_fit, new_data = new_points,type = "conf_int") #predict함수로 신뢰구간 예측
conf_int_pred

plot_data <- 
  new_points %>%             #새로운 데이터에
  bind_cols(mean_pred) %>%   #평균예측값과
  bind_cols(conf_int_pred)   #새로운 데이터의 신뢰구간 예측값을 합친다.
plot_data

ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) +  labs(y = "urchin size")

#하나의 모델로 피팅시켜둠
#210322 건강 문제로 하루 ㅅ
#210323 허리통증이 완화되지 않아 하루 ㅅ
#210324 허리통증으로 하루 쉼

#선형 모델 이외의 베이지안 방식으로 모델 피팅을 원함
#베이지안 모델 피팅을 위해서 사전 확률도 이력 해야함

install.packages("rstanarm")
prior_dist <- rstanarm::student_t(df = 1)



#Tidyverse - 기본 함수 익히기
#datapasta로 엑셀파일 바로 붙여 넣기 할수 있음

#총 8개 패키지로 묶여 있음 - tidydate를 지향함
#ggplot2 - 시각화, dplyr, tidyr- 전처리형, readr - 읽어오기, tibble - data frame 대신 사용 한다
#forcats - factor 데이터를 다룰때 사용, purrr- 고급 사용시 사용, 나중에 다시 설명함.


#tools -> install packages에 패키지 이름에 "devtools" 입력

devtools::install_github("allisonhorst/palmerpenguins") #설치
library(palmerpenguins) #iris데이터 대신 사용 

remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)
data(package = 'palmerpenguins')
head(penguins)

pen <- penguins

#column 설명 : flipper = 날개, bill = 주둥이 (dbl : 실수, int:정수)

#tidyverse의 5가지 함수 
#filter
pen %>% filter(species == "Chinstrap")
pen %>% filter(species == "Chinstrap", island == "Dream") #친스트렙 중 드림 아일랜드에 있는 아이만 "and" 값

# %in% operater ->  왼쪽의 값이 오른쪽에 포함되면 "T"
pen %>% filter(species %in% c("Chinstrap", "Adelie"), island == "Dream") #종이 아델리 or 친스트랩 중 드림 아일랜드에 사는 아이만

#select column 선정함
pen %>% select(-species) %>% head() # speices 빼고 나머지 컬럼의 헤더만 보여줘
pen %>% select(end_with("mm")) %>% names() # "mm"으로 끝나는 컬럼명이 포함된 전체 컬럼을 선택하여 선택된 컬럼의 이름을 보여줘
pen %>% select(island, bill_length_mm, everything())%>% head() #아일랜드와 주둥이길이 컬럼을 먼저 보여주고 나머지 전체 컬럼은 뒤로 붙여줘, 열이 뒤 바뀌는 효과가 있다


#mutate 새로운 컬럼을 만듦

pen %>% select(bill_length_mm, bill_depth_mm) %>% mutate(bill_total = bill_length_mm + bill_depth_mm) 
#두개의 컬럼 과 두개의 컬럼 값을 더한 값을 bill_total로 저장하여 새로우 컬럼을 만들어 달라

#arrange 정렬

#summarize (새로만들 컬럼  = 함수(데이터, na.rm = T) na.rm은 값이 없는 것은 생략하고 계산함
#주로 group by 함수와 사용 함


#across 함수  - 여러 컬럼에 하나의 함수를 적용할때

pen %>% group_by(species) %>% summarize(bill_length_mean = mean(bill_length), bill_depth_mean = mean(bill_depth_mm),filper_mean=mean(filpper_length_mm))

pen %>% group_by(species) %>% summarize(across(bill_length_mm : filpper_length_mm,mean, na.rm = T)) 
#mean함수를 bill_length_mm : filpper_length_mm 까지 적용


#ggplot2 의 레이어 확인 하기
#ggplot는 %>%가 안먹는다....  대신 +로 대체한다, 한층 씩 쌓아서 그림을 그리는게 방식이다

#첫번째 레이어  - data

ggplot(data = pen) # 아무것도 안나온다 백지, 도화지

#두번째 레이어 - aesthetice
#x축과 y축을 설정하는 함수
ggplot(data = pen, aes(x=bill_length_mm,y=bill_depth_mm)) # 도화지에 x축과 y축이 맵핑됨.
#aes는 x,y 축 설정가능, 투명도 = alpha, 색 = color, fill(포인트로 채운다), 모양과 사이즈  = shape,size

#세번째 레이어 - geom_layer
#geom에서도 aes 사용 가능하다.
geom_point(aes(color = as_factor(species), size = body_mass_g, alpha = 0.7))
#geom 에서 point로 그림을 그릴건데 색은 종별로 색칠하고, point의 사이즈는 몸무게로 표시하고 점의 투명도는 0.7
#투명도를 낮추면 겹쳐진걸 확인 가능하다


#scales - aes을 조정하는 함수
#사용법 scale_<aes>_<type>

#scales_y_continuous(change lable, breaks(limits, etc) 
#조정한다_y의 aes를 _ y의 type는 연속형이다 (첫번째 입력은 레이블값, 두번째는 breaks 또는 limits 사용)

#scales_color_brewer(palette = "set1", lables = c("my","me","mind") 
#조정한다_색 aes를 _ 색의 타입은 그린다 ( 색의 조합을 "set1"로, color에 사용된 변수의 lable은 "my","me","mind"사용한다


#facets 한면 - 그려진 그래프를 변수별로 나눌때 사용

#coord_fixed() 사용하면 x축과 y축의 비율을 맞춰준다

table(pen$species)
pen %>% select(species) %>% table()
pen %>% group_by(islands) %>% summarise(table(species))

summary(pen)
pen <- tbl_df(pen)
glimpse(pen)

install.packages("skimr")
library(skimr)
library(Hmisc)
Hmisc::describe(pen)


ggplot(data=pen,aes(x=bill_length_mm,y=bill_depth_mm))+
  geom_point(color=species)
  