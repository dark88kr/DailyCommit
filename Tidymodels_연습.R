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
                    ymax = .pred_upper),쉼
                width = .2) + 
  labs(y = "urchin size")

#하나의 모델로 피팅시켜둠
#210322 건강 문제로 하루 ㅅ
#210323 허리통증이 완화되지 않아 하루 ㅅ
#210324 허리통증으로 하루 쉼


쉼