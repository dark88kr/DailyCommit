#Tidymodels 첫번째 Build Model
#tidymodels 로 통계

library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

urchins <-  read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  setNames(c("food_regime", "initial_volume", "width"))

summary(urchins)
str(urchins)

urchins %>% mutate(food_regime = factor(food_regime, levels = c("Initial","Low","Hight")))
#food_regime를 factor로 변경한 후 각 범주의 레벨을 정해줌
#y값은 width 로 연속형 함수, x의 두값은 하나는 범주형 하나는 연속형
#각변수별 기울기와 절편이 존재하는 선형모형으로 예측

#첫번째는 그림 그리기


ggplot(data = urchins, aes(x= initial_volume,
                           y=width,
                           group = food_regime,
                           color = food_regime))+
  geom_point()+
  geom_smooth(method = lm, se= F)

#종속변수의 속성이 연속형, 선형 모델로 적합시킴
lm_mod <- linear_reg() %>% set_engine("lm")
lm_fit <- lm_mod %>% fit(width ~ initial_volume*food_regime, data = urchins)

summary(lm_fit) #tibble모형으로 summary 함수로 결과를 호출하면 보기 어려움
tidy(lm_fit)

tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "blue"),
        whisker_args = list(color = "black"),
        vline = geom_vline(xintercept = 0, color = "gray50", linetype = 2))

new_data <- expand.grid(initial_volume= 20,
                        food_regime = c("Initial", "Low", "High")) #expand.grid는 가능한 모든벡터의 조합생성
new_data

mean_pred <- predict(lm_fit, new_data = new_data) #예측되는 각 데이터의 예측 중간값
mean_pred

conf_int_pred <- predict(lm_fit, new_data = new_data, type = "conf_int") #예측되는 각 데이터의 신뢰구간 끝 값
conf_int_pred

plot_data <- new_data %>% bind_cols(mean_pred) %>% bind_cols(conf_int_pred)
plot_data

ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) +  labs(y = "urchin size")

#베이지안 모델로 예측
#베이지안 모델 사용시 사전 분포를 지정해줘야한다.
#데이터 탐색 전에 분포를 지정해야 하기 때문에 보수적으로 가우시안분포를 사용한다.

prior_dist <- rstanarm::student_t(df=1) 

#베이지안 모델 사용시 모델피팅에 랜덤으로 생성된 숫자가 포함 되므로 set.seed로 결과치를 일정하게 한다

set.seed(123)
bayes_mod <- linear_reg() %>% set_engine("stan",
                                         prior_intercept = prior_dist,
                                         prior = prior_dist)
bayes_fit <- bayes_mod %>% fit(width ~ initial_volume*food_regime, data = urchins)
print(bayes_fit)
tidy(bayes_fit)
