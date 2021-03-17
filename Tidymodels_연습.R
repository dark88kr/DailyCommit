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


