#####연습장#####
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(Hmisc)
library(MASS)
library(psych)


install.packages("installr")
library(installr)
check.for.updates.R()

install.R()
version

car <- Cars93
dim(car)
colnames(car)
headtail(car,5,5)

#고속도로와 시내주행 연비 변수를 하나로 합침 mpg 이름으로
temp_car <- car %>% dplyr :: mutate(mpgs = (car$MPG.city + car$MPG.highway)/2)


#연관 상관분석 부터
str(temp_car)
cor(temp_car$mpgs,temp_car$Weight)

system.time(plot(car)) #전체 그림 그리는데 시간이 오래걸린다

Hmisc::describe(temp_car)
summary(temp_car)

attach(temp_car)
plot(mpgs,Horsepower)
plot(Cylinders)

ggplot(data = temp_car) +
  geom_bar(aes(x=mpgs),color = Cylinders)

temp_car %>% dplyr::select(Cylinders) %>% distinct()

temp_car_cy <- temp_car %>% filter(Cylinders == 4 | Cylinders == 6 | Cylinders == 8)
str(temp_car_cy)

temp_car_cy %>% cor(Price,mpgs, use = "complete.obs")

cor(temp_car_cy$Price,temp_car_cy$mpgs)

colnames(temp_car_cy)

car_lm <- lm(Price ~ Type + mpgs + Horsepower + AirBags, data = temp_car_cy)

summary(car_lm)
