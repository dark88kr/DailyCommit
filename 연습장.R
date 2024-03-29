#####연습장#####
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
install.packages("Hmisc")
library(Hmisc)
library(MASS)
library(psych)
<<<<<<< HEAD


install.packages("installr")
library(installr)
check.for.updates.R()

install.R()
version

=======
library(data.table)
>>>>>>> 59c2020c61ee7f03e3eeeed1e870a614a7877d51
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
<<<<<<< HEAD

cor(temp_car_cy$Price,temp_car_cy$mpgs)

colnames(temp_car_cy)

car_lm <- lm(Price ~ Type + mpgs + Horsepower + AirBags, data = temp_car_cy)
=======

cor(temp_car_cy$Price,temp_car_cy$mpgs)

colnames(temp_car_cy)

car_lm <- lm(Price ~ Type + mpgs + Horsepower + AirBags, data = temp_car_cy)

summary(car_lm)

#### 후원기록 불러오기 ##

do <- fread(input = "HA_Donation count.csv",
            header = T,
            sep = ',',
            na.strings = c('NA'),
            strip.white = T)
            

view(do)
describe(do)
do %>% filter(YOB == "2072") #생년월일이 2072년생이 없기 때문에 로 처리
is.na(do$YOB) # rlw
do[280,]
do$YOB <- ifelse(do$YOB > 2021,NaN,do$YOB)
describe(do$YOB)


library(palmaerpenguins)
penguins
library(tidyverse)

pen <- penguins
pen %>% split(.$species) %>% 
  map(~lm(bill_length_mm~bill_depth_mm,data = .))
  

nested_df <- pen %>%
  group_by(species) %>% 
  nest() %>% ungroup(species)

nested_df %>% 
  mutate(model = map(data, ~lm(bill_length_mm~bill_depth_mm, data = pen)))


library(tidymodels)
library(tidyverse)
str(mtcars)
mcar<- mutate(am = ifelse(am,"au","mu"))
?mtcars
glimpse(mcar)

mtcars$cyl <- as.factor(cyl, level = c("4","6","8"))

# 오늘은 휴식

#ggplot2 pratices

mpg <- as.data.frame(ggplot2::mpg) 


# 독서로 대체
#review


w_train$Weekly_Sales
plot(w_train$Weekly_Sales)
hist(w_train$Weekly_Sales)
summary(w_train)
summary(w_train)

w_train %>% group_by(IsHoliday) %>% summarise(mean = mean(Weekly_Sales))

w_train %>% group_by(Dept) %>% summarise(mean = mean(Weekly_Sales)) %>% 
  ggplot(aes(x=Dept)) +geom_histogram()    

w_train %>% head(5)

ggplot(data=w_train) + geom_histogram(aes(x = Store, y=Weekly_Sales))
>>>>>>> 59c2020c61ee7f03e3eeeed1e870a614a7877d51

summary(car_lm)
