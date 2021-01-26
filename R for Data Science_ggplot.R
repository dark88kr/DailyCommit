############################R for Data Science 책 실습 
######Chapter 2 Data Visualization with ggplot2

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
mpg
str(mpg)
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ,y=hwy))
#ggplot함수에 mpg 데이터 사용하고 x=displ, y=hwy로 mapping해서 point로 그려라
#기본코드
#ggplot(data = "data" you use) + geom_function(mapping = aes(x="",y=""))
mtcars
str(mtcars)
?mpg

mpg %>% select(displ) %>% distinct()
ggplot(data = mpg) +
  geom_bar(mapping = aes(x=displ))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ,y=hwy, color=class))
#color를 class로 지정할 경우, 알아서 각자 색으로 배정함

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ,y=hwy),color = "blue")
#색만 지정할 경우 그냥 색만 바뀜

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy, size = class))
#size by class

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy, alpha = class))
#alpha 는 투명도

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy, shape = class))
#shape는 class별로 모양이 다르게 나타난다

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy)) +
  facet_wrap(~class, nrow = 2)
#각 class별로 창을 만든다


ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy)) +
  facet_grid(drv ~ .)
#drv별로 창을 나눠서 표현 (row별로 나뉨)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy)) +
  facet_grid(. ~ drv)
#drv별로 창을 나눠서 표현 (column별로 나뉨

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ,y=hwy)) +
  facet_grid(. ~ cyl)
#cyl별로 창을 나워서 표현 (column별로 나뉨)

#####geom function 확인

ggplot(data = mpg) +
  geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data = mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy))

#geom function 에 따라 같은 데이터도 그림이 달라짐

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ,y=hwy, grope=drv))
#그냥 drv 별로 그림만 나눠짐
ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ,y=hwy, color=drv))
#위 그래프와 동일하나 색이 들어감

######p.18까지 함 