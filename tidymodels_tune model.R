#4.Tune model Parameter
#hyperparameter

#기존 데이터 셋으로는 학습할 수 없는 parameter가 존재
#트리기반모형의 분활에 샘플링된 예측변수의 수, 부스트 트리모형의 학습률 등
#이러한 하이퍼 파라미터를 학습하는 대신 resampling 된 많은 데이터 모델을 학습하고
#이러한 모델 성능을 탐색하여 최상의 값을 추정 할수 있다
library(tidymodels)  # for the tune package, along with the rest of tidymodels
library(modeldata)   # for the cells data
install.packages("vip")
library(vip)    

data(cells, package = "modeldata")
cells

#잠시 통계 공뷰부주