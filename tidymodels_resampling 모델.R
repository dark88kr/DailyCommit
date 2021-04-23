#3. Evaluate model with resampling
#cell의 이미지가 잘 분리 되었는지 확인하는것
#modeldata패키지의 cell 데이터 사용

install.packages("modeldata")

library(tidymodels)
library(modeldata)

data(cells, package = "modeldata")
cells #y는 class 변수

cells %>% 
  count(class) %>% 
  mutate(porp = n/sum(n)) #64.5%가 잘못 분류된 데이터이다

#일단 model생성 이전에 train과 test 데이터로 나눈다
#train은 매개변수를 추정하고 모델과 엔지니어링 변수를 비교하고 튜닝에 사용
#test 는 최종모델의 성능을 측정하기 위한 소스로 사용 되며 
#test 데이터를 사용하기 이전에 이미 최종모델로 2~3개가 선정되어야 한다.

set.seed(123)
cell_split <- initial_split(cells %>% select(-case),
                            strata = class)
#strata 에서 컬럼을 지정하면 실제 해당 컬럼과 비슷한 비율로 세트르 만든다

cell_train <- training(cell_split)
cell_test <- testing(cell_split)

nrow(cell_train)/nrow(cells) #cell_train이 몇%정도 분배 되어 있나 

cell_train %>% 
  count(class) %>% 
  mutate(prop = n /sum(n)) #train 데이터의 class의 비율은?
#streta 요소 사용으로 원본과 비슷한 비율로 train 데이터 생성됨

cell_test %>% 
  count(class) %>% 
  mutate(prop = n/sum(n)) #test 데이터도 함께 확인

#모델링 이후 랜덤 포레스트 모델 사용 예정
#랜덤포레스트의 장점은 유지관리가 적으며, 데이터 전처리가 거의 필요하지 않다.
#cell 데이터는 recipe 를 이용한 전처리는 진행하지 않는다

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(class ~ . , data = cell_train) #랜덤포레스트 모델 적용하기

#모델링 프로젝트로 다양한 모델을 생성할수 있다.
#ROC 커브로 모델의 성능을 계산 할수 있다.
#첫번째로 50% 확률컷오프로 잘못 분활된 셀을 분류한다.
#train set로 모델을 평가하면 안된다.

rf_training_pred <- 
  predict(rf_fit, cell_train) %>% 
  bind_cols(predict(rf_fit, cell_trin, type = "prob")) %>% 
  bind_cols(cell_train %>% 
              select(class))

rf_training_pred %>% 
  roc_auc(truth = class, .pred_ps)

rf_training_pred %>% 
  accuracy(truth = class, .pred_class)

#완벽하게 분류한다, - overfitting.


rf_testing_pred <- 
  predict(rf_fit,cell_test) %>% 
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>%
  bind_cols(cell_test %>% select(class))

rf_testing_pred %>% 
  roc_auc(truth = class, .pred_ps)

rf_testing_pred %>% 
  accuracy(truth = class, .pred_class)

#구조를 재조정한다.
#resampling 방법은 교차검증이나, 부트스트랩처럼 경험적 시뮬레이션 시스템이다.
#initial_split 함수로 train과 test 세트를 나눈다.

#설면됭 교차 검증을 직접 실현 가능 하다

set.seed(345)
folds <- vfold_cv(cell_train, v=10)

#어떤 row가 analysis와 assessment로 나뉘는지 analysis(), assessment() 확인가능


rf_wf <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_formula(class ~ .)

set.seed(456)
rf_fit_rs <- 
  fr_wf %>%
  fit_resamples(folds)

collect_metrics(rf_fit_rs)
