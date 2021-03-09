#####따라하며 배우는 데이터 과학#####
###8장 빅데이터 분류분석 첫번째
#지도변수(Supervised Learning) - 설명변수로부터 반응변수를 예측하는 방법
#그 중, 분류분석은 주어진 설명변수로 부터 범주형 반응변수를 예측하는 방법이다.
#회귀분석은 반응변수가 연속형 또는 수치형 반응변수를 예측 하는 방법이다

#분류분석
#채무불이행 확률, 투자에 성공할 확률, 광고를 클릭할 확률 등 성공과 실패의 확률을 예측하기 위함
#즉 이항분류 분석문제만 학습할 예정
#목적 - 미래 데이터의 정확성, 변수간의 관계 이해
#분류분석에서 ROC곡선으로 모형의 예측 능력을 평가 -가로축(FPR),세로축(TPR)
#ROC곡선이 위에 있는 모형이 더 예측력이 좋은 모형이다.
#ROC곡선 아래의 영역은 AUC이며 곡선아래의 면적을 0.5과1사이의 값으로 나타내며 1에 가까울수록 좋은 모형

#분류분석 순서
#1.데이터 구조파악 - y변수,x변수의 타입 등
#2.데이터는 훈련,검증,테스트로 6:2:2 로 나눈다
#3.시각화로 y와 x 관계파악, x간의 상관관계, 이상치, x의 변환 필요 여부 등
#4.다양한 분류분석 방법 적용 - 로지스틱, 라쏘,트리,랜덤포레스트, 부스팅
#5.변수 유의성확인, 적절한 시각화, 모형의 정확도 확인
#6.검증세트로 가장성능이 좋은 모형 확인

install.packages("ISLR")
install.packages("glmnet")
install.packages("randomForest")
install.packages('gbm')
install.packages("rpart")
install.packages("boot")

require(tidyverse)
require(ISLR)
require(MASS)
require(glmnet)
require(randomForest)
require(gbm)
require(rpart)
require(boot)


ad <- read.csv("adult.data",header = F,strip.white = T)
names(ad) <- c('age', 'workclass', 'fnlwgt', 'education',
                  'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hours_per_week', 'native_country',
                  'wage')
glimpse(ad) #마지막 wage 변수가 y 반응변수이다

#y변수 확인
ad %>% dplyr :: select(wage) %>% table() 
round(prop.table(table(ad$wage)),2)  #50k미만이 24,720로 76%, 50k 이상이 7,841로 24% 차지
levels(ad$wage) #y변수는 level을 가지고 있으며 각 1과 2 수치에 대응된다

#위 level 정보가 중요한 이유
#1.glm함수에서 binomial패밀리 사용시 범주형 반응변수의 첫번째 레벨이 F, 나머지는 S로 간주됨-50K 이상은 성공의미
#2.glmnet,cv.glmnet함수에서 binomial패밀리 사용시 범주형 반응변수는 두 level 가져야 하며 
#2-1.첫번째 level = F, 두번째 level = S - 50K이상은 성공의미, 반응변수가 두가지 레벨만 가질경우 glm과 동일
#3.randomforest는 반응변수가 여러레벨을 가져도 되며 예측은 각각의 레벨의 확률값으로 주어진다
#4.gbm함수에서 반응변수는 distribution=bernoulli 옵션으로 다루며 0=실패 1=성공으로 간주
#4-1.현재 1과 2의 레벨에서 0과 1의 레벨로 변환이 필요하다.(여러레벨인 경우 distribution=multinomial 사용)


#x변수 타입 확인
summary(ad) 
#범주형 설명변수인 경우 복잡도는 어떻게 되는 것일까?
#설명변수는 모형행열이라고 하는 수치로만 이루어진 행렬로 변환되어야 한다.
#간단히 model.matrix 함수로 변환 가능하며 실제 adult의 컬럼은 총 101개가 된다.


#내일은 훈련,검증, 테스트 세트로 구분할 예정



