#####따라하며 배우는 데이터 과학#####
###9장 빅데이터 분류분석 두번째 - 라쏘, 랜덤 포레스트, 그래디언트 부스팅
#GLM 모형의 경우 최대우도추정치를 찾는다
#GLM 최대우도추정치의 방법으로 1000개의 변수가 있다면 X변수의 모수를 계산한다
#이러한 복잡한모형은 해석능력을 떨어뜨리며 예측능력이 떨어진다.
#이에 boot 패키지에 cv.glm의 경우 유의미한 변수를 선택하여 최종모델을 만들어 준다
#그리고 모향의 복잡도를 줄여주는 방법으로 함수식을 최소화 한다. - penalized maximum likelihood
#라쏘 - L1-norm, 능형회귀 - L2-norm, 일래스틱넷-라쏘와능형 모형을 일반화 하여 모형계산

require(tidyverse)
require(glmnet)

#glm함수와는 다르게 glmnet함수는 모형행렬을 수동으로 설정해야 한다.
xx <- model.matrix(wage~.-1,data=ad) #전체데이터를 모형행렬로 변경한다음, 절편항은 필요하지 않기 때문에 -1
x <- xx[train_idx, ] #그중에서 train_idx에 포함되는 행을 x로 넣는다
y <- ifelse(train$wage==">50K",1,0) #train의wage의 값이 >50k면 1, 아니면, 0 (소문자 k때문에 에러났ㅇ
dim(x) #정상적으로 범주형변수가 수치로만 이루어진 행렬이 되었는지 dim으로 확인
table(y)
ad_glmnet_fit <- glmnet(x, y)
plot(ad_glmnet_fit) #디폴트로 알파 = 1 라쏘모형으로 적합된다.

ad_glmnet_fit #람다의 값이 줄어드는 순서로 정렬되어 있다.
#첫번째 행은 df = 0 #dev=0 -> 가장 간단하지만 가장 쓸모 없는 모형이다
#가장 마지막행은 가장 복잡한 모형이여 거의 모든 변수 df=94 %dev=37.06
#이처럼 glmnet 함수의 결과는 다양한 람다값에 대한 모형이다.

#자동모형선택 ->cv.glmnet
#위처럼 다양한 람다 값의 모형중, 어떤 모형을 선택해야 하는가?

ad_cvfit <- cv.glmnet(x,y,family = "binomial") #교차검증 cv.glmnet 함수에 family=binomial로 로지스틱모형으로 적합
plot(ad_cvfit)
#x축은 람다 값이며 상단에는 x변수의 수가 적혀 있으며 그래프의 왼쪽으로 갈수록 a모든 x 변수를 사용한다.
#중간위 실선은 최적의 람다 값을 나타난다 
#1. lambda.min : 교차검증 오차의 평균값을 최소화하는 값
#2. lambda,1se : 교차검증 오차의 평균값이 최소값으로 부터 1-표준편차 이상 떨어지지 않는 가장 간단한 람다값.
log(ad_cvfit$lambda.min)
log(ad_cvfit$lambda.1se)

coef(ad_cvfit, s=ad_cvfit$lambda.1se)
coef(ad_cvfit, s="lambda.1se")

#각 람다값의 선택된 모수의 개수
length(which(coef(ad_cvfit, s="lambda.min")>0)) #48개
length(which(coef(ad_cvfit, s="lambda.1se")>0)) #24개

#9.1.4 알파값 찾기는 나중

