install.packages(c("caret","dplyr","GGally","pROC"))
library(MASS)
library(caret)  
library(dplyr)  
library(ggplot2)
library(GGally)  
library(pROC)

# 필요한 패키지와 스크립트 다운 받기 --------------------------------------
source("decisionplot.R")


# 타이타닉 자료 -----------------------------------------------------------
# https://www.kaggle.com/c/titanic
# 타이타닉 승객들의 생존여부 예측
titan <-read.csv("train.csv")
View(titan)
library(dplyr)  
titan <- titan %>% 
  dplyr::select(Sex, Pclass, Survived) %>% 
  mutate(Survived = as.factor(ifelse(Survived == 1,"Yes","No")))

report.tab <- xtabs( ~  Sex + Pclass + Survived, data = titan)
mosaicplot(report.tab)

library(ggplot2)
titan  %>% ggplot(aes(x = Sex)) + geom_bar(aes(fill = Survived), position = "fill") + facet_wrap(~Pclass)
titan  %>% ggplot(aes(x = Sex)) + geom_bar(aes(fill = Survived), position = "fill")  
titan  %>% ggplot(aes(x = Pclass)) + geom_bar(aes(fill = Survived), position = "fill") 


titan %>% group_by(Sex, Pclass) %>% 
  summarise(P_survive = mean ( Survived == "Yes"), P_demise= mean ( Survived == "No")) 


# 대학원 입학 자료 -------------------------------------------------------
# https://www.kaggle.com/mohansacharya/graduate-admissions

rawdata <- read.csv("Admission_Predict.csv")
data <- rawdata %>% mutate(Class = ifelse(Chance.of.Admit > 0.7, "Admit", "No")) %>% 
  dplyr::select(TOEFL.Score,CGPA,Class)
data$Class <- as.factor(data$Class)

library(GGally) # 쌍별 산점도를 그리기 위해  
ggpairs(data, mapping = aes(color = Class))

data %>% ggplot(aes(x = TOEFL.Score, y= CGPA, color = Class)) + geom_point()



# Section 2 ---------------------------------------------------------------
 

# knn ---------------------------------------------------------------------
library(caret) 
# Class 변수가 범주형 변수인지 확인할 것
knn_out <- knn3(Class ~ ., data = data, k = 5) 
decisionplot(model = knn_out, data = data, class = "Class", main = "kNN")
knn_predictions <- predict(knn_out, data, type = "class")

# 정규화를 한 이후에 
data2 <- data %>% mutate(TOEFL.Score = c(scale(TOEFL.Score)), 
                         CGPA = c(scale(CGPA)))
out <- knn3(Class ~ ., data = data2, k = 5) 
decisionplot(model = out, data = data2, class = "Class", main = "kNN")



# LDA ---------------------------------------------------------------------

lda_out <- lda(Class ~ ., data = data)
decisionplot(model = lda_out, data = data, class = "Class", main = "LDA")
lda_predictions <- predict(lda_out,data)
 
# QDA 
qda_out <- qda(Class ~ ., data = data)
decisionplot(model = qda_out, data = data, class = "Class", main = "QDA")
qda_predictions <- predict(qda_out,data)

# 로지스틱 회귀분석 -------------------------------------------------------
logi_out <- glm(Class ~ ., "binomial", data = data) 
summary(logi_out)
decisionplot2(model = logi_out, data = data, class = "Class", main = "Logistic Regression") 


# Section 3 ---------------------------------------------------------------
cm <- confusionMatrix(lda_predictions$class,data$Class)
cm

library(pROC)
roc.out <- roc(response = data$Class, predictor = lda_predictions$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)


## positive 범주와 negative 범주를 바꿈
head(data$Class) # 변수의 첫 번째 범주를 positivskime 범주로 취급

# "No" 범주를 positive으로 바꿈: 
data.s <- data
data.s$Class <- relevel(data.s$Class, "No")

lda_out2 <- lda(Class ~ ., data = data.s)
lda_predictions2 <- predict(lda_out2,data.s)
cm2 <- confusionMatrix(lda_predictions2$class,data.s$Class)
cm2
 
roc.out <- roc(response = data.s$Class, predictor = lda_predictions2$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)




# 연습을 들어가기 전에 -----------------------------------------------------

# 다음의 패키지들 중 설치되지 않은 것들을 설치하자
# 
# library(skimr) # 데이터 요약
# library(dplyr) # 데이터 전처리
# library(corrplot) # 상관계수 행렬
# library(GGally) # 쌍별 산점도  
# library(broom) # t.test의 결과를 깔끔하게 
# library(moments) # 왜도와 첨도 계산 
# library(caret)  # 복잡한 회귀와 분류 문제에 대한 모형 훈련 (classification and regression training)



# 하부 요통(lower back pain) 예측

install.packages(c("skimr","corrplot","broom","moments"))
# 라이브러리 로드하기  ----------------------------------------------------------
library(skimr) # 데이터 탐색
library(dplyr) # 데이터 전처리
library(corrplot) # 상관계수 행렬
library(GGally) # 쌍별 산점도  
library(broom) # t.test의 결과를 깔끔하게 
library(moments) # 왜도와 첨도 계산 
library(caret)  # 여러 방법을 이용하여 예측모형을 만들 때 쉽게 이용 (Classification And REgression Training)


# 데이터 로드하기  ---------------------------------------------------------------


# 데이터 출처:  
# https://www.kaggle.com/sammy123/lower-back-pain-symptoms-dataset
data_raw <- read.csv("Dataset_spine.csv")


# 탐색적 자료 분석 (Exploratory Data Analysis) -----------------------------------


# 변수의 갯수가 많지 않을 때는 각각의 변수를 살펴보는 것이 가장 좋음. 
# 데이터 살펴보기 
summary(data_raw)

# summary() 대신 skimr 패키지에 있는 skim() 함수를 사용해도 됨
library(skimr) 
skim(data_raw)

View(data_raw)
# X 변수가 이상함... 자세히 살펴보면, 이 변수가 코드북으로 되어있음을 발견할 수 있다.
# 코드북을 활용하면 의미있는 변수 명을 만들 수 있다.


# 다음은 변수명을 바꿔주는 코드이다. 바꿔준 이후에 X 변수는 제거한다. 
library(dplyr)  
data <- data_raw %>% 
  dplyr::rename('pelvic_incidence' = Col1, 
                'pelvic_tilt' = Col2, 
                'lumbar_lordosis_angle' = Col3,
                'sacral_slope' = Col4, 
                'pelvic_radius' = Col5, 
                'degree_spondylolisthesis' = Col6, 
                'pelvic_slope' = Col7, 
                'direct_tilt' = Col8, 
                'thoracic_slope' = Col9, 
                'cervical_tilt' = Col10, 
                'sacrum_angle' = Col11, 
                'scoliosis_slope' = Col12) %>%
  dplyr::mutate(Class_att = as.factor(Class_att)) %>% 
  dplyr::select(-X)  

head(data)

# Class_att를 예측하기 위해 처음 12개 변수 사용.  

# 12개의 예측 변수(많지 않음)밖에 없기 때문에, 변수들 간의 연관성 구조를 살펴볼 수 있음.  
library(corrplot)  
corrplot(cor(data[,1:12])) 
# 처음 6개의 변수들은 강하게 연관되어 보임. 사실, "scaral_slope"변수는 처음 두 변수들과 완전히 연관되어 있음.  
# sacral_slope =  pelvic_incidence - pelvic_tilt,
# 이는 error가 0인 회귀분석으로 확인 가능:
summary(lm(sacral_slope ~ pelvic_incidence + pelvic_tilt, data = data))

# 어떤 분석에서는, "sacral_slope" 변수를 제거하는 것이 타당함. 
# data <- data %>% dplyr::select(-sacral_slope)
# 이 작업은 일단 뒤로 미루도록함. 

# 예측 변수가 많지 않으므로, 산점도를 확인할 수 있음.
library(GGally) 
ggpairs(data, mapping = aes(color = Class_att))


# 변수 변환   --------------------------------------------------------------
# 때때로 통계 학습의 성능을 높이기 위해 기존 변수를 통해 다른 변수를 만드는 것이 합리적이다.
# 데이터사이언스 분야에서는 "feature engineering" 이라고도 불림. 
# 특별히 "degree_spondylolisthesis"(6번째 열) 변수가 하부 요통(lower back pain)을 잘 분류해내는지 확인해보자. 좀 더 자세히 관찰해보자.  
# (ggpairs 함수는 변수 하나에 대해서 사용하면 안되지만, 여기서는 두 개의 일변량 분포를 비교하는데 사용함.)

summary(data[,6]) 
ggpairs(data, mapping = aes(color = Class_att), columns = 6) 


# 데이터가 0근처에 모여 있음. 이상치가 존재함. 변환을 시도해보자. 
# x 변수를 log10(x - 2min(x))로 대체.
min_d_s <- min(data$degree_spondylolisthesis) * 2
tdata <- data %>% mutate( degree_spondylolisthesis = 
                  log10( degree_spondylolisthesis - min_d_s ) )
ggpairs(tdata, mapping = aes(color = Class_att), columns = 6) 

# 위에서 살펴보면, log10(x - 2min(x))은 합리적인 변환들 중 하나로 볼 수 있음. 더 시도해보자.

# knn을 포함한 일부 방법의 경우 데이터의 범위가 분석의 질에 영향을 준다.
# 범위를 무시하는 간단한 방법 중 하나는 평균은 0, 표준 편차는 1이 되도록 변환해 주는 것이다.

tdata.num.id <- unlist(lapply(tdata, is.numeric))
tdata[,tdata.num.id] <- tdata[,tdata.num.id] %>% scale()


# 이 두 가지 변환은 데이터베이스의 새로운 항목에 대해 수행할 수 있다. 
# 분류에서 이는 매우 중요한데, 이는 목표가 *새로운* 관측치가 "정상(Normal)"인지 "비정상(Abnormal)"인지 분류하는 것이기 때문이다. 

# 산점도를 다시 관찰해보자.
ggpairs(tdata, mapping = aes(color = Class_att))

# "scaral_slope" 변수와 같이 이상치를 가진 변수가 있음. 더 많은 변수 변환을 시도할 수 있다. 
# 이 시점에 변수 7-12은 판별력이 없는 것으로 보인다. 
# 이 것을 기억하고 다음으로 넘어가자. 


# 많은 변수들에 대한 탐색적 자료 분석 및 변수 변환 ------------------
# 수십, 수백 (또는 수천)개의 변수가 있는 경우 위의 탐색 절차는 의미가 없음. 
# 그래도 그래픽 형식이 아닌 데이터를 변수별로 "보는" 방법은 의미가 있음.
# 한 가지 방법은 각 변수별 (a) t-통계량 값과 (b)왜도/첨도를 검사하는 것. 
# 다음의 코드가 이를 해줌.  
library(broom)  
library(moments)  
t_stats <- lapply(dplyr::select_if(tdata, is.numeric), 
                  function (x) broom::tidy(t.test(x ~ tdata$Class_att) ) ) 
t_stats <- bind_rows(t_stats, .id = "variable")
skew <- sapply(dplyr::select_if(tdata, is.numeric), moments::skewness)
kurt <- sapply(dplyr::select_if(tdata, is.numeric), moments::kurtosis)
t_stats %>% 
  dplyr::select(variable, statistic, p.value) %>% 
  bind_cols(list(skewness = skew,kurtosis = kurt ))

# testing과 training으로 데이터로 분리 -----------------------------------
N <- nrow(tdata)

# 항상 랜덤 seed를 설정해주는 것이 바람직함.
set.seed(1)
# 데이터의 75%를 training으로 설정하고 나머지 25%는 testing을 위해 따로 배정한다. 
train.id <- sample(1:N, floor(N*.75), replace = F)
trdata <- tdata[train.id,]
test <- tdata[-train.id,] 


# k-NN 분류기 ---------------------------------------------------

# k-NN 분류기를 훈련시키기 위해 caret :: knn3을 사용함.
# 우선, k = 5를 사용하자. 
library(caret)   
out <- knn3(Class_att ~ ., data = trdata, k = 5)
View(out)

# 표본 내(in-sample) 분류 성능을 조사해보자. 
# 아래에서 class_pred는 예측된 범주 또는 단순히 분류된 라벨임. 
# prob_pred는 5명의 이웃이 비정상인지 정상인지를 투표한 결과임.(비율)
(class_pred <- predict(out, trdata, type = "class"))
(prob_pred <- predict(out, trdata, type = "prob"))
head(prob_pred)
head(class_pred)

# confusionMatrix 함수는 혼동 행렬(confusion matrix)뿐 아니라 이를 통해 얻어지는 다양한 통계량들을 제공한다. 
confusionMatrix(class_pred,trdata$Class_att)
# 여기에 주목해보자. 
#  1) 정확도 (Accuracy)
#  2) 민감도 (Sensitivity)
#  3) 특이도 (Specificity)
#  4) 범주의 크기가 매우 불균형인 경우, 균형 정확도 (Balanced Accuracy)
# 이 분류에서 정확도, 민감도 및 특이도는 유사하다.

# 더 중요한 것은 표본 외(out-sample)에서의 성능을 조사하는 것. 
confusionMatrix(predict(out, test, type = "class"),
                test$Class_att)


# 표본 외 정확도가 표본 내 정확도에 비해 약간 작음을 참고하자. (일반적으로 성립)
# 별다른 이유없이 k = 5로 설정했으므로 다른 경우도 시도해 보자. 
# train 데이터만 접근할 수 있다고 가정하자. 이런 경우, cross-validation을 사용해야 한다.

# caret 패키지가 이런 목적에 잘 부합한다. (for light users)
# 
# train 함수를 사용해 다양한 k 값(3 - 71)과 다양한 데이터의 folds에 대해 (10 folds가 기본) knn 분류기를 반복적으로 추정할 수 있다. 

set.seed(2)
train_knn <- train(Class_att ~ ., method = "knn", 
                   data = trdata,
                   trControl = trainControl(method = "cv"),
                   tuneGrid = data.frame(k = seq(3, 71, 2)))
ggplot(train_knn, highlight = TRUE)
# 19개의 neighbors가 가장 좋은 결과를 제공함. k = 21를 선택하자. 
train_knn$bestTune

# tuned models의 성능 평가. 
confusionMatrix(predict(train_knn, trdata), 
                trdata$Class_att)
confusionMatrix(predict(train_knn, test),
                test$Class_att)

# 선택한 k (= 19)로 knn 객체를 다시 만들 수 있음. 결과는 같다. 
out19 <- knn3(Class_att ~ ., data = trdata, k = 19)
confusionMatrix(predict(out19, test, type = "class"),
                test$Class_att) 

# 선형분류판별분석(linear discriminant analysis) -------------------------- 

# MASS::lda() 함수를 사용하자.
library(MASS)
out <- lda(Class_att ~  ., data = trdata) 
# 이때 경고는 무시하도록 하자.

out

# 표본 내 예측과 표본 외 예측을 실행하자. 
# 아래에서 각 결과는 1) 분류 결과, 2) 사후 확률(정규분포 가정) 3) FLD 방향 벡터에 사영된(projected) 점수로 구성되어 있음.

pred.train <- predict(out, trdata)
pred.test <- predict(out, test)

confusionMatrix(pred.train$class, trdata$Class_att)
confusionMatrix(pred.test$class, test$Class_att)
# 실제로 성능은 나쁘지 않음.  

# ROC 곡선(Receiver-Operating Curve)과 AUC (Area Under the Curve)는 민감도와 특이도 사이의 trade-off를 보여준다.
library(pROC)
roc.out <- roc(response = test$Class_att, predictor = pred.test$posterior[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary LDA - Test data")
# 수동으로 변수 선택을 할 수 있다. 
# 예를 들어, 열 #7-#12 변수가 별로 기여하지 않는 것으로 보이므로 #1-#6 열의 변수를 선택한다. 
out16 <- lda(Class_att ~  ., data = trdata[,c(1:6,13)])
pred.test <- predict(out16, test)
confusionMatrix(pred.test$class, test$Class_att)


# 나아가 "sacral_slope" 변수는 pelvic_dㅡ로 시작하는 두 변수 사이에 공선성이 있다. 제거하자.
out17 <- lda(Class_att ~  ., data = trdata[,c(1:3,5,6,13)])
pred.test <- predict(out17, test)
confusionMatrix(pred.test$class, test$Class_att)
  
 


# quadratic discriminant analysis  -----------------------------------
 

# MASS::qda() function을 사용하자.
out <- qda(Class_att ~  ., data = trdata) 
# 공선성 때문에 에러가 발생한다. 공선성을 명시적으로 다뤄줄 필요가 있다.
select <- dplyr::select
trdata.q <- trdata %>% select(-sacral_slope)
out <- qda(Class_att ~  ., data = trdata.q)  

# 예측과 혼동 행렬(confusion matrix)
pred.test <- predict(out, test) 
confusionMatrix(pred.test$class, test$Class_att) 

# ROC 곡선(Receiver-Operating Curve)과 AUC (Area Under the Curve)  
roc.out <- roc(response = test$Class_att, predictor = pred.test$posterior[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary QDA - Test data")



# lda와 qda 모형 비교를 위한 caret 패키지 사용 -------------------------------------------

# 'sacral_slope' 변수가 제거된 데이터 셋을 사용. 
trdata.q <- trdata %>% select(-sacral_slope)

# test 데이터에 접근하지 않고, lda와 qda의 성능을 비교하려고 한다. 
# lda와 qda의 추정 결과에 대해 cross-validation을 해야한다. 
train_lda <- train(Class_att ~ ., method = "lda", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))

train_qda <- train(Class_att ~ ., method = "qda", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))

# 이제 caret :: resamples 함수를 이용하여 모든 cross-validations (resamples)의 결과를 요약할 수 있다. 
# 
models <- resamples(list(LDA = train_lda,
                         QDA = train_qda,
                         kNN = train_knn))   # knn 결과를 포함
summary(models)
dotplot(models, metric = "Accuracy")


# Regularized Discriminant Analysis ---------------------------------------


# RDA에서는 두 개의 hyperparameter (lambda, gamma)를 선택해야 함.
# 1)       QDA  0 --- lambda --- 1 LDA 
# 2) (QDA-LDA)  0 ---  gamma --- 1 (Equal variance-no correlation) 

# caret::train() 를 사용해 다양한 k 값(3 - 71)과 다양한 데이터의 folds에 대해
# RDA 분류기를 반복적으로 추정할 수 있다. 

set.seed(2)
train_rda <- train(Class_att ~ ., method = "rda", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"),
                   tuneLength = 9)
ggplot(train_rda, highlight = TRUE)

train_rda$bestTune

# 예측 (test 데이터에서의) 
pred.test <- predict(train_rda, test) 
# 혼동 행렬(Confusion Matrix)
confusionMatrix(pred.test, test$Class_att)
 

# 로지스틱 회귀분석 -----------------------------------------------

# 공선성이 없는 데이터를 사용함. 그렇지 않으면 로지스틱 회귀분석을 계산하기 위한 알고리즘 적합이 수렴하지 않기 때문.
trdata.q <- trdata %>% select(-sacral_slope)
glm.out <- glm(Class_att ~ ., family = "binomial", data = trdata.q)
summary(glm.out)
# GLM은 사실상 회귀분석 같은 것이다. 각 예측 변수에 대한 유의성 결과가 나옴. 
# AIC 값을 이용한 단계(stepwise) 변수 선택을 시도해 볼 수도 있음. 
glm.step.out <- stepAIC(glm.out,direction = "backward")

# 분류 방법으로, 혼동 행렬을 추출하고 ROC 곡선을 그려볼 수 있음. 
# 다시 caret 패키지 사용으로 돌아가자.
 
train_glm <- train(Class_att ~ ., method = "glm", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))
train_glmstep <- train(Class_att ~ ., method = "glmStepAIC", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))

# 방법을 명시하는 것만으로도 다양한 방법들을 시도하기에 충분하는 것을 알 수 있다.
# https://topepo.github.io/caret/available-models.html 를 보면 caret 패키지에서 사용가능한 모형을 확인할 수 있음.
# cv를 사용하는 이유는 lda, qda, knn 방법과 로지스틱 회귀분석 방법의 성능을 비교하기 위함. 

# 예측 (test 데이터에서의) 
pred.test <- predict(train_glm, test)
pred.test_prob <- predict(train_glm, test, type = "prob")
# 혼동 행렬(Confusion Matrix)
confusionMatrix(pred.test, test$Class_att)
# ROC 곡선(Receiver-Operating Curve)과 AUC (Area Under the Curve)  
roc.out <- roc(response = test$Class_att, predictor = pred.test_prob[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary logistic regression - Test data")




# 전체 결과 비교 -----------------------------------------------------------


# 이제 caret :: resamples 함수를 이용하여 모든 cross-validations (resamples)의 결과를 요약할 수 있다. 
# 
allmodels <- list(LDA = train_lda,
                  QDA = train_qda,
                  kNN = train_knn,
                  RDA = train_rda,
                  Logistic = train_glm,
                  Logistic_AIC = train_glmstep)
mods <- resamples(allmodels)   # including the knn results
summary(mods)
dotplot(mods, metric = "Accuracy")

# 많은 실제 상황에서는 testing 데이터가 없음. testing 데이터는 강의실이나 대회에만 존재함.
# testing 데이터는 따로 나눴으므로 testing 데이터로 방법의 정확성을 확인해 볼 것. 

test.acc <- as.data.frame(
       lapply(allmodels, 
            function (x) 
              {a <- confusionMatrix(predict(x, test), test$Class_att); c(a$overall,a$byClass)}) )
test.acc 

