
# 필요 패키지 설치(실습 시작 전 실행) ---------------------------------------------------

install.packages("mlbench") # 데이터셋
install.packages("tree")    # 의사결정나무 #R 3.6버전 이상 필요
install.packages("ipred")   # 배깅
install.packages("randomForest")  #랜덤포레스트
install.packages("pdp")     #부분의존도 그림



# 데이터 전처리 -----------------------------------------------------------------

library(mlbench)

data(BreastCancer)
dim(BreastCancer)
head(BreastCancer)

BreastCancer <- BreastCancer[,-1]
colSums(is.na(BreastCancer))
BreastCancer <- BreastCancer[complete.cases(BreastCancer), ]
dim(BreastCancer)
# 설명변수의 데이터형을 보면 ordered factor로 되어있다.
head(BreastCancer$Cell.size)
class(BreastCancer$Cell.size)
# Class를 제외한 설명변수를 모두 수치형으로 변환한다.
XIdx <- !(names(BreastCancer) %in% "Class")
BreastCancer[XIdx] <-
  Map(as.numeric, BreastCancer[XIdx])

head(BreastCancer$Cell.size)
class(BreastCancer$Cell.size)

# 전체 데이터의 30%를 test 데이터로 사용한다. test 데이터로 사용할 데이터의 번호를 BC.TsIdx에 저장한다.
BC.n <- nrow(BreastCancer)
ratioTs <- 0.3
BC.nTs <- round(ratioTs * BC.n)
BC.nTr <- BC.n - BC.nTs
set.seed(1)
BC.TsIdx <- sample(BC.n, BC.nTs)
# mlbench를 불러온 후 data 함수를 통해 내장된 데이터 BostonHousing을 불러올 수 있다.
# 보스턴 506개의 구역으로 나눠 집값과 방의 수, 연식, 위치적 특성, 거주자 특성 등을 요약한 자료이다.
data(BostonHousing)
dim(BostonHousing)
# 관심있는 변수는 구역별 집값의 중앙값(medv)이며 단위는 \$1000이다.
head(BostonHousing)
# 전체 데이터의 30%를 test 데이터로 사용한다. test 데이터로 사용할 데이터의 번호를 BH.TsIdx에 저장한다.
BH.n <- nrow(BostonHousing)
ratioTs <- 0.3
BH.nTs <- round(ratioTs * BH.n)
BH.nTr <- BH.n - BH.nTs
set.seed(1)
BH.TsIdx <- sample(BH.n, BH.nTs)



# 의사결정나무 ------------------------------------------------------------------

# tree 패키지의 tree 함수를 사용하여 의사결정나무를 적합할 수 있다.
# BreastCancer[-BC.TsIdx, ]를 통해 train 데이터를 선택하여 data 인자로 넣어준다.
# formula로 어떤 변수를 사용하여 어떤 변수를 예측할지 지정해준다. Class $\sim$ . 는 나머지 모든 변수로 Class를 예측한다는 formula이다.
library(tree)
BC.TreeFit<-tree(Class~., data=BreastCancer[-BC.TsIdx, ])

# 적합 결과를 출력하면 다음과 같이 각 노드에서 어떻게 분할되는지 볼 수 있다.
# 한 눈에 알아보기는 어렵다.
BC.TreeFit

plot(BC.TreeFit)
text(BC.TreeFit)

# tree.control로 마디를 언제까지 분할할지 지정할 수 있다.
# mindev를 지정하면 끝마디의 deviance가 뿌리마디의 deviance의 mindev배보다 클 때까지만 분할한다.
# mindev는 기본 0.01이며 커질수록 더 적게 분할하며 작아질수록 더 많이 분할한다.

BC.BigTreeFit<-tree(Class~., data=BreastCancer[-BC.TsIdx, ], 
                      control = tree.control(BC.nTr, mindev = 0.001))
BC.SmallTreeFit<-tree(Class~., data=BreastCancer[-BC.TsIdx, ], 
                      control = tree.control(BC.nTr, mindev = 0.1))

plot(BC.BigTreeFit)
text(BC.BigTreeFit)

plot(BC.SmallTreeFit)
text(BC.SmallTreeFit)



# 이론 강의의 교차확인 예제와 비슷한 방식으로 최적 모형을 고르려고 한다.
# 교차확인 예제에서는 주어진 모형 집합이 있으나 의사결정나무에서는 데이터에 따라 모형이 결정되므로 그대로 적용할 수는 없다.
# 최적 모형을 선택하는 대신 최적 끝마디 수를 선택한다.
# cv.tree에 적합된 의사결정나무와 fold의 수 K를 입력해주면 된다. K의 기본값은 10이다.

# 다음 그래프는 끝마디 수에 따른 교차확인 deviance를 나타낸 것이다.
# $\text{비용복잡도}(a)=\text{나무 } T\text{의 } deviance + a|T|$에서 $x$축이 $|T|$, $y$축이 교차확인 $deviance$에 해당된다.
# $\text{비용복잡도}(a)$를 최소로 하는 $|T|$를 $a$의 함수로 볼 수 있으며 $x$축을 $|T|$ 대신 $a$에 대해서 표현할 수 있다. 그래프 위에 나타나 있다.

BC.TreeCV <- cv.tree(BC.BigTreeFit)
plot(BC.TreeCV)

# BC.TreeCV에는 끝마디 수에 따른 교차확인 deviance가 저장되어있으며 이를 최소로하는 끝마디 수를 BestSize에 저장한다.
# prune.tree 함수를 이용해 적합된 나무 BC.TreeFit를 끝마디 수가 BestSize가 되도록 가지치기한다.

BestSize <- BC.TreeCV$size[which.min(BC.TreeCV$dev)]
BestSize
BC.TreePruned <- prune.tree(BC.BigTreeFit, best = BestSize)


# 가지치기된 나무는 다음과 같다. 

plot(BC.TreePruned)
text(BC.TreePruned)

# BC.TreeFit을 사용하여 test 데이터를 예측한다. 
# predict에서 type을 'class'로 하여 속한 그룹을 예측하도록 한다. 지정하지 않을 경우 확률이 계산된다.
# confusion matrix와 accuracy를 계산한다.

BC.TreeFit.pred <- predict(BC.TreeFit, BreastCancer[BC.TsIdx, ], type = 'class')

table(pred=BC.TreeFit.pred, true=BreastCancer$Class[BC.TsIdx])
BC.TreeFit.Acc <- mean(BC.TreeFit.pred==BreastCancer$Class[BC.TsIdx])
BC.TreeFit.Acc

# 다음은 BC.TreePruned의 예측 결과이다.
BC.TreePruned.pred <- predict(BC.TreePruned, BreastCancer[BC.TsIdx, ], type = 'class')

table(pred=BC.TreePruned.pred, true=BreastCancer$Class[BC.TsIdx])
BC.TreePruned.Acc <- mean(BC.TreePruned.pred==BreastCancer$Class[BC.TsIdx])
BC.TreePruned.Acc

# 회귀분석 자료에도 같은 방식으로 의사결정나무를 적합한다.
# 반응변수인 medv가 수치형이며 별도의 설정없이 자동으로 회귀모형이 적합된다.
class(BostonHousing$medv)
BH.TreeFit<-tree(medv~., data=BostonHousing[-BH.TsIdx, ])
BH.TreeFit

plot(BH.TreeFit)
text(BH.TreeFit)
##

# tree.control로 마디를 언제까지 분할할지 지정할 수 있다.
# mindev를 지정하면 끝마디의 deviance가 뿌리마디의 deviance의 mindev배보다 클 때까지만 분할한다.
# mindev는 기본 0.01이며 커질수록 더 적게 분할하며 작아질수록 더 많이 분할한다.

BH.BigTreeFit<-tree(medv~., data=BostonHousing[-BH.TsIdx, ], 
                    control = tree.control(BH.nTr, mindev = 0.001))
BH.SmallTreeFit<-tree(medv~., data=BostonHousing[-BH.TsIdx, ], 
                      control = tree.control(BH.nTr, mindev = 0.1))

plot(BH.BigTreeFit)
text(BH.BigTreeFit)

plot(BH.SmallTreeFit)
text(BH.SmallTreeFit)


# BreastCancer 데이터와 비슷하게 예측값을 계산하고 성능을 확인한다.
# 분류가 아닌 회귀모형이므로 confusion matrix와 accuracy 대신 산점도와 MSE를 구한다.

BH.TreeFit.pred <- predict(BH.TreeFit, BostonHousing[BH.TsIdx, ])
plot(x=BH.TreeFit.pred, y=BostonHousing$medv[BH.TsIdx])
#예측값의 범주 수가 끝마디 수 이하라는 한계가 있음.
BH.TreeFit.MSE <- mean((BH.TreeFit.pred-BostonHousing$medv[BH.TsIdx])^2)
BH.TreeFit.MSE

# 배깅 ----------------------------------------------------------------------

# bagging은 ipred의 bagging 함수를 사용하여 학습할 수 있다.
# bagging에서는 정해진 개수만큼 나무를 만드는데 이를 nbagg에 지정하면 된다.

library(ipred)

BC.Bag <- bagging(Class~., data = BreastCancer[-BC.TsIdx, ], nbagg = 100, coob=TRUE)
BC.Bag




# BC.Bag은 100개의 나무로 이루어져있으며 이 나무들의 정보는 BC.Bag\$mtrees에 저장되어있다.
# BC.Bag\$mtrees는 길이가 100인 리스트이며 하나의 원소가 하나의 나무를 나타낸다.
length(BC.Bag$mtrees)

# BC.Bag의 첫번째 나무를 확인하면 다음과 같다.
BC.Bag$mtrees[[1]]$btree

# 다음은 BC.Bag의 1 $\sim$ 3번째 나무를 나타낸 그래프이다. 
par(mfrow=c(1,3))
for(i in 1:3){
  plot(BC.Bag$mtrees[[i]]$btree)
  text(BC.Bag$mtrees[[i]]$btree)
}
par(mfrow=c(1,1))

# BC.Bag는 이러한 나무 100개로 이루어져있다.
# 수많은 나무로 이루어진 bagging을 직접 해석하는 것은 어렵다.
# 부분의존도 그림을 통해 일부 변수에 대해 해석할 수 있다.
# 부분의존도 그림은 BostonHousing 데이터에서 소개한다.

BC.Bag.pred <- predict(BC.Bag, newdata = BreastCancer[BC.TsIdx, ])
table(pred=BC.Bag.pred, true=BreastCancer$Class[BC.TsIdx])
BC.Bag.Acc <- mean(BC.Bag.pred==BreastCancer$Class[BC.TsIdx])
BC.Bag.Acc


# pdp 패키지의 partial 함수로 부분의존도 함수를 계산한 후 그 결과를 plotPartial에 입력하여 부분의존도 그림을 그릴 수 있다.
# partial 함수에는 적합된 모델과 모델에 포함된 설명변수 중 관심있는 변수 하나의 이름을 지정해줘야한다.
# ipred나 randomForest 등 자주 사용되는 여러 패키지들에서 학습된 모델을 지원한다.

# 세포 크기(Cell.size)가 증가할수록 Class가 악성(maligant)일 확률의 예측값이 증가한다.
library(pdp)
BC.Bag.Cell.size <- partial(BC.Bag, pred.var = 'Cell.size', which.class="malignant")
plotPartial(BC.Bag.Cell.size)

# 덩이리 굵기(Cl.thickness)가 증가할수록 Class가 악성(maligant)일 확률의 예측값이 증가한다.
BC.Bag.Cl.thickness <- partial(BC.Bag, pred.var = 'Cl.thickness', which.class="malignant")
plotPartial(BC.Bag.Cl.thickness)

# 접착력(Marg.adhesion)이 증가할수록 Class가 악성(maligant)일 확률의 예측값이 증가한다.
BC.Bag.Marg.adhesion <- partial(BC.Bag, pred.var = 'Marg.adhesion', which.class="malignant")
plotPartial(BC.Bag.Marg.adhesion)




# bagging에서도 예측할 변수 medv가 수치형이기 때문에 별 다른 설정없이 회귀모형이 적합된다.
BH.Bag <- bagging(medv~., data = BostonHousing[-BH.TsIdx, ], nbagg = 100, coob=TRUE)
BH.Bag

BH.Bag.pred <- predict(BH.Bag, newdata = BostonHousing[BH.TsIdx, ])
plot(x=BH.Bag.pred, y=BostonHousing$medv[BH.TsIdx])
BH.Bag.MSE <- mean((BH.Bag.pred-BostonHousing$medv[BH.TsIdx])^2)
BH.Bag.MSE


# 범죄율(crim)이 증가할수록 집값(medv)의 예측값이 감소한다.
library(pdp)
BH.Bag.crim <- partial(BH.Bag, pred.var = 'crim')
plotPartial(BH.Bag.crim)

# 방의 수(rm)가 증가할수록 집값(medv)의 예측값이 증가한다.
BH.Bag.rm <- partial(BH.Bag, pred.var = 'rm')
plotPartial(BH.Bag.rm)

# 지위가 낮은 사람의 비율(lstat)이 증가할수록 집값(medv)의 예측값이 감소한다.
BH.Bag.dis <- partial(BH.Bag, pred.var = 'lstat')
plotPartial(BH.Bag.dis)

# 랜덤포레스트 ------------------------------------------------------------------


# randomForest패키지의 randomForest 함수를 이용하여 랜덤포레스트를 적합할 수 있다.
# randomForest에서는 나무의 개수 ntree와 각 노드에서 사용할 후보 변수의 수 mtry를 지정해야한다.
# 총 설명변수의 수를 $p$라 할 때, ntree의 기본값은 $500$이며 분류모형일 때, $mtry \approx \sqrt{p}$, 회귀모형일 때, $mtry \approx p/3$이다.

#Breast Cancer data
library(randomForest)
BC.RF <- randomForest(Class~., data = BreastCancer[-BC.TsIdx, ], importance = TRUE)

BC.RF.pred <- predict(BC.RF, newdata = BreastCancer[BC.TsIdx, ])
table(pred=BC.RF.pred, true=BreastCancer$Class[BC.TsIdx])
BC.RF.Acc <- mean(BC.RF.pred==BreastCancer$Class[BC.TsIdx])
BC.RF.Acc


# importance를 사용하여 중요도를 구할 수 있다.
# 특정 변수의 값을 데이터 내에서 랜덤하게 섞어서 예측에 도움이 되지 않도록 만든 후 예측했을 때, 결과가 얼마나 안 좋아지는지를 계산한다.
# 더 많이 나빠질수록 예측에 중요한 변수라 볼 수 있다. 
head(importance(BC.RF))

# varImpPlot 함수를 사용하면 중요도를 그림으로 나타낼 수 있다.
# 두 가지 기준 공통으로 Cell.size, Bare.nuclei가 중요한 것으로 보인다.
varImpPlot(BC.RF)



# 세포 크기(Cell.size)가 증가할수록 Class가 악성(maligant)일 확률이 증가한다.
BC.RF.Cell.size <- partial(BC.RF, pred.var = 'Cell.size', which.class="malignant")
plotPartial(BC.RF.Cell.size)

# 덩이리 굵기(Cl.thickness)가 증가할수록 Class가 악성(maligant)일 확률이 증가한다.
BC.RF.Cl.thickness <- partial(BC.RF, pred.var = 'Cl.thickness', which.class="malignant")
plotPartial(BC.RF.Cl.thickness)

# 접착력(Marg.adhesion)이 증가할수록 Class가 악성(maligant)일 확률이 증가한다.
BC.RF.Marg.adhesion <- partial(BC.RF, pred.var = 'Marg.adhesion', which.class="malignant")
plotPartial(BC.RF.Marg.adhesion)



# Boston Housing data
BH.RF <- randomForest(medv~., data = BostonHousing[-BH.TsIdx, ], importance = TRUE)
BH.RF

BH.RF.pred <- predict(BH.RF, newdata = BostonHousing[BH.TsIdx, ])
plot(x=BH.RF.pred, y=BostonHousing$medv[BH.TsIdx])
BH.RF.MSE <- mean((BH.RF.pred-BostonHousing$medv[BH.TsIdx])^2)
BH.RF.MSE

# importance를 사용하여 중요도를 구할 수 있다.
# 특정 변수의 값을 데이터 내에서 랜덤하게 섞어서 예측에 도움이 되지 않도록 만든 후 예측했을 때, 결과가 얼마나 안 좋아지는지를 계산한다.
# 더 많이 나빠질수록 예측에 중요한 변수라 볼 수 있다. 
head(importance(BH.RF))

# varImpPlot 함수를 사용하면 중요도를 그림으로 나타낼 수 있다.
# 두 가지 기준 공통으로 lstat, rm이 중요한 것으로 보인다.
varImpPlot(BH.RF)


# 범죄율(crim)이 증가할수록 집값(medv)의 예측값이 감소한다.
BH.RF.crim <- partial(BH.RF, pred.var = 'crim')
plotPartial(BH.RF.crim)

# 방의 수(rm)가 증가할수록 집값(medv)의 예측값이 증가한다.
BH.RF.rm <- partial(BH.RF, pred.var = 'rm')
plotPartial(BH.RF.rm)

# 고용센터와의 거리(dis)가 증가할수록 집값(medv)의 예측값이 감소한다.
BH.RF.dis <- partial(BH.RF, pred.var = 'dis')
plotPartial(BH.RF.dis)


# 방법론 비교 ------------------------------------------------------------------

# logistic regression
BC.LoR <- glm(Class~.,family=binomial, data = BreastCancer[-BC.TsIdx, ])
BC.LoR

BC.LoR.prob <- predict(BC.LoR, newdata = BreastCancer[BC.TsIdx, ], type="response")
BC.LoR.pred=ifelse(BC.LoR.prob<0.5,"benign","malignant")
table(pred=BC.LoR.pred, true=BreastCancer$Class[BC.TsIdx])
BC.LoR.Acc <- mean(BC.LoR.pred==BreastCancer$Class[BC.TsIdx])
BC.LoR.Acc

# logistic regression과 Random Forest의 예측성능이 가장 좋다.
BC.LoR.Acc          # Logistic regression
BC.TreeFit.Acc      # Decision tree
BC.TreePruned.Acc   # Decision tree (pruning)
BC.Bag.Acc          # Bagging
BC.RF.Acc           # Random forest


# linear regression
BH.LM <- lm(medv~., data = BostonHousing[-BH.TsIdx, ])
BH.LM

BH.LM.pred <- predict(BH.LM, newdata = BostonHousing[BH.TsIdx, ])
plot(x=BH.LM.pred, y=BostonHousing$medv[BH.TsIdx])
BH.LM.MSE <- mean((BH.LM.pred-BostonHousing$medv[BH.TsIdx])^2)
BH.LM.MSE


# Random Forest의 예측성능이 가장 좋으며 bagging이 그 다음으로 좋다.
BH.LM.MSE       # Linear model
BH.TreeFit.MSE  # Decision tree
BH.Bag.MSE      # Bagging
BH.RF.MSE       # Random forest
