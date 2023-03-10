##### 상관/회귀/분산분석 #####

##################
#### 상관분석 ####
##################

### Boston Data ###
## MASS 패키지에 내장되어 있는 Boston 데이터에서는 보스턴 지역의 중위 집값과 주요 변수들을 포함하고 있습니다.
## 자료에 관한 자세한 설명은 help(Boston)을 실행하여 얻을 수 있습니다.
## 그 중 다음 4개의 변수를 중심으로 중위 집값에 관련있는 변수를 찾아보고, 회귀분석을 진행하겠습니다.
## 변수 crim : 인구 대비 범죄율, rm : 주거지의 평균 방 개수, lstat : 지역별 lower status의 비율, medv : 중위 집값


### Boston Data ###
## 데이터 불러오기 & 데이터 구조 확인 ##
library(MASS)
data(Boston)
str(Boston)   


### Boston Data ###
## 관심 대상인 변수(열) 선택 
## 변수 crim : 인구 대비 범죄율, rm : 주거지의 평균 방 개수, lstat : 지역별 lower status의 비율, medv : 중위 집값
Boston <- Boston[,c("crim", "rm",  "lstat", "medv")]
head(Boston)


### Data Visualization ###
## 변수 간 산점도행렬(scatterplot matrix) 및 상관계수 확인 
## 산점도행렬 오른쪽 대각선 wd: 상관계수 / 대각선 왼쪽 아래: 산점도 
upper <- function(x,y){
  r=cor(x,y)
  par(usr = c(0, 1, 0, 1), mar=c(.1,.1,.1,.1)) 
  text(0.5, 0.5, round(r,2), cex = (abs(r)+1))
}   
pairs(Boston, upper.panel = upper)   # help(pairs) 참조


### Correlation Plot ###
## 상관 분석은 두 변수 사이에 선형적 관계가 있는지를 확인합니다.
## 시각화 결과 집값(medv)과 lower status 비율(lstat), 집값과 주거지의 평균 방 개수(rm) 사이에 관련성이 있음을 유추할 수 있었습니다.
## 상관 행렬을 구해, corrplot 패키지를 통해 상관관계를 시각화한 그림을 그리면 다음과 같습니다.
library(corrplot)   # 패키지 설치 후 실행 install.pacakges("corrplot)
cor.mat  <- cor(Boston)
print(round(cor.mat, 4))
corrplot(cor.mat, method="ellipse")


### Correlation Test ###
## 상관계수의 유의성 검정 
# crim vs medv
cor.test(Boston$crim, Boston$medv)   
# rm vs medv
cor.test(Boston$rm, Boston$medv)   
# stat vs medv
cor.test(Boston$lstat, Boston$medv)

####################
#### Regression ####
####################

### Regression ###
## 이제 중위 집값(medv)과 lower status 비율(lstat) 간의 선형 관계를 구해보겠습니다.
## lm 함수를 사용하여 회귀분석을 진행합니다.
## 모형 적합 후 coef, fitted, residuals 등의 함수를 사용하여 추가적 정보를 얻을 수 있습니다.
m <- lm(medv ~ lstat, data=Boston)  
print(m)   # 추정된 회귀식: medv = 34.55 - 0.95*lstat


### Regression - Fit ###
## 회귀분석 결과 
summary(m)
# 회귀계수 확인
coef(m)
# fitted value
fitted(m)[1:6]
# 잔차(residuals)
residuals(m)[1:6]
# orignal y = fitted value + residual
fitted(m)[1:6] + residuals(m)[1:6]
Boston$medv[1:6]   # 위와 동일한 결과


### Regression Plot ###
## ggplot2 이용
library(ggplot2)
ggplot(Boston, aes(x=lstat,y=medv)) + 
  geom_point() +
  stat_smooth(method='lm')

## R base plot 이용
par(mar=c(4,4,1,1), mgp=c(2,1,0)) # 그림 여백 설정. help(par) 참조
plot(Boston$medv, Boston$lstat)
#추가 직선을 그려줌
abline(m,col='blue')   # 회귀직선(fitted line) 
points(Boston$lstat,fitted(m),col='red',pch=20)


### Regression - Prediction ###
# lstat=10 일 때 medv 예측
predict(m, newdata=data.frame(lstat=10))
coef(m)[1]+coef(m)[2]*10  # 위와 동일한 결과
predict(m,newdata=data.frame(lstat=10),interval='confidence')  # 신뢰구간(95%)
predict(m,newdata=data.frame(lstat=10),interval='confidence', level=0.9)  # 신뢰구간(90%)


### Regression - Prediction & Evaluation ###
## 회귀 모형 평가를 위해 데이터를 랜덤하게 train set과 test set으로 나눕니다. 
## train set 만을 이용해 적합한 선형 모형이 test set의 새로운 관측치에 대해 얼마나 잘 예측하는지 그래프를 통해 확인합니다.
## train set 은 sample() 을 이용하여 전체 데이터의 약 80%를 랜덤하게 뽑아 사용하였습니다. 
set.seed(2021)
n = nrow(Boston)
train = sample(1:n, size= as.integer(0.8*n))   # train data 랜덤 추출
train.data = Boston[train, ]
test.data = Boston[-train,]
m.train = lm(medv ~ lstat, data=train.data)   # train data로 선형 모형 적합
predicted.test = predict(m.train, newdata = data.frame(lstat = test.data$lstat))   # test data 에 대한 예측값

# Predicton plot
par(mar=c(4,4,.1,1)) 
plot(test.data$lstat, test.data$medv, pch=20 )   
points(test.data$lstat, predicted.test, col='red',pch=10)

# Model Evaluation plot: 적합한 회귀 모형이 타당한지 확인 
par(mfrow=c(2,2), mar=c(4,4,1.2,.5), oma=c(1,1,1,1.5), mgp=c(1.7,1.2,0)) 
plot(m, cex.axis=0.8, cex.lab=0.8)

###############
#### ANOVA ####
###############

### 예1. 반복수가 일정하지 않은 일원배치법 ###
## 세 종류의 공정에서 생산된 철선의 인장 강도에 차이가 있는지 알아보자.

### 예1. 반복수가 일정하지 않은 일원배치법 ###
## 데이터 입력 
M1 <- c(2,3,4,5)
M2 <- c(4,5,6,4,3)
M3 <- c(6,5,7,4,6,8) 
M <- c(M1,M2,M3)
### 분산분석에서는 항상 factor 타입으로 변환해줄 것 
group_M <- as.factor(rep(1:3,times=c(4,5,6)))  # 처리 수준은 factor 타입으로 입력
mechanism<-data.frame(M,group_M)
head(mechanism)  # 데이터 확인
c(mean(M1), mean(M2), mean(M3), mean(M))  # 처리 수준별 평균

## 분산분석
aov_mechanism<-lm(M ~ group_M, data=mechanism)
anova(aov_mechanism)  # 분산분석표


### 예2. 반복이 없는 이원배치법 ###
## 코크스의 내압강도에 역청탄(A, coal)의 종류와, 타르피치(B, tar)의 첨가량이 영향을 미치는지 알아보자. ##

### 예2. 반복이 없는 이원배치법 ###
## 데이터 입력 
pres <- c(79,72,51,58,68,75,66,48,56,
          65,69,64,44,51,61,65,62,41,45,58) 
coal <- factor(rep(1:5, 4))
tar <- factor(rep(1:4,each=5))
cokes <- data.frame(coal,tar,pres)
head(cokes)   # 데이터 확인 

## 분산분석
anova(lm(pres~coal+tar,data=cokes))


### 예3. 반복이 있는 이원배치법 ###
## 다음은 음악 감상이 알츠하이머를 겪고 있는 환자들의 불안감 정도에 어떠한 영향을 미치는지 알아보기 위한 실험의 결과이다. 
## 초기(early stage)와 중기(middle stage)의 알츠하이머 환자들 각각에 대해 가벼운 대중음악(easy), 피아노 연주곡(piano)을 들려주고 환자들이 느끼는 불안감의 정도를 측정하였다. 
## 측정된 점수가 높을수록 불안감의 정도가 높다는 것을 의미한다. 
## 주어진 자료에 대해 유의수준 5\%에서 이원배치 분산분석을 시행해보자.

### 예3. 반복이 있는 이원배치법 ###
## 데이터 불러오기 ##
alz = read.table('alzheimer.txt', header = TRUE, stringsAsFactors = TRUE)
head(alz)
View(alz)
# A: 음악의 종류(Piano, easy) / B: 알츠하이머 진행 정도(early, middle) 
# y: 불안감 점수

# 검정하고자 하는 가설은 다음과 같다. 
# (1) 음악의 종류(A)와 알츠하이머의 진행 정도(B)의 상호작용이 존재하는가?
# (2) 음악의 종류에 따라서 불안감에 차이가 있는가?
# (3) 알츠하이머의 진행 정도에 따라서 불안감에 차이가 있는가?

## 먼저 interaction.plot() 함수를 사용하여 평균 그림을 확인한다. 
par(mfrow=c(1,1))
interaction.plot(alz$A, alz$B, alz$y, cex.axis=0.8)

## 요인별 상자그림으로 요인별 자료의 분포와 상호작용의 존재 여부를 시각적으로 추측할 수 있다. 
par(mar=c(5,4,3,3), mfrow=c(1,2))
plot(alz$A, alz$y, main="A")
plot(alz$B, alz$y, main="B")

## 분산분석
fit <- lm(y ~A*B, data = alz)
anova(fit)
# 유의수준 5\%에서 알츠하이머 병의 진행 정도(B)와 음악의 종류(A)에 따른 상호작용은 존재하는 것으로 나타났다. 이는 앞서 평균 그림을 통해서도 확인할 수 있었다. 
# 감상한 음악의 종류에 따라서 환자들의 불안감은 차이가 존재하지 않았다. 
# 알츠하이머 병의 진행 정도에 따라서 환자들이 느끼는 불안감에 차이가 존재한다. 
