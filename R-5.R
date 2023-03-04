광고자료를 통해 회귀분석을 진행한다.

이를 위하여 광고자료를 불러들인다.

```{r}
adv = read.csv("Advertising.csv", header=T, sep=",")
adv = adv[,-1]
names(adv) = tolower(names(adv)) 
str(adv)
head(adv)
```


### 자료 탐색

광고자료의 분석에 앞서 자료에 대하여 기본적인 파악이 필요하다.

이를 탐색적 자료분석(EDA)라 부르며, 다음의 코드들을 통해 탐색적 자료분석을 진행한다.


```{r}
library(Hmisc)
library(ggplot2)
library(GGally) #ggpairs 
summary(adv) 
Hmisc::describe(adv)
psych::describe(adv)
hist(adv)
pairs(adv) 

cor(adv)
ggpairs(adv)
# ggplot(adv, aes(x=sales)) + geom_histogram(bins=10)
```

그림을 Rstudio pane에서 보고 싶으면 Rstudio의 콘솔에서 동일한 명령을 한번 더 수행한다. 

### 단순 선형회귀모형의 적합

광고자료에서 반응변수가 sales일 경우의 단순선형 회귀분석을 진행한다.

이 때 독립변수는 각각 tv와 radio이다.
```{r}
lm.fit = lm(sales ~ tv, data=adv)
str(lm.fit)
summary(lm.fit)
coef(lm.fit)

lm.fit2 = lm(sales ~ radio, data=adv)
summary(lm.fit2)
```


### 신뢰구간

앞선 분석결과를 통해 90\% 신뢰구간을 구한다.

```{r}
confint(lm.fit, level=0.90)
```


### 예측

독립변수가 새로운 수치일 경우에, sales의 수치를 예측한다.

```{r}
predict(lm.fit, data.frame(tv=c(147)), level=0.90, interval="prediction")
predict(lm.fit, data.frame(tv=c(147)), level=0.90, interval="confidence")
predict(lm.fit, data.frame(tv=c(230.1, 44.5, 17.2)), level=0.95, interval="confidence")
predict(lm.fit, data.frame(tv=c(200, 50, 50)), level=0.90, interval="prediction")
predict(lm.fit, data.frame(tv=c(230.1, 44.5, 17.2)), level=0.95, interval="none")
```


### 추정치의 변동

아래는 빨간색 회귀직선을 따르는 데이터를 생성한 후, 이 데이터에 회귀직선을 적합해서 그린 그림이다. 여러번 반복해보자. 
```{r}
x = runif(100)*300
y = 7.0 + 0.04*x + rnorm(100, 0, 3.259)

sim.fit = lm(y~x)

plot(x, y, xlim=c(0,300), ylim=c(0,30));abline(c(7.0, 0.04), col=2); abline(sim.fit,col=3)

coef(sim.fit)
```


아래는 위의 작업을 100번 반복했다. 
```{r}
plot(1, xlim=c(0,300), ylim=c(0,30), type="n")
betas = numeric()
for(i in 1:100) {
  x = runif(100)*300
  y = 7.0 + 0.04*x + rnorm(100, 0, 3.259)
  sim.fit = lm(y~x)
  abline(sim.fit,col=i)
  betas = rbind(betas, coef(sim.fit))
}
betas = data.frame(betas)
summary(betas)
sapply(betas, sd)
hist(betas)
```


### 회귀직선 그리기

앞서 구한 회귀직선을 그린다.

```{r}
attach(adv)
plot(tv, sales);abline(lm.fit, lwd=3, col="red")
detach(adv)
```


### 다중회귀모형의 적합

반응변수를 sales, 독립변수를 tv, radio, newspaper로 하는 다중회귀분석을 진행한다.

```{r}
lm.fit = lm(sales ~ tv + radio + newspaper, data=adv)
summary(lm.fit)
```


## 신용카드자료

이번엔 신용카드자료를 사용하여 분석을 진행한다.

### 자료 읽기

```{r}
credit = read.csv("Credit.csv", header=T, sep=",")
credit = credit[,-1]
names(credit) = tolower(names(credit))
head(credit)
```


### 자료 탐색

다음의 코드들을 통해 탐색적 자료분석을 진행한다.

```{r}
# library(Hmisc)
summary(credit)
Hmisc::describe(credit)
psych::describe(credit)  
sapply(credit[,-(7:10)], sd)
hist(credit)
#pairs(credit)  # 자료형이 수치형인 변수에 대해서만 실행가능
cor(credit[,-(7:10)])
ggpairs(credit)
ggduo(credit, columnsX = 7:8, columnsY = 1:3) 
ggduo(credit, columnsX = 7:8, columnsY = 11:11) 
```


### 가변수

독립변수가 범주형 변수일 경우에는 가변수를 사용하여 분석한다.

신용카드자료에서 gender와 ethnicity는 범주형 변수이며, 이를 사용하여 단순선형회귀분석을 진행한다.

```{r}
lm.fit1 = lm(balance ~ gender, data = credit)
summary(lm.fit1)
lm.fit2 = lm(balance ~ ethnicity, data = credit)
summary(lm.fit2)
```


### 교호작용

```{r}
# lm.fit = lm(sales ~ tv*radio, data=adv)
lm.fit = lm(sales ~ tv+radio+tv:radio, data=adv)
summary(lm.fit)
```


### 변수선택

변수를 선택하는 데에는 여러가지 방법이 존재한다.

해당 분석에서는 광고자료를 사용하여 변수선택을 선보인다.

```{r}
library(leaps)
regfit.full=regsubsets(sales~.,data=adv, method="exhaustive")
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$cp
reg.summary$bic

regfit.forward=regsubsets(sales~.,data=adv, method="forward")
summary(regfit.forward)
```


#### 교차검증을 이용한 변수선택

```{r}
k=10
set.seed(1)
folds=sample(1:k,nrow(adv),replace=TRUE)
cv.errors=matrix(NA,k,3, dimnames=list(NULL, paste(1:3)))
```


#### predict 함수의 작성

```{r}
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
```


#### 교차 검증

```{r}
for(j in 1:k){
  best.fit=regsubsets(sales~.,data=adv[folds!=j,],nvmax=3)
  for(i in 1:3){
    pred=predict(best.fit,adv[folds==j,],id=i)
    cv.errors[j,i]=mean( (adv$sales[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
plot(mean.cv.errors,type='b')
which.min(mean.cv.errors)

```


#### 최적모형

```{r}
reg.best=regsubsets(sales~., data=adv, nvmax=3)
coef(reg.best,2)
```
