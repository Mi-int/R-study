## 스탠 설치

라이브러리의 호환성 문제로 인하여 맥과 윈도우의 설치코드가 다르다.

따라서 사용하는 PC의 운영체제에 따라 코드를 적절히 실행한다.


### rstan 설치

```{r, eval = F, echo = F}
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

pkgbuild::has_build_tools(debug = TRUE)

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)

if(grepl("^darwin", R.version$os)){
  cat("\nCXX14FLAGS += -O3 -mtune=native -arch x86_64 -ftemplate-depth-256", file = M, sep = "\n", append = FALSE)
}else if(.Platform$OS.type == "windows"){
  cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
      "CXX11FLAGS=-O3 -march=native -mtune=native",
      file = M, sep = "\n", append = FALSE)
}else{
  cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
      "CXX14FLAGS += -fPIC", file = M, sep = "\n", append = FALSE)
}
```


윈도우 PC는 경우에 따라 에러가 발생할 수 있다.

해당 파일의 rstan 코드를 모두 실행해본 후, 에러가 있으면 제공된 pdf을 따라하여 에러를 해결한다.


## 패키지 로딩

```{r, echo = F, include = F}
library(ggplot2)
library(GGally) #ggpairs
library(dplyr)
library(rstan)
library(ggmosaic) #geom_mosaic
library(psych) #describe
library(reshape2)
```


## 스탠 설정

rstan을 실행하기에 앞서, 기본적으로 설정해야 할 코드이다.

해당 코드를 실행하지 않으면, rstan 작동에 있어 오류가 발생할 수 있다.
```{r}
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
```


## 압정 자료 분석

### 데이터

```{r}
x = 7
n = 10
data = list(x=x, n=n)
```


### 스탠 코드

```{r, echo = F}
pin.code ="
data {
  // data
  int<lower=0> x;
  int<lower=0> n;
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  x ~ binomial(n, theta);
  theta ~ uniform(0,1);
}
"

pin.fit = stan(model_code=pin.code, data=data, 
               seed=1234567, chains=4, iter=2000, thin=1)
```


### 결과 확인

```{r}
print(pin.fit)

plot(pin.fit, plotfun="plot")
plot(pin.fit, plotfun="dens")
plot(pin.fit, plotfun="hist")
plot(pin.fit, plotfun="trace")
plot(pin.fit, plotfun="ac", pars="theta")
```


## 쥐종양 자료 : 독립모형

### 데이터 

```{r}
load("rats.RData")

x = rats$x[71]
n = rats$n[71]
data = list(x=x, n=n) 
```

## 데이터 탐색 
```{r}
summary(rats)
Hmisc::describe(rats)
psych::describe(rats)
ggpairs(rats)
```


### 스탠 코드

```{r, echo = F}
rats1 ="
data {
  // data
  int<lower=0> x;
  int<lower=0> n;
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  x ~ binomial(n, theta);
  theta ~ uniform(0,1);
}
"

fit1 = stan(model_code=rats1, data=data, 
            seed=1234567, chains=4, iter=2000, thin=1)
```


### 결과

```{r}
print(fit1)

plot(fit1, plotfun="plot")
plot(fit1, plotfun="dens")
plot(fit1, plotfun="hist")
plot(fit1, plotfun="trace")
plot(fit1, pars=c("theta"), plotfun="ac")
```


## 쥐종양 자료 : 통합모형

### 데이터

```{r}
x = sum(rats$x)
n = sum(rats$n)
data = list(x=x, n=n) 
```


### 스탠 코드

```{r, echo = F}
rats2 ="
data {
  // data
  int<lower=0> x;
  int<lower=0> n;
}

parameters {
  real<lower=0, upper=1> theta;
}

model {
  x ~ binomial(n, theta);
  theta ~ uniform(0,1);
}
"

fit2 = stan(model_code=rats2, data=data, 
            seed=1234567, chains=4, iter=2000, thin=1)
```


### 결과

```{r}
print(fit2)

plot(fit2, plotfun="plot")
plot(fit2, plotfun="dens")
plot(fit2, plotfun="hist")
plot(fit2, plotfun="trace")
plot(fit2, pars =c("theta"), plotfun="ac")
```


## 쥐종양 자료: 계층모형

### 데이터

```{r}
x = rats$x
n = rats$n
k = length(x)
data = list(x=x, n=n, k=k) 
```

### 스탠 코드

```{r, echo = F}
rats3 ="
data {
  // data
  int<lower=0> k;
  int<lower=0> x[k];
  int<lower=0> n[k];
}

parameters {
  real<lower=0, upper=1> theta[k];
  
  real<lower=0, upper=1> mu;
  real nu;
}

transformed parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  
  alpha = mu*exp(nu);
  beta = (1-mu)*exp(nu);
}

model {
  for(i in 1:k) {
    x[i] ~ binomial(n[i], theta[i]);
    theta[i] ~ beta(alpha, beta);
  }
  mu ~ uniform(0,1); 
  nu ~ logistic(0, 1);
}
"

fit3 = stan(model_code=rats3, data=data, 
            seed=1234567, chains=2, iter=20000, thin=2)
```


### 결과

```{r}
print(fit3)

plot(fit3, plotfun="plot", pars=c("theta"))
plot(fit3, plotfun="plot", pars=c("alpha", "beta"))
plot(fit3, plotfun="dens", pars=c("theta[71]", "alpha", "beta"))
plot(fit3, plotfun="hist", pars=c("theta[71]", "alpha", "beta"))
plot(fit3, plotfun="trace", pars=c("theta[71]", "alpha", "beta"))
plot(fit3, plotfun="ac", pars=c("theta[71]", "alpha", "beta"))
```


## 8개 학교 비교

### 데이터

```{r}
schools = 
  data.frame(y = c(28., 8., -3., 7., -1., 1., 18., 12.), 
             s =c(15., 10., 16., 11., 9., 11., 10., 18.), 
             index=c("A","B","C","D","E","F","G", "H") )
schools
```


### 데이터 탐색
```{r}
summary(schools)
Hmisc::describe(schools)
psych::describe(schools)
ggpairs(schools)
```


### 스탠코드(독립모형)

```{r, echo = F}
eightschools_indep = "
data {
  int<lower=0> J;         // number of schools 
  real y[J];              // estimated treatment effects
  real<lower=0> sigma[J]; // standard error of effect estimates 
}
parameters {
  vector[J] theta;          // unscaled deviation from mu by school
}
model {
  target += normal_lpdf(y | theta, sigma); // log-likelihood
}
"

data = list(J = dim(schools)[1], y=schools$y, sigma = schools$s)

fit.indep = 
  stan(model_code=eightschools_indep, data=data, 
       seed=1234567, chains=1, iter=2000, thin=10, algorithm="NUTS")
```

#### 결과

```{r}
print(fit.indep)
plot(fit.indep, plotfun="plot")
plot(fit.indep, plotfun="plot")
plot(fit.indep, plotfun="dens")
plot(fit.indep, plotfun="hist")
plot(fit.indep, plotfun="trace")
plot(fit.indep, pars =c("theta"), plotfun="ac")
```

### 스탠코드(통합모형)

```{r, echo = F}
eightschools_pooled = "
data {
  int<lower=0> J;         // number of schools 
  real y[J];              // estimated treatment effects
  real<lower=0> sigma[J]; // standard error of effect estimates 
}
parameters {
  real theta;          // 
}
model {
  for(j in 1:J) {
      y[j] ~ normal(theta, sigma[j]);
  }
}
"

data = list(J = dim(schools)[1], y=schools$y, sigma = schools$s)

fit.pooled = stan(model_code=eightschools_pooled, data=data,
                  seed=1234567, chains=1, iter=2000, 
                  thin=10, algorithm="NUTS")
```

#### 결과

```{r}
print(fit.pooled)

plot(fit.pooled, plotfun="plot")
plot(fit.pooled, plotfun="plot")
plot(fit.pooled, plotfun="dens")
plot(fit.pooled, plotfun="hist")
plot(fit.pooled, plotfun="trace")
plot(fit.pooled, pars=c("theta"), plotfun="ac")
```


### 스탠코드(계층모형)

```{r, echo = F}
eightschools_hier = "
data {
  int<lower=0> J;         // number of schools 
  real y[J];              // estimated treatment effects
  real<lower=0> sigma[J]; // standard error of effect estimates 
}
parameters {
  real mu;                // population treatment effect
  real<lower=0> tau;      // standard deviation in treatment effects
  vector[J] eta;          // unscaled deviation from mu by school
}
transformed parameters {
  vector[J] theta = mu + tau * eta;        // school treatment effects
}
model {
  target += normal_lpdf(eta | 0, 1);       // prior log-density
  target += normal_lpdf(y | theta, sigma); // log-likelihood
}
"

data = list(J = dim(schools)[1], y=schools$y, sigma = schools$s)

fit.hier = stan(model_code=eightschools_hier, data=data, 
                seed=1234567, chains=2, iter=2000, 
                thin=2, algorithm="NUTS")
```


#### 결과

```{r}
print(fit.hier)

plot(fit.hier, plotfun="plot")
plot(fit.hier, plotfun="plot")
plot(fit.hier, plotfun="dens")
plot(fit.hier, pars=c("mu"), plotfun="dens")
plot(fit.hier, plotfun="hist")
plot(fit.hier, plotfun="trace")
plot(fit.hier, pars=c("theta"), plotfun="ac")
```
