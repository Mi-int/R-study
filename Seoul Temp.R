```{r}
##  데이터 기입
library(ggplot2)
dat2 <- read.csv("data2.csv")
dat23 <- read.csv("data23csv.csv")

head(dat2)

### 습도와 기온 산점도
ggplot(dat2, aes(x=rh, y=oc)) + geom_point(shape=1) + geom_smooth(method=lm, color="red")

### 상관계수
cor.test(dat2$rh,dat2$oc)

```

```{r}
### 강수량과 기온 산점도
ggplot(dat2, aes(x=mm, y=oc)) + geom_point(shape=1) + geom_smooth(method=lm, color="red")

### 상관계수
cor.test(dat2$mm,dat2$oc)
```

```{r}

### 변수간 상관관계확인

library(corrplot)

corrplot(cor(dat2[, c(2:4)]))
corrplot(cor(dat2[, c(2:4)]), method= "number")

```

```{r}
## 도시별 분석

library(lattice)

a <- dat23$oc
b <- dat23$rh
c <- dat23$mm

xyplot(b ~ a | dat23$lv, panel = function(x,y, ...){panel.xyplot(x,y, ...)
panel.lmline(x,y, col=2)})

xyplot(c ~ a | dat23$lv, panel = function(x,y, ...){panel.xyplot(x,y, ...)
panel.lmline(x,y, col=2)})

xyplot(b ~ c | dat23$lv, panel = function(x,y, ...){panel.xyplot(x,y, ...)
panel.lmline(x,y, col=2)})
```

```{r}

seouloc <- ts(dat2$oc, start=c(2012, 1), frequency = 12)
str(seouloc)
plot.ts(seouloc, ylab = "", xlab = "Date", main = "seoul_oc")


seoulmm <- ts(dat2$mm, start=c(2012, 1), frequency = 12)
str(seoulmm)
plot.ts(seoulmm, ylab = "", xlab = "Date", main = "seoul_mm")

seoulrh <- ts(dat2$rh, start=c(2012, 1), frequency = 12)
str(seoulrh)
plot.ts(seoulrh, ylab = "", xlab = "Date", main = "seoul_rh")

```

```{r}
### MA 도출

library(forecast)

oc_ma<-ma(seouloc,120)
plot(oc_ma,ylim=c(0,30),col="red",lwd=5)

mm_ma<-ma(seoulmm,120)
plot(mm_ma,ylim=c(10,330),col="red",lwd=5)

rh_ma<-ma(seoulrh,120)
plot(rh_ma,ylim=c(40,80),col="red",lwd=5)
```


```{r}
seoul_temperature_dat <- read.csv("1992_2021_seoul_temperature.CSV")
seoul_temperature_dat


seoul_temperature <- ts(seoul_temperature_dat, start=c(1992, 1), frequency = 12)
str(seoul_temperature)
plot.ts(seoul_temperature, ylab = "", xlab = "Date", main = "seoul_temperature")

# log변환 생략
# log_seoul_temperature <- log(seoul_temperature)
# plot.ts(log_seoul_temperature, ylab = "", xlab = "Date", main = "seoul_temperature")

seoul_temperature_decompose <- decompose(seoul_temperature, type = "additive")
plot(seoul_temperature_decompose)

seoul_temperature_notrend <- seoul_temperature - seoul_temperature_decompose$trend

plot(seoul_temperature, main = "seoul_temperature") # 추세 있고
plot(seoul_temperature_notrend, main = "seoul_temperature_notrend") # 추세제거 그래프

# <로그변환 & 차분>
# 로그변환 생략
# log_seoul_temperature <- log(seoul_temperature)

# (일)차분
diff_seoul_temperature <- diff(seoul_temperature)

# acf를 통해서 ARIMA 모형에서 pdq를 설정해줘야 한다
acf(diff_seoul_temperature, main = "diff_seoul_temperature", lag.max = 360) # 로그변환&차분 acf

# 절단값 : 2 AR(p) p값=1
pacf(diff_seoul_temperature, main = "diff_seoul_temperature", lag.max = 360) # 로그변환&차분 pacf

library(tseries)
# 귀무가설 : 비정상
# 대립가설 : 정상
# p-value 값이 0.01로 0.05보다 작기때문에 유의해서 귀무가설 기각하므로 대립가설인
# 정상시계열이다. 라고 볼 수 있다. 
adf.test(diff_seoul_temperature, alternative = "stationary") # 차분하면 정상시계열

# <ARIMA>
# [acf와 pacf를 통한 직관적 p,d,q 선택]
library(forecast)
# arima 함수안에 원데이터 보단 변동성 제거한 데이터를 넣었다(원데이터 넣어도 ok)
# c(1, 1, 1) -> acf와 pacf를 봤을 때 1과 1로 결정을 했으니까, d값은 차분을
# 한 번 한다는 의미로 1로 설정
# aic값으로 어떤 모델이 더 좋은지 비교 가능
# 내가 설정한 pdq값으로 만든 모델과 오토 아리마를 통해 만든 모델 중 
# 뭐가 더 나은지 aic 값으로 비교

# [auto.arima를 통한 p,d,q 선택]
auto.arima(seoul_temperature)
fit <- arima(seoul_temperature, c(1, 0, 1),
             seasonal = list(order=c(1, 1, 0), period=12))
fit # aic: 작을수록 좋다
seoul_temperature_forecast <- forecast(fit, h=12*5) #12개월*5년=60
plot(seoul_temperature_forecast)

# <직관적 예측 검증>
# 데이터 중 108개
# index로만 가져오면 시계열 함수가 풀리기 때문에 다시 한번 ts함수를 써준다
seoul_temperature_tr <- ts(seoul_temperature[1:(12*20)], start=c(1992,1), frequency=12)
# log_seoul_temperature_tr <- log(seoul_temperature_tr)

auto.arima(seoul_temperature_tr)

fit_tr <- arima(seoul_temperature_tr, c(1, 0, 1),
                seasonal = list(order=c(1, 1, 0), period=12))

seoul_temperature_forecast_tr <- forecast(fit_tr, h=12*10)
plot(seoul_temperature_forecast_tr)
