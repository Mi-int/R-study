
# - 1. 확률변수 그래프 그리기       
# - 2. 시계열 데이터 그래프 그리기         
# - 3. 순수확률과정(백색잡음과정)
# - 4. 이동평균과정(Moving Average Process)
# - 5. 자기회귀과정(Auto Regressive Process)
# - 6. 잡음이 섞인 신호
# - 7. Binary Process
# - 8. Random Walk

# < Examples of Stochastic Process > 
# - 1. Binary Process
# - 2. Random Walk 
#  * MA(Moving Average)를 이용하여 Random walk의 평활 그래프 그리기

# < Continuous Probability Distribution >
# - 1. Uniform Distribution
# - 1. Normal Distribution

# < Discrete Probability Distribution >
# - 1. Uniform Distribution

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------


# 1. 확률변수 그래프 그리기


# 객체 생성
# ex) x<-7 또는 x=7
# 단축키 ; alt + -

# 코드 실행 ; Ctrl + Enter 또는 Run

# rnorm ; 정규분포에서 난수생성하는 함수
# rnorm(n=표본수, mean=평균,sd=표준편차)

# hist ; 히스토그램을 그려주는 함수
# hist(x=데이터, main = 제목)

# sqrt ; SQUARE ROOT, 제곱근을 구해주는 함수


#-----------------------------------------------------------------------------------------------


# 1-1. n=10일 때


# 1) N(O,1)
A_10<-rnorm(n=10, mean=0, sd=sqrt(1))
hist(A_10,main="n=10, X~N(0,1)")

# 2) N(0,5)
B_10<-rnorm(n=10, mean=0, sd=sqrt(5))
hist(B_10,main="n=10, X~N(0,5)")

# 3) N(O,10)
C_10<-rnorm(n=10, mean=0, sd=sqrt(10))
hist(C_10,main = "n=10, X~N(O,10)")


#-----------------------------------------------------------------------------------------------


# 1-2. n=50일 때


# 1) N(0,1)
A_50<-rnorm(n=50, mean=0, sd=sqrt(1))
hist(A_50, main="n=50, X~N(0,1)")

# 2) N(0,5)
B_50<-rnorm(n=50, mean=0, sd=sqrt(5))
hist(B_50, main="n=50, X~N(0,5)")

# 3) N(0,10)
C_50<-rnorm(n=50, mean=0, sd=sqrt(10))
hist(C_50, main="n=50, X~N(0,10)")



#-----------------------------------------------------------------------------------------------


# 1-3. n=100일 때


# 1) N(0,1)
A_100<-rnorm(n=100, mean=0, sd=sqrt(1))
hist(A_100, main="n=100, X~N(0,1)")

# 2) N(0,5)
B_100<-rnorm(n=100, mean=0, sd=sqrt(5))
hist(B_100, main="n=100, X~N(0,5)")

# 3) N(0,10)
C_100<-rnorm(n=100, mean=0, sd=sqrt(10))
hist(C_100, main="n=100, X~N(0,10)")


#-----------------------------------------------------------------------------------------------


# 1-4. n=1000일 때


# 1) N(0,1)
A_1000<-rnorm(1000, mean=0, sd=sqrt(1))
hist(A_1000,main="n=1000, X~N(0,1)")

# 2) N(0,5)
B_1000<-rnorm(1000, mean=0, sd=sqrt(5))
hist(B_1000,main="n=1000, X~N(0,5)")

# 3) N(0,10)
C_1000<-rnorm(1000, mean=0, sd=sqrt(10))
hist(C_1000,main ="n=1000, X~N(0,10)")


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------


# 2. 시계열 데이터 그래프 그리기


# ts ; 시계열 데이터 생성하는 함수
# ts(데이터, frequency=간격, start=시작일)

# plot ; 그래프를 그려주는 함수, 시계열 데이터 객체를 시계열 그래프로 나타낼 수 있음.
# plot(x,y)
# plot(시계열 객체, main = 제목)


#-----------------------------------------------------------------------------------------------


# 2-1. n=10일 때


# 1) N(0,1)
ts_A_10=ts(rnorm(n=10, mean=0, sd = sqrt(1)), frequency=1, start=1)
plot(ts_A_10, main= 'n=10, mean=0, var=1' )


# 2) N(0,5)
ts_B_10=ts(rnorm(n=10, mean=0, sd = sqrt(5)), frequency=1, start=1)
plot(ts_B_10, main= 'n=10, mean=0, var=5' )

# 3) N(0,10)
ts_C_10=ts(rnorm(n=10, mean=0, sd = sqrt(5)), frequency=1, start=1)
plot(ts_C_10, main= 'n=10, mean=0, var=10' )


#-----------------------------------------------------------------------------------------------


# 2-2. n=50일 때


# 1) N(0,1)
ts_A_50=ts(rnorm(n=50, mean=0, sd = sqrt(1)), frequency=1, start=1)
plot(ts_A_50, main= 'n=50, mean=0, var=1' )

# 2) N(0,5)
ts_B_50=ts(rnorm(n=50, mean=0, sd = sqrt(5)), frequency=1, start=1)
plot(ts_B_50, main= 'n=50, mean=0, var=5')

# 3) N(0,10)
ts_C_50=ts(rnorm(n=50, mean=0, sd = sqrt(10)), frequency=1, start=1)
plot(ts_C_50, main= 'n=50, mean=0, var=10')


#-----------------------------------------------------------------------------------------------


# 2-3. n=100일 때


# 1) N(0,1)
ts_A_100=ts(rnorm(n=100, mean=0, sd = sqrt(1)),frequency=1, start=1)
plot(ts_A_100, main= 'n=100, mean=0, var=1' )

# 2) N(0,5)
ts_B_100=ts(rnorm(n=100, mean=0, sd = sqrt(5)), frequency=1, start=1)
plot(ts_B_100, main= 'n=100, mean=0, var=5')

# 3) N(0,10)
ts_C_100=ts(rnorm(n=100, mean=0, sd = sqrt(10)), frequency=1, start=1)
plot(ts_C_100, main= 'n=100, mean=0, var=10')


#-----------------------------------------------------------------------------------------------


# 2-4. n=1000일 때


# 1) N(0,1)
ts_A_1000=ts(rnorm(n=1000, mean=0, sd = sqrt(1)),frequency=1, start=1)
plot(ts_A_1000, main= 'n=1000, mean=0, var=1' )

# 2) N(0,5)
ts_B_1000=ts(rnorm(n=1000, mean=0, sd = sqrt(5)), frequency=1, start=1)
plot(ts_B_1000, main= 'n=1000, mean=0, var=5')

# 3) N(0,10)
ts_C_1000=ts(rnorm(n=1000, mean=0, sd = sqrt(10)), frequency=1, start=1)
plot(ts_C_1000, main= 'n=1000, mean=0, var=10')


#-----------------------------------------------------------------------------------------------
# 동일한 정규분포에서 생성된 난수이지만,
# 확률변수와 시계열의 모양은 전혀 다름.

#--------------------------------------------

# 확률변수 그래프

a_10 <- runif(n = 10, min = 0 , max = 1)
his(a_10, main = "n=10 , min = 0, max = 1")


a_50 <- runif(n = 50 , min = 0 , max = 1)
hist(a_50 , main = " n=50, min=0, max=1")

a_100 <- runif(n=100, min=0, max=1)
hist(a_100)

a_1000 <- runif(n=1000, min=0, max=1)
hist(a_1000, main = "n= 1000, min=0, max=1")

a_50000 <- runif(n=50000, min=0, max=1)
hist(a_50000, main = "n= 50000, min=0, max=1")

# 시계열 데이터 그래프

ts_10 <- ts(runif(n=10,min=0, max=1), frequency = 1 , start =1)
plot(ts_10, main = "ts_10")


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# - 3. 순수확률과정(백색잡음과정)
# Wt~iid N(0,분산)


# Wt 생성   /  Wt~iid N(0,분산)
set.seed(1) # 난수 생성시, 재사용 가능
W<-ts(rnorm(n=10, mean=0, sd = sqrt(1)), frequency=1, start=1) # ts함수 이용하여 시계열 데이터 만들기

# Wt 그래프
plot(W, main= '순수확률과정(백색잡음과정)' )


#-----------------------------------------------------------------------------------------------
# n=100인경우


# Wt 생성   /  Wt~iid N(0,분산)
W<-ts(rnorm(n=100, mean=0, sd = sqrt(1)), frequency=1, start=1)

# Wt 그래프
plot(W, main= '순수확률과정(백색잡음과정)' )


#-----------------------------------------------------------------------------------------------
# n=100, 분산=10인 경우 → 분산 증가하여, 변동폭이 커짐


# Wt 생성   /  Wt~iid N(0,분산)
W<-ts(rnorm(n=100, mean=0, sd = sqrt(10)), frequency=1, start=1)

# Wt 그래프
plot(W, main= '순수확률과정(백색잡음과정)' )


#-----------------------------------------------------------------------------------------------
# n=100, 분산=1인경우 & n=100, 분산=10인경우


# Wt 생성   /  Wt~iid N(0,분산)
W_1<-ts(rnorm(n=100, mean=0, sd = sqrt(1)), frequency=1, start=1)
W_10<-ts(rnorm(n=100, mean=0, sd = sqrt(10)), frequency=1, start=1)
# Wt 그래프
plot(W_1, main= '순수확률과정(백색잡음과정)',
     ylim=c(min(min(W_1),min(W_10)), max(max(W_1),max(W_10))))
lines(W_10,col="blue")
legend("topright", legend = c("분산=1","분산=10"),
       lty = c(1,1),
       fill = c("black","blue"))


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# - 4. 이동평균과정(Moving Average Process )
# 백색잡음 그래프 위에 그려서 비교해보기
# Yt= (Wt-1 + Wt + Wt+1)/3 → zoo패키지의 rollmean 함수 이용


# 3-Point Moving Average Process 

# Wt 생성   /   Wt~iid N(0,분산)
set.seed(1)
W<-ts(rnorm(n=10, mean=0, sd = sqrt(1)), frequency=1, start=1)

# Wt 그래프
plot(W, main= '순수확률과정(백색잡음과정)' )

# Yt생성   /   Yt= (Wt-1 + Wt + Wt+1)÷3 → zoo패키지의 rollmean 함수 이용
library(zoo) # 이동평균계산을 위해 필요한 패키지
zoo_W <- zoo(W) # 'zoo'객체로 변환
MA_Yt<-rollmean(x = zoo_W, k = 3)  # k=3일 때,이동평균 계산

# Wt그래프와 Yt그래프
plot(W,main="순수확률과정 & 이동평균과정")   # Wt 그래프
lines(MA_Yt, col = 'blue')   # k=3로,이동평균한 그래프
legend("topleft", legend = c("백색잡음","k=3인 이동평균"),
       lty = c(1,1),
       fill = c("black","blue"))

#-----------------------------------------------------------------------------------------------
# 5-Point Moving Average Process
# n=100, k=5

# Wt 생성 /  Wt~iid N(0,분산)
W<-ts(rnorm(n=100, mean=0, sd = sqrt(1)), frequency=1, start=1)

# Wt 그래프
plot(W, main= '순수확률과정(백색잡음과정)' )

# Yt생성   /   Yt= (Wt-2 + Wt-1 + Wt + Wt+1 + Wt+2)÷5 → zoo패키지의 rollmean 함수 이용
library(zoo) # 이동평균계산을 위해 필요한 패키지
zoo_W<- zoo(W) # 'zoo'객체로 변환
MA_Yt<-rollmean(x = zoo_W, k = 5)  # k=5일 때,이동평균 계산

# Wt그래프와 Yt그래프
plot(W,main="순수확률과정 & 이동평균과정")    # Wt 그래프
lines(MA_Yt, col = 'blue')   # k=5로,이동평균한 그래프
legend("topleft", legend = c("백색잡음","k=5인 이동평균"),
       lty = c(1,1),
       fill = c("black","blue"))




#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# - 5. 자기회귀과정(Auto Regressive Process) → AR(1) : 하나의 시점에만 종속됨
# Xt=Xt-1 + Wt


# Wt 생성 /  Wt~iid N(0,분산)
set.seed(1) # 난수 생성
W<-ts(rnorm(n=10, mean=0, sd = sqrt(1)), frequency=1, start=1)


# Xt 생성   / Xt=Xt-1 + Wt
X=c() 

for(t in 2:10){
  X[1]=W[1]
  X[t]=X[t-1]+W[t]
  print(X[t])
}

X  # Xt 값

plot(W,main="순수확률과정 & 자기회귀과정",ylim=c(min(min(W),min(X)), max(max(W),max(X))))
lines(X,col="red")

#-----------------------------------------------------------------------------------------------
# n=100


# Wt 생성 /  Wt~iid N(0,분산)
W<-ts(rnorm(n=100, mean=0, sd = sqrt(10)), frequency=1, start=1)


# Xt 생성   / Xt=Xt-1 + Wt
X=c() 
for(t in 2:100){
  X[1]=W[1]
  X[t]=X[t-1]+W[t]
  print(X[t])
}

X  # Xt 값

plot(W,main="순수확률과정 & 자기회귀과정", ylim=c(min(min(W),min(X)), max(max(W),max(X))))
lines(X,col="red")

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# - 6. 잡음이 섞인 신호
# Yt= S(t)+Wt


# S(t)=0.5t + 1.7
# 따라서, Yt= S(t)+Wt=0.5t + 1.7 + Wt

# Wt 생성  /  Wt~iid N(0,분산)
set.seed(1) # 난수 생성
W<-ts(rnorm(n=10, mean=0, sd = sqrt(1)), frequency=1, start=1)

# S(t)=0.5t + 1.7 생성
S=c()
for(t in 1:10){
  S[t]=(0.5)*t + 1.7
  print(S[t])

}

S

# Yt계산 / Yt= S(t) + Wt=  0.5t + 1.7 + Wt
Y=c()
for(t in 1:10){
  Y[t]=(0.5)*t+1.7+W[t]
  print(Y[t])
}
Y # Yt값

# 그래프
plot(W,main="백색잡음 & S(t) & 잡음이 섞인 신호 Yt",ylim=c(min(min(W),min(Y)),max(max(W),max(Y))) ) # 백색잡음
lines(S,col="blue") # 선형형태의 S(t)
lines(Y,col="red") # Yt
legend("topleft", legend = c("백색잡음","S(t)","Yt"),
       lty = c(1,1),
       fill = c("black","blue","red"))

#-----------------------------------------------------------------------------------------------
# n = 500

# Wt 생성  /   Wt~iid N(0,분산)
W<-ts(rnorm(n=500, mean=0, sd = sqrt(1)), frequency=1, start=1)

# S(t)=0.5t + 1.7 생성
S=c()
for(t in 1:500){
  S[t]=(0.5)*t + 1.7
  print(S[t])
  
}

# Yt계산
Y=c()
for(t in 1:500){
  Y[t]=(0.5)*t+1.7+W[t]
  print(Y[t])
}
Y # Yt값

# 그래프
plot(W,main="백색잡음 & S(t) & 잡음이 섞인 신호 Yt",ylim=c(min(min(W),min(Y)),max(max(W),max(Y))) ) # 백색잡음
lines(S,col="blue") # 선형형태의 S(t)
lines(Y,col="red") # Yt
legend("topleft", legend = c("백색잡음","S(t)","Yt"),
       lty = c(1,1),
       fill = c("black","blue","red"))

#-----------------------------------------------------------------------------------------------
# n=500, 분산 = 50

# Wt 생성  /  Wt~iid N(0,분산)
W<-ts(rnorm(n=500, mean=0, sd = sqrt(50)), frequency=1, start=1)

# S(t)=0.5t + 1.7 생성
S=c()
for(t in 1:500){
  S[t]=(0.5)*t + 1.7
  print(S[t])
  
}

# Yt계산
Y=c()
for(t in 1:500){
  Y[t]=(0.5)*t+1.7+W[t]
  print(Y[t])
}
Y # Yt값

# 그래프
plot(W,main="백색잡음 & S(t) & 잡음이 섞인 신호 Yt",ylim=c(min(min(W),min(Y)),max(max(W),max(Y))) ) # 백색잡음
lines(S,col="blue") # 선형형태의 S(t)
lines(Y,col="red") # Yt
legend("topleft", legend = c("백색잡음","S(t)","Yt"),
       lty = c(1,1),
       fill = c("black","blue","red"))



#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# < Examples of Stochastic Process > 


# [t시점이 10개인 경우] → 손으로 직접 계산하여 비교
# - 1. Binary Process
# Uniform 분포에서 난수생성
set.seed(1)
U_Data<-runif(n = 9, min = 0, max=1)
U_Data

# ifelse 함수를 사용하여 0.5보다 크면 1, 작으면 -1로 변환하는 과정
U_Data_Bin<- ifelse(U_Data > 0.5, 1,-1)
U_Data_Bin
U_Data_Bin<-c(0,U_Data_Bin) # t=0일 때의 X0=0값 지정(initial 값 지정)

#시각화 
plot(U_Data_Bin, xlab= 't', ylab= "Xt", main= 'Binary Process',col="blue",
     xaxt='n')
axis(side = 1,at=1:10,labels=as.character(0:9)) 

# - 2.  Random Walk
#cumsum 함수 : 누적합을 출력하는 함수
U_Data_Bin_RW <- cumsum(U_Data_Bin)
U_Data_Bin_RW 

#시각화
plot(U_Data_Bin_RW , type= 'l', ylim = c(min(U_Data_Bin_RW), max(U_Data_Bin_RW)),
     xlab= 't  ', ylab= 'St',
     main= 'Random walk', col='blue',
     xaxt='n')
axis(side = 1,at=1:10,labels=as.character(0:9)) 


#-----------------------------------------------------------------------------------------------
# [t시점이 100개인 경우]
# - 1. Binary Process

# Uniform 분포에서 난수생성
set.seed(2)
U_Data<-runif(n = 99, min = 0, max=1)
U_Data

# ifelse 함수를 사용하여 0.5보다 크면 1, 작으면 -1로 변환하는 과정
U_Data_Bin<- ifelse(U_Data > 0.5, 1,-1)
U_Data_Bin
U_Data_Bin<-c(0,U_Data_Bin) # t=0일 때의 X0=0값 지정(initial 값 지정)

#시각화 
plot(U_Data_Bin, xlab= 't', ylab= "Xt", main= 'Binary Process',col="blue",
     xaxt='n')
axis(side = 1,at=1:100,labels=as.character(0:99)) 

# - 2.  Random Walk
#cumsum 함수 : 누적합을 출력하는 함수
U_Data_Bin_RW <- cumsum(U_Data_Bin)
U_Data_Bin_RW 

#시각화
plot(U_Data_Bin_RW , type= 'l', ylim = c(min(U_Data_Bin_RW), max(U_Data_Bin_RW)),
     xlab= 't  ', ylab= 'St',
     main= 'Random walk', col='blue',
     xaxt='n')
axis(side = 1,at=1:100,labels=as.character(0:99)) 

#-----------------------------------------------------------------------------------------------
# [t시점이 1000개인 경우]
# - 1. Binary Process
# Uniform 분포에서 난수생성
set.seed(3)
U_Data<-runif(n = 999, min = 0, max=1)
U_Data

# ifelse 함수를 사용하여 0.5보다 크면 1, 작으면 -1로 변환하는 과정
U_Data_Bin<- ifelse(U_Data > 0.5, 1,-1)
U_Data_Bin
U_Data_Bin<-c(0,U_Data_Bin) # t=0일 때의 X0=0값 지정(initial 값 지정)

#시각화 
plot(U_Data_Bin, xlab= 't', ylab= "Xt", main= 'Binary Process',col="blue",
     xaxt='n')
axis(side = 1,at=1:1000,labels=as.character(0:999)) 


# - 2.  Random Walk
#cumsum 함수 : 누적합을 출력하는 함수

U_Data_Bin_RW <- cumsum(U_Data_Bin)
U_Data_Bin_RW 

#시각화
plot(U_Data_Bin_RW , type= 'l', ylim = c(min(U_Data_Bin_RW), max(U_Data_Bin_RW)),
     xlab= 't  ', ylab= 'St',
     main= 'Random walk', col='blue',
     xaxt='n')
axis(side = 1,at=1:1000,labels=as.character(0:999)) 

#  * MA(Moving Average)를 이용하여 Random walk의 평활 그래프 그리기
library(zoo)
zoo_U_Data_Bin_RW <- zoo(U_Data_Bin_RW)
MA_St<-rollmean(x = zoo_U_Data_Bin_RW, k = 5) # 5-Point Moving Average Process 
lines(MA_St, col = 'red') 

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# < Continuous Probability Distribution >
# - 1. Uniform Distribution

# [n=10인경우]  → 손으로 직접 계산하여 비교
# 난수발생
set.seed(11)
U_data<-runif(n=10,min =0,max=1)
U_data 
hist(U_data, freq = F)

# U_data * 48 → 값 변환
U_data_48<-U_data*48
U_data_48<-ceiling(U_data_48)
U_data_48


hist(U_data_48,breaks =24,freq = F) # freq = F 이면 확률밀도보여줌
axis(side = 1, at =seq(0,48, by=2))
#-----------------------------------------------------------------------------------------------
# [n=100인경우]
# 난수발생
set.seed(12)
U_data<-runif(n=100,min =0,max=1)
U_data
hist(U_data, freq = F)

# U_data * 48 → 값 변환
U_data_48<-U_data*48
U_data_48<-ceiling(U_data_48)
U_data_48


hist(U_data_48, breaks =24, freq = F) # freq = F 이면 확률밀도보여줌
axis(side = 1, at =seq(0,48, by=2))
#-----------------------------------------------------------------------------------------------
# [n=5000인경우]
# 난수발생
set.seed(13)
U_data<-runif(n=5000,min =0,max=1)
U_data
hist(U_data, freq = F)

# U_data * 48 → 값 변환
U_data_48<-U_data*48
U_data_48<-ceiling(U_data_48)
U_data_48

hist(U_data_48, breaks =24, freq = F) # freq = F 이면 확률밀도보여줌
axis(side = 1, at =seq(0,48, by=2))
#-----------------------------------------------------------------------------------------------
# [n=5000000인경우]
# 난수발생
set.seed(14)
U_data<-runif(n=5000000,min =0,max=1)
U_data 
hist(U_data, freq = F)

# U_data * 48 → 값 변환
U_data_48<-U_data*48
U_data_48<-ceiling(U_data_48)
U_data_48


hist(U_data_48,breaks =24,freq = F) # freq = F 이면 확률밀도보여줌
axis(side = 1, at =seq(0,48, by=2))
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# - 1. Normal Distribution, X~N(µ, σ^2)
# X~N(0,1)

# X값 생성
X<- seq(-5, 5, 0.1)

# dnorm ; 확률밀도함수 (Probability density function)
f_X1<-dnorm(X, mean=0, sd=sqrt(0.2))
f_X2<-dnorm(X, mean=0, sd=sqrt(1))
f_X3<-dnorm(X, mean=0, sd=sqrt(5))
f_X4<-dnorm(X, mean=-2, sd=sqrt(0.5))

# 그래프
plot(X, f_X1, type = "l",col="blue",xlim = c(-5,5))
lines(X,f_X2, type = "l",col="red")
lines(X,f_X3, type = "l",col="yellow")
lines(X,f_X4, type = "l",col="green")

# 범례
legend("topright",
       legend = c("µ=0, σ^2=0.2","µ=0, σ^2=1","µ=0, σ^2=5","µ=-2, σ^2=0.5"),
       fill = c("blue", "red","yellow","green"))

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# < Discrete Probability Distribution >
# - 1. Uniform Distribution

#pdf
set.seed(21)
x<-seq(-3,3,length.out=10)
x


# 구간 지정
a<-0 
b<-1


for(i in 1:10){
        if(x[i] >=0 & x[i]<=1){
                y[i]<-1/(b-a)
        }
        else { 
                y[i]<-0
        }
                }
plot(x,y,ylab="Probability",main="Discrete Uniform Distribution")

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

