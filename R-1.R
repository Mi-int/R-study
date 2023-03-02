## ----error=TRUE-----------------------------------------------
scan("z1.txt")
scan("z2.txt")
scan("z3.txt")
### 문자는 읽지 못해서 "" 적용
scan("z3.txt",what="")



## -------------------------------------------------------------
scan("z4.txt", what="")
scan("z3.txt",what="")
### 스페이스바 까지 같이 문자로 엮고싶을 때
scan("z3.txt",what="",sep="\n")


## -------------------------------------------------------------
x <- 1:3
print(x^2)
print("abc")


## -------------------------------------------------------------
### 옵션을 추가해서 여러개를 묶어서 보여줄 수도 있음
cat("abc\n")
### \n 을 적용하면 엔터 키가 적용되서 출력됨
cat("abc\n", "de")
### \t을 적용하면 탭 키가 적영되서 출력됨
cat("abc\t", "de")

cat(x, "abc", "de\n")
### sep="" 사이에 공백을 보이지 않고 출력
cat(x, "abc", "de\n",sep="")
### sep="," 사이에 공백을 ,로 바꿔 출력
cat(x, "abc", "de\n",sep=",")



## -------------------------------------------------------------
### R에서 csv, txt 파일 저장
id=c(1,2,3)
name=c('Mr.Foo', 'Ms.Bar','Mr. Baz')
score=c(95,97,92)
a=data.frame(id,name,score)
#### 데이터 저장
write.csv(a, file='a.csv')
#### 맨 앞 숫자값 X (첫줄 header 제거)
write.csv(a, file='a2.csv',row.names=FALSE)
#### " 전체제거
write.table(a, quote=FALSE, sep=',',file='a3.csv', 
            row.names=FALSE)
write.table(a, quote=FALSE, file='a4.txt',
            row.names=FALSE, sep='\t')
### write.csv는 sep=""를 쓸수 없다.


## -------------------------------------------------------------
### 외부 파일 불러들이기
x <- read.csv("a2.csv")
x
str(x)



## -------------------------------------------------------------
### 맨 윗줄 제거 (첫줄 header 제거)
write.table(a, quote=FALSE, sep=',',file='b.csv', 
            row.names=FALSE,col.names=FALSE)

y <- read.csv("b.csv", header=FALSE)
y


## -------------------------------------------------------------
### header에 원하는 값 입력
colnames(y)
colnames(y) <- c('id','name','score')
y


## -------------------------------------------------------------
### 문자열에 chr 대신 요인 (factor) 변수형으로 파일을 읽기 위해서
### stringsAsFactors=TRUE 옵션을 준다.
z <- read.csv("a2.csv", stringsAsFactors=TRUE)
str(z)


## -------------------------------------------------------------
#여러개의 R 오브젝트를 하나의 파일로 저장해보자
b=list(a=1,b=TRUE, c='oops')
save(a, b, file='xy.RData')
#load('xy.RData')

##----------------------------------------------------
### history에서 값을 선택해서 지우고싶을때
x=1
rm(x)

## ---- fig.align = "center",echo=FALSE,out.width="60%"---------
include_graphics("./figure/fig_uci.png")



## -------------------------------------------------------------
### 인터넷에서 바로 데이터 불러들이기
addr <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
w = read.csv(addr,header=TRUE, sep=";")
w[1:6,1:3]


## ----out.width= "2.1in"---------------------------------------
### for 문
e=rnorm(100)
n=length(e)
### 0을 n번 반복
s=rep(0,n)
### 1~n까지 for문을 반복 실행
for  (i  in  (1:n)){
  s[i]=sum(e[1:i])
}
t=1:n 
plot(t,s)


## -------------------------------------------------------------
### while 문
b=c(1,5,8,-1,0,2)
counter = 0
isPositive = TRUE
while(isPositive){
  counter=counter+1
  isPositive= (b[counter] >0)
}
### cat=실행해라 paste=붙여서
cat(paste("A negative number is detected at the ", 
          counter, "th place.\n"))



## -------------------------------------------------------------
### val==3 3회에서 참이면 if문을 실행시켜라
### break 실행중지
### 해당은 3회에서 실행이 중지되었으므로 2까지 실행된것

x <- 1:5
for (val in x) {
    if (val == 3){
        break
    }
    print(val)
}




## -------------------------------------------------------------
x <- 1:5

for (val in x) {
    if (val == 3){
        next
    }
    print(val)
}


## -------------------------------------------------------------
### 예제 1
x=3
if(x>1){
  y=1 
}else{
  y=-1
}
y
y=ifelse(x>1,1,-1)
y


## -------------------------------------------------------------
x=runif(100); n=length(x); argm=1; m=x[1]
for  (i in (2:n)){
  if(m<x[i]){
    m=x[i]
    argm=i
    }
}
c(argm,m)


## -------------------------------------------------------------
myvector=c(1,1,1,2,2,2,2)
mean(myvector) # 내장함수
myfunction <- function(x) { return(20 + (x*x)) } 
# 사용자정의 함수
myfunction(10)
myfunction(25)


## ---- error=TRUE----------------------------------------------
fish(myvector) # 미정의 함수


## -------------------------------------------------------------
MyMode  <- function(myVector)
{
return(myVector)
}


## -------------------------------------------------------------
tinyData  <-  c(1,2,1,2,2,3,3,4,5,4,5)
tinyData
MyMode(tinyData)


## -------------------------------------------------------------
MyMode  <- function(myVector)
  {
  uniqueValues <- unique(myVector)
  return(uniqueValues)
  }

MyMode(tinyData)


## -------------------------------------------------------------
MyMode <- function(myVector)
  {
  uniqueValues <- unique(myVector)
  uniqueCounts <- tabulate(myVector)
  return(uniqueCounts)
  }

MyMode(tinyData)


## -------------------------------------------------------------
MyMode <- function(myVector)
  {
  uniqueValues <- unique(myVector)
  uniqueCounts <- tabulate(myVector)
  return(uniqueValues[which.max(uniqueCounts)])
  }

MyMode(tinyData)


## -------------------------------------------------------------
tinyData<-c(tinyData,5,5,5)
tinyData

MyMode(tinyData)


## -------------------------------------------------------------
tinyData<-c(tinyData,1,1,1)
tinyData

MyMode(tinyData)

tinyData<-c(tinyData,9,9,9,9,9,9,9)
MyMode(tinyData)


## -------------------------------------------------------------
tabulate(tinyData)

unique(tinyData)


## -------------------------------------------------------------
tinyData

### match가 uniquevalues의 값을 위치로 매칭시켜줌
match(tinyData,unique(tinyData))

MyMode <- function(myVector)
  {
  uniqueValues <- unique(myVector)
  uniqueCounts <- tabulate(match(myVector,uniqueValues))
  return(uniqueValues[which.max(uniqueCounts)])
  }


## -------------------------------------------------------------
MyMode(tinyData) # done!


## -------------------------------------------------------------
mdata=c(1,2,1,3,4,9,5)
which.max(mdata)
mdata=c(mdata,9)
which.max(mdata)


## -------------------------------------------------------------
mywhich.max=function(x,val=FALSE,all=FALSE){
  n=length(x)
  ind=1
  m=x[1]
  for(i in 2:n){
    if(m < x[i]){m=x[i]; ind=i}
  }
  all.ind=(1:n)[x==m]
  if(val==TRUE){
    if(all==TRUE){return(list(max.ind=all.ind, max.val=m))
      }else{
        return(list(max.ind=ind,max.val=m))}
  }else{
    if(all==TRUE){return(list(max.ind=all.ind))}else{
    return(list(max.ind=ind))} } }


## -------------------------------------------------------------
mywhich.max(mdata)
## 일치하는 모든 값을 볼 때
mywhich.max(mdata,all=TRUE)



## -------------------------------------------------------------
mywhich.max(mdata,val=TRUE)
## 일치하는 값을 출력할 때
mywhich.max(mdata,val=TRUE,all=TRUE)


## -------------------------------------------------------------
mdata2=rbind(mdata,mdata)
mywhich.max(mdata2)
mywhich.max(mdata2,all=TRUE)


## -------------------------------------------------------------

### stop 대신 warning을 사용할수도 있다.
mywhich.max=function(x,val=FALSE,all=FALSE){
  if (!is.vector(x)){
    stop('input is not a vector!')
  }
  n=length(x)
  ind=1
  m=x[1]
  for(i in 2:n){
    if(m < x[i]){m=x[i]; ind=i}
  }
  all.ind=(1:n)[x==m]
  if(val==TRUE){
    if(all==TRUE){return(list(max.ind=all.ind, max.val=m))
      }else{
        return(list(max.ind=ind,max.val=m))}
  }else{
    if(all==TRUE){return(list(max.ind=all.ind))}else{
    return(list(max.ind=ind))} } }


## ----error=TRUE-----------------------------------------------
mywhich.max(mdata2)


## -------------------------------------------------------------
f1 =function(a,b) return(a+b)
f2 =function(a,b) return(a-b)
g=function(h,a,b) h(a,b)  #h는 함수
g(f1,3,2)
g(f2,3,2)


## ----error=TRUE-----------------------------------------------
g=function(){
  t=function(x){
    return(x^2)
  }
  return(t)
}
g(2)  # g ?뒗 ?엯?젰?쓣 諛쏆? ?븡?뒗 ?븿?닔
test.ft=g() # test.ft媛 g()?쓽 output?씤 t ?븿?닔 ?뿭?븷?쓣 ?븿 
test.ft(2)


## -------------------------------------------------------------
x <- runif(1000000)
y <- runif(1000000)
z1 <- c()
z1
z1 <- rep(0,1000000)
c()
z1 <- c()
z1 <- NULL

system.time(z1 <- x+y)

z2 <-c()
system.time(for(i in 1:length(x)){
  z2[i] <- x[i]+y[i]
})


## -------------------------------------------------------------
# 벡터에서 홀수의 갯수를 세는 code
oddcount1 <-function(x){
    c <- 0
    for(i in 1:length(x)){
      if(x[i] %%2 == 1) c<- c+1}
    return(c)
}
# 나머지가 1 인 것들을 나열
oddcount2 <- function(x){return(sum(x%%2 ==1))}



## -------------------------------------------------------------
x<- sample(1:1000000,100000,replace=T)

system.time(oddcount1(x))

system.time(oddcount2(x))


## ----warning = FALSE, message=FALSE, size='small'-------------
library(foreach)
library(doParallel)
# Set the number of cores to use
core_use=detectCores()-1
# Register a number of cores to use.
registerDoParallel(cores = core_use)
# number of repetition
rep=10
# Iterations begin for training
results=foreach(l=1:rep, .combine = rbind) %dopar% {
  sample = rnorm(1000)
  mean(sample)
}
results


## -------------------------------------------------------------

mean.parallel <- function(n){
  x <- runif(1000000)
  mx=mean(x)
  fname=paste('meanx', n, '.RD', sep='')
  save(mx,file=fname)
  return(n)
}
## 파일 이름은 공란이 있으면 안되므로 sep=" "으로 공란제거

## -------------------------------------------------------------
# Set the number of cores to use
core_use=detectCores()-1
# Register a number of cores to use.
registerDoParallel(cores = core_use)
# number of repetition
rep=20
# Iterations begin for jobs
results=foreach(l=1:rep, .combine = rbind) %dopar% {
  mean.parallel(l)
}



## -------------------------------------------------------------
xvec=rep(0,20)
for(i in 1:20){
  fname=paste('meanx', i, '.RD', sep='')
  load(fname)
  xvec[i]=mx
}
xvec

