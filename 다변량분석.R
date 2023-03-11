# 군집분석

## K평균 군집화 

### 랜덤 시드 생성
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)

### 첫 25개의 x1, x2값을 변경
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

plot(x)


km.out=kmeans(x,2,nstart=20)
## k = 2 인 군집화. 군집 개수는 2개
## nstart = 20으로 초기값 20개 설정

## 군집화 시각화
plot(x, col=(km.out$cluster+1),
main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

km.out

## 첫 25와 뒤 25개로 군집이 나뉨.


set.seed(4)
km.out=kmeans(x,3,nstart=20)

km.out

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="",
pch=20, cex=2)



## 초기값 1과 20의 비교.
### 초기값 1
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss

### 초기값 20
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

## 초기값 20이 내부제곱합이 더 작음. 초기값은 큰 값을 사용해야함.



# 계층 군집화

## dist(x)는 자료들 간의 거리를 구하는 함수
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

dist(x[1:4,])

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

## 군집의 인덱시는 cutree. k = 2 는 군집의 개수
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)


xsc=scale(x)
plot(hclust(dist(xsc), method="complete"),
main="Hierarchical Clustering with Scaled Features")


## 거리행렬 변환
x=matrix(rnorm(30*3), ncol=3)
dd = as.dist(1-cor(t(x)))

plot(hclust(dd, method="complete"),
main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")



# 주성분 분석

## 데이터 소개 : 미국 각 주 범죄 관련 자료
str(USArrests)
head(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)
summary(USArrests)

## scale-TRUE로 척도화 > 주성분분석 진행
pr.out=prcomp(USArrests, scale=TRUE)
str(pr.out)

## 각 변수의 평균
pr.out$center
## 각 변수의 표준편차
pr.out$scale
## 적재 벡터 확인
pr.out$rotation

dim(pr.out$x)
str(pr.out$x)

## 그래프 그리기
biplot(pr.out, scale=0)

## 주성분의 사인 변경
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

## 주성분의 표준편차
pr.out$sdev

## 주성분의 분산
pr.var=pr.out$sdev^2
pr.var

## 주성분의 설명 비율
pve=pr.var/sum(pr.var)
pve

## 분산 비율 그림
plot(pve, xlab="Principal Component",
ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')

## 누적 분산 비율 그림
plot(cumsum(pve), xlab="Principal Component",
ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


# 인자 분석
scores <- factanal(life, factors = 3, method = "mle",
scores = "regression")$scores
cex <- 0.8
plot(scores[,1], scores[,2], type = "n", xlab = "Factor 1",
ylab = "Factor 2")
text(scores[,1], scores[,2], abbreviate(rownames(life), 5),
cex = cex)
plot(scores[,1], scores[,3], type = "n", xlab = "Factor 1",
ylab = "Factor 3")
text(scores[,1], scores[,3], abbreviate(rownames(life), 5),
cex = cex)
plot(scores[,2], scores[,3], type = "n", xlab = "Factor 2",
ylab = "Factor 3")
text(scores[,2], scores[,3], abbreviate(rownames(life), 5),
cex = cex)

## psych package 이용
library(psych)
library(GPArotation)

head(bfi, n = 3)

### NA값 삭제
fa_fit <- fa(
  r = na.omit(bfi[,1:25]),
    nfactors = 5,
    fm= 'ml',
    max.iter = 100,
    rotate = 'varimax',
    scores = 'regression'
    )


fa_fit$loadings

head(fa_fit$scores)

### 인자 분석 도식화
fa.diagram(fa_fit)

loadings <- data.frame(fa_fit$loadings[,paste0('ML',1:5)],
  category = substr(rownames(fa_fit$loadings),1,1))
  
plot(x = loadings$ML1,
    y = loadings$ML2,
    col = as.numeric(loadings$category),
    pch = as.numeric(loadings$category),
    xlab = 'Factor 1',
    ylab = 'Factor 2',
    main = "Factor 1 vs. Factor 2",
    cex = 3,
    cex.main = 3,
    cex.axis = 2,
    cex.lab = 2)
legend('topright',
      legend = unique(as.character(loadings$category)),
      title = 'Personality items',
      col = unique(as.numeric(loadings$category)),
      pch = unique(as.numeric(loadings$category)),
      cex = 2)

### 로딩 값을 이용한 변수 군집화
thres <- function(x, cutoff){x[abs(x) <= cutoff] <- 0; return(x)}
set.seed(10)
kmeans_fit <- kmeans(thres(fa_fit$loadings[,colnames(fa_fit$loadings)], 0.1), centers = 2)
ftable(substr(names(kmeans_fit$cluster,1,1), kmeans_fit$cluster))

### 스코어 값을 이용한 표본 군집화
library(ggplot2)

set.seed(10)
fa_kmeans_fit <- kmeans(fa_fit$scores, centers = 3, iter.max = 100, nstart = 25)

## 시각화
pca_fit <- prcomp(na.omit(bfi[.1:25]))
ggplot(data = data.frame(pca_fit$x[,1:2], cluster = as.factor(fa_kmeans_fit$cluster)),mapping = aes(x = PC1, y = PC2, color = cluster)) + geom_point(size = 0.7)

## 시각화
set.seed(10)
pca_kmeans_fit <- kmeans(pca_fit$x[,1:5], centers = 3, iter.max = 100, nstart = 25)
ggplot(data = data.frame(pca_fit$x[,1:2], cluster = as.factor(pca_kmeans_fit$cluster),mapping = aes(x = PC1, y = PC2, color = cluster))) + geom_point(size = 0.7)
