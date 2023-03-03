
## `ggplot2`패키지 설치
install.packages("ggplot2")
library(ggplot2)
### aes : 다른 변수와 같이 결합시켜주는 함수

## `Seatbelts` 데이터
# 1969년부터 1984년까지 매달 영국에서 교통사고로 인한 운전자, 앞좌석 뒷자석 사상자 수 등을 기록
data(Seatbelts)
seatbelts <- data.frame(Seatbelts,
                        date = seq(from = as.Date("1969-01-01"), to = as.Date("1984-12-01"),
                                   by = 'month'))
str(seatbelts)
head(seatbelts)

## `iris` 데이터
data(iris)
str(iris)


################################################################################
##### Q1: 안전벨트 의무 착용 법안이 교통사고 사상자 감소에 도움이 되었는가? ####
################################################################################

## 산점도 그리기
ggplot(data = seatbelts) +
  geom_point(aes(x = date, y = DriversKilled)) +
  labs(title = "Number of Drivers Killed",
       subtitle = "UK, 1969 to 1984",
       x = "Date", y = "Number of deaths")

## 선 그래프 그리기
ggplot(data = seatbelts) +
  geom_line(aes(x = date, y = DriversKilled)) +
  labs(title = "Number of Drivers Killed",
       subtitle = "UK, 1969 to 1984",
       x = "Date", y = "Number of deaths")


## 선 그래프에 점 찍기, 크키/모양/색깔 바꾸기
ggplot(data = seatbelts, aes(x = date, y = DriversKilled)) +
  geom_line(linetype = 2, color = "red") +
  geom_point(size = 2) +
  labs(title = "Number of Drivers Killed",
       subtitle = "UK, 1969 to 1984",
       x = "Date", y = "Number of deaths")

## 여러 자료를 색으로 구분하기
ggplot(data = seatbelts, aes(x = date)) +
  ### y에 drivers 와 front 따로 만드므로 ggplot에 기입 X
  geom_line(aes(y = drivers, col = "red")) +
  geom_line(aes(y = front, col = "blue")) +
  labs(title = "Number of Road Casualties", 
       subtitle = "Killed or seriously injured", x = "Date", y = "Number") +
  ### geom line, aes에 "red"와 "blue"는 변수가 아니기 때문에 
  ### scale_color_identity를 기입해서 색을 지정함.
    scale_color_identity(name = "Seat Position", 
                       breaks = c("red", "blue"),
                       labels = c("drivers", "front"), 
                       guide = "legend")


## 안전벨트의 효과? 수직선 추가하기
ggplot(data = seatbelts, aes(x = date)) +
  geom_line(aes(y = drivers, col = "red")) +
  geom_line(aes(y = front, col = "blue")) +
  ### 수직선을 추가해 법안이 도입된 전과 후의 차이를 한눈에 보기 쉽게 함.
  geom_vline(xintercept = as.Date("1983-01-01"), size = 2, 
             color = "gray47", linetype = 2) +
  labs(title = "Was the Seatbelt Law Effective?",
       x = "Date", y = "Number of casualties") +
  scale_color_identity(name = "Seat Position", breaks = c("red", "blue"),
                       labels = c("drivers", "front"), guide = "legend")


################################################################################
############### Q2: 붓꽃의 품종에 따라 꽃의 크기에 차이가 있을까? ##############
################################################################################

## 데이터 시각화 : 꽃받침의 크기와 너비 산점도
ggplot(data = iris) + 
  geom_point(aes(x = Sepal.Length, y = Sepal.Width)) +
  labs(title = "Iris", x = "sepal_length", y = "sepal_width")


## 데이터 시각화 : 점과 색의 모양으로 품종 구별하기
ggplot(data = iris) + 
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 ### 색과 모양 지정
                 color = Species, shape = Species)) +
  labs(title = "Iris", x = "sepal_length", y = "sepal_width")


## 데이터 시각화 : 텍스트 추가하기
### 붙일 텍스트를 만듦. substr은 지정해준 인덱스만큼의 변수를 뽑음
names <- c("Korea", "Japan", "China")
substr(names,1,3)

iris$Species.abbr <- substr(iris$Species,1,2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species, shape = Species)) +
  geom_text(aes(label = Species.abbr), 
            #nudge_x = 점 옆에 텍스트를 붙이는 간격
            nudge_x = 0.1, size = 3) +
  labs(title = "Iris", x = "sepal_length", y = "sepal_width")


## 이산형 자료를 요약하는 방법
# 도수분포표
table(iris$Species)

iris$sw_rev <- 
  ### 3.2보다 작거나 같으면 small, 크면 large로 변환
  ifelse(iris$Sepal.Width <= 3.2, 
         "Small", "Large")
table(iris$Species, iris$sw_rev)

# 막대 그래프
ggplot(data = iris) +
  geom_bar(aes(x = Species, 
               y = Sepal.Width), 
           stat = "summary", 
           fun = "mean") +
  ### coord_cartesian() 막대그래프의 Y축 범위를 지정해줌.
  coord_cartesian(ylim = c(2, 3.5))


## 연속형 자료를 요약하는 방법
# 히스토그램
ggplot(iris, aes(x = Sepal.Length)) + 
  ### bins() 구간이 몇개로 나누는지 적는 함수
  geom_histogram(bins = 20) +
  labs(title = "Sepal.Length Histogram")

# 상자그림
ggplot(iris, aes(x=Species, 
                 y=Sepal.Length)) + 
  geom_boxplot() +
  labs(title = "Species vs Sepal.Length")


## 상자그림을 이용한 탐색
library(gridExtra)
p=list()
p[[1]] = ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot() + labs(title = "Sepal.Length", y = "", x = "")
p[[2]] = ggplot(iris, aes(x=Species, y=Sepal.Width)) + geom_boxplot() + labs(title = "Sepal.Width", y = "", x = "") +
  ### 차트 위에 그리기
  geom_rect(aes(xmin = 2 - 0.5, xmax = 3 + 0.5, ymin = 2 - 0.5, ymax = 4 + 0.5),
            fill = "transparent", color = "tomato", size = 1.5)
p[[3]] = ggplot(iris, aes(x=Species, y=Petal.Length)) + geom_boxplot() + labs(title = "Petal.Length", y = "", x = "")
p[[4]] = ggplot(iris, aes(x=Species, y=Petal.Width)) + geom_boxplot() + labs(title = "Petal.Width", y = "", x = "")
### 그래프들을 한번에 묶어서 그림. 
grid.arrange(grobs = p, ncol=2)

## ?닔移? ?슂?빟?쓣 ?넻?븳 ?깘?깋
tapply(iris$Sepal.Width, iris$Species, summary)


## 그림으로 좀더 자세히 알아보기
library(dplyr)

iris %>%
  ## setosa 제거
    filter(Species != "setosa") %>%
  ggplot(data = ., 
         aes(x = Sepal.Width)) + 
  ## 색을 나눠야 하기 때문에 species에 색을 지정해줌
  geom_histogram(aes(fill = Species), 
                 bins = 20) +
  labs(title = "Sepal.Width by Species") +
  theme(legend.position = "bottom")


iris %>%
  filter(Species != "setosa") %>%
  ggplot(data = ., 
         aes(x = Sepal.Width)) + 
  ## 밀도함수를 그려 확인해보기
  geom_density(aes(fill = Species),
               alpha = 0.5) +
  labs(title = "Sepal.Width by Species") +
  theme(legend.position = "bottom")  


## 가설점정

## Step 1: 등분산 검정
# 가설: $H_0: \sigma_{vs}^2 = \sigma_{vg}^2$ vs $H_1: \sigma_{vs}^2 \neq \sigma_{vg}^2$
### 각각 추출해서 벡터값에 입력
vs <- iris$Sepal.Width[iris$Species=='versicolor']
vg <- iris$Sepal.Width[iris$Species=='virginica']
var.test(vs, vg)

# `p-value`$>0.05이므로 유의수준 5%에서 귀무가설 기각.
# 이제 등분산을 가정한 이표본 t검정을 진행한다.

## Step 2: 등분산 독립 이표본 t검정
#가설: $H_0: \mu_{vs} = \mu_{vg}$ vs $H_1: \mu_{vs} \neq \mu_{vg}$
t.test(vs, vg, var.equal = T)

# `p-value`$<0.05이므로 유의수준에서 귀무가설 기각.


## 손계산 연습
mu.vg = mean(vg)
mu.vs = mean(vs)  
n.vg = length(vg); n.vs = length(vs); df.v = n.vg + n.vs -2
mu.vg; mu.vs
Sp = sqrt(((n.vg-1)*var(vg)+(n.vs-1)*var(vs)) / df.v)
t0 = (mu.vs - mu.vg) / (Sp * sqrt( 1/n.vs + 1/n.vg ))
cat("t statistic = ", t0) #same with result of t.test
cat("p-value = ", 2 * pt(t0, df = df.v)) #same with result of t.test

## 검정통계량이 어디에 위치하는지 확인
par(mar=c(4,4,1,1))
x = seq(-5, 5, length = 100)
plot(x, dt(x, df = df.v), type = 'l', ylab="",xlab="", main = "t distribution")
abline(v = t0, col='red')
abline(v = -t0, col='red', lty=2)
