install.packages("tibble") # install R pacakge tibble for data input
install.packages("ISwR") # install R package ISwR for a data set
install.packages("DescTools") # for Descriptive Statistics and simple test procedures
install.packages("ggplot2") # Graphing supplement
install.packages('installr')
library(installr)
updateR()
library(tibble)
library(DescTools)
library(ISwR)
library(ggplot2)

## 1. 분할표 작성

# Read and view titanic data set from a .csv file
titan <-read.csv("titanic.csv")
View(titan)
str(titan)

# Use xtabs ("cross-tabulation") to creat a contingency table
tab_1<-xtabs(~ sex + survived, data=titan)

# When data includes the count of each cell, use xtabs as well. 
# This could be one way of manually creating a contingency table.
titan_2 <- tribble(
  ~sex, ~survived, ~Freq,
  # --- / -- / ---
  "Female", "No", 127,
  "Female", "Yes", 339,
  "Male", "No", 682,
  "Male", "Yes", 161,
)
tab_2 <- xtabs(Freq ~ sex + survived, data = titan_2)

# A direct, through error-prone, method of manually creating a contingency table.
tab_3 <- matrix(c(127,682,339,161),nrow=2)
dimnames(tab_3) <- list(sex=c("Female","Male"), survived=c("No","Yes"))

# 수정파트
# 분할표를 개인별 자료로 돌리고자 하는 경우 다음 Untable을 이용하여 얻을 수 있다.
# 여기서 head는 길게 출력되는 개인별 자료를 6개의 줄만 출력해주는 함수이다.
# 개인별자료를 통째로 저장하고자 하는 경우 head를 제외하고 Untable(tab_1)을 실행한다.
head(Untable(tab_1))

# tab_1, tab_2, tab_3 가 같음을 확인할 수 있다 
# 분할표는 자료 정리의 한 형태이다. 다음과 같이 다시 행렬자료로 정리할 수 있다. 
data.frame(tab_2)


tab <- xtabs( ~ survived + pclass, data = titan)

# 주변합은 margin.table()로 분할표의 주변 (margin) 여백을 채운다.
# 행별(row-wise) 주변합은 margin=1, 열별(column-wise) 주변합은 margin=2로 입력한다.
# 전체 주변 여백을 채운 분할표는 addmargins()를 이용한다.

margin.table(tab, margin=2)

addmargins(tab)

# 분할표의 기술통계량 : 상대빈도표
# 열(margin=2)에 있는 변수인 선실 등급(pclass)별로 생존여부(survived)의 상대빈도를 구하면
prop.table(tab, margin=2)

#전체 탑승인원수를 기준으로 상대빈도를 구하면

prop.table(tab)


## 2. r x c 분할표

# 예제: 교육수준과 임상실험
hiv.edu <-matrix(
  c(52,62,53,54,18,25,79,153,213,231,46,139,342,417,629,
    571,139,330,226,262,375,244,74,116),
  nrow=6
)
dimnames(hiv.edu) <-list(
  education=c("고교중퇴","고졸","대학중퇴",
              "대졸","대학원중퇴","대학원졸"),
  참여도 =c("절대안함", "아마도 안함", "아마도 함","반드시 함")
)
# 예제 : 폐암과 흡연
lung.cancer <-matrix(c(7,61,55,129,489,570,475,431,293,154,38,12),
                     nrow=2)
dimnames(lung.cancer)<-list(
  group=c("case","control"),
  smoking=c("0","<5","5-14","15-24","25-49","50+"))


lung.cancer.e <- chisq.test(lung.cancer)$expected # 기댓값 계산
par(mfrow=c(1,2))
mosaicplot(lung.cancer.e, main = "Expected")
mosaicplot(lung.cancer, shade = TRUE, main = "Observed")
par(mfrow=c(1,1))

chisq.test(lung.cancer)

chisq.test(hiv.edu)

  
ISwR::caesar.shoe

mosacicplot(t(ISwR::caesar.shoe))
mosacicplot(ISwR::caesar.shoe)

(caesar.shoe.yes <- caesar.shoe["Yes", ])
(caesar.shoe.total <- margin.table(caesar.shoe, 2))
addmargins(caesar.shoe)

chisq.test(caesar.shoe)

prop.trend.test(caesar.shoe.yes, caesar.shoe.total)



#분할표 정돈
tab <- xtabs(~ survived + embarked, data=titan)
#위의 tab은 분할표의 순서가 ABC순이다.

(tab_mod1 <- tab[,2:4])

(tab_mod2 <- tab_mod1[,c(3,1,2)])

## 3. 2 x 2 분할표 분석 
 
tmp <- tribble(
        ~"조건", ~"결과", ~Freq,
        "A", "위험", 10,
        "A", "Okay", 90,
        "B", "위험", 40,
        "B", "Okay", 60)
tmp.tbl <- xtabs(Freq ~ 조건+결과,data = tmp)
tmp.tbl <- Rev(tmp.tbl, margin = c(1,2))
ex1 <- xtabs(Freq ~ 결과+조건,data = tmp)

 
ex1
(ex2 <- t(ex1))  # 행과 열을 교환한다
(ex3 <- Rev(ex2, margin = 2))  # 열의 변수값 순서를 바꾼다
ex3


RelRisk(ex3)
OddsRatio(ex3)
RelRisk(ex3, conf.level = 0.95)#95% 신뢰구간 추정
OddsRatio(ex3, conf.level = 0.95) 
 


# 비타민 C와 감기 자료 분석
pauling <-matrix(c(17,122,31,109),nrow=2)
dimnames(pauling)<-list(cold=c("Yes", "No"), treatment=c("Vitamin C","Placebo"))
(pauling <- Rev( t(pauling), margin = 1 )) 

chisq.test(pauling)$p.value
RelRisk(pauling, conf.level = 0.95) 

# 흡연과 구강암 자료 분석
keller <-matrix(c(484,385,27,90),nrow=2)
dimnames(keller)<-list(group=c("case", "control"), smoking=c("Yes", "No"))
(keller.new <-t(keller))
 
OddsRatio(keller.new , conf.level = 0.95) 

# 임금과 직업 만족도 자료 분석 
norusis.tab1 <-matrix(c(104,66,391,340),nrow=2)
dimnames(norusis.tab1)<-list(wage=c("Low","High"), satisfaction=c("Low", "High"))
norusis.tab1

RelRisk(norusis.tab1 , conf.level = 0.95) 
chisq.test(norusis.tab1)$p.value

## ----size='scriptsize'---------------------------------------------------
prison <- tribble(
      ~Drug, ~HIV, ~Freq, 
      "Yes", "Positive", 59,
      "Yes", "Negative", 77,
      "No",  "Positive", 29,
      "No", "Negative" , 310
)
prison.tab <- xtabs(Freq~Drug + HIV, data = prison)
( prison.tab <- Rev(prison.tab, margin = c(1,2)) )
 






# McNemar 검정
hivnet <- matrix(c(251, 68, 178, 98), nrow = 2)
dimnames(hivnet) <-list(baseline =c("Incorrect",  "correct"),
                        post = c("Incorrect", "correct"))
mcnemar.test(hivnet)

# Fisher’s Exact Test
CVD.salt.tab <- matrix (c(5,2,30,23), nrow=2)
dimnames(CVD.salt.tab) <-list(CVD=c("Yes", "No"), Salt=c("High", "Low"))
fisher.test(CVD.salt.tab, alternative = "greater")


## 층화분석 

# UCBAdmissions data 

mosaicplot(~ Gender + Admit, UCBAdmissions, shade = TRUE)

opar <- par(mfrow = c(2, 3), oma = c(0, 0, 0, 0))
for (i in 1:6) mosaicplot(UCBAdmissions[, , i], xlab = "Admit", ylab = "Sex", main = paste("Department", LETTERS[i]))
 mtext(expression(bold("Student admissions at UC Berkeley")), outer = TRUE,
 cex = 1.5)
par(mfrow = c(1,1))
 
library(ggplot2) 
ggplot( data = Untable(UCBAdmissions), aes(x = Gender, fill = Admit)) + 
geom_bar(position = "fill")  +facet_wrap(~Dept)


# 3차원 분할표 

MI.raw <- read.csv("MI2.csv")
 
( MIdata <- Rev( xtabs(~ SBP + MI + Age, MI.raw), margin = 1) ) 

# 1. 층별 오즈비, 상대위험도 추정
apply(MIdata, 3, 
      function(x) list(`colnames<-`(rbind(
        "Case-control (odds ratio)" = OddsRatio(x, conf.level = 0.95),
        "Cohort (col1 risk)" = RelRisk(x, conf.level = 0.95),
        "Cohort (col2 risk)" = RelRisk(Rev(x, 1), conf.level = 0.95)),c('Risk','lwr.ci','upr.ci'))))

# 2. Breslow-Day test
BreslowDayTest(MIdata)

# 2'. 층화분석이 필요없을 때 
( MI22 <- xtabs( Freq ~ SBP + MI, data = data.frame(MIdata)) ) 
chisq.test(MI22)$p.value
OddsRatio(MI22, conf.level = 0.95)

# 3. 나이 효과를 조정한 통합 오즈비 추정 및 검정
 mantelhaen.test(MIdata)

  

