## =============================== <1>
## if (!require(showtext)) install.packages("showtext")
## font.h <- "NanumGothic"
## library(showtext)
## font_add(font.h, "/path/to/NanumGothic.ttf")
## showtext_auto()
## ===============================

## =============================== <2>
## ### 내 R 환경
## 
## setwd("/Users/홍길동/myR")
## 
## library(tidyverse)
## library(lubridate)
## library(ggplot2)
## ===============================

## =============================== <3>
names(mtcars)
mtcars[, "mpg"]
## ===============================

## =============================== <4>
my_data <- data.frame(
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35),
  score = c(85, 92, 78)
)
save(my_data, file = "my_saved_data.RData")
## ===============================

## =============================== <5>
another_vector <- c(1, 2, 3, 4, 5)
save(my_data, another_vector, file = "multiple_objects.RData")
load("my_saved_data.RData")
print(my_data)
## ===============================

## =============================== <6>
save.image()
load(".RData")
## ===============================

## =============================== <7>
# R에 내장된 iris 데이터 세트 불러오기
data(iris)
# 데이터의 처음 6행 확인
head(iris)
# 데이터의 마지막 6행 확인
tail(iris)
# 데이터의 구조 확인
str(iris)
# 데이터의 기술 통계량 확인
summary(iris)
# 특정 열의 평균 계산
mean(iris$Sepal.Length)
# 데이터의 차원(행과 열의 수) 확인
dim(iris)
# 열 이름 확인
names(iris)
# 특정 조건을 만족하는 데이터 추출
subset(iris, Species == "setosa" & Sepal.Length <= 4.5)
# 간단한 산점도 그리기
plot(iris$Sepal.Length, iris$Sepal.Width, 
     col = iris$Species, 
     main = "Iris Sepal Length vs Width")
my.save.figs("1장_Iris Sepal Length vs Width", type = "jpg", dpi = 600)
# 데이터의 상관관계 확인
cor(iris[, 1:4])
## ===============================

