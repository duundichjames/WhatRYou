## =============================== <1>
if (!require(forecast)) install.packages("forecast")
if (!require(ggplot2)) install.packages("ggplot2")
library(forecast)
library(ggplot2)
data("AirPassengers")
print(AirPassengers)
## ===============================

## =============================== <2>
par(family = font.h)
plot(AirPassengers, ylab = "승객 수", xlab = "연도")
## ===============================

## =============================== <3>
p <- autoplot(AirPassengers) +
      xlab("연도") +
      ylab("승객 수") +
      theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <4>
air_stl <- stl(AirPassengers, s.window = "periodic")
print(head(air_stl$time.series))
## ===============================

## =============================== <5>
plot(air_stl)
## ===============================

## =============================== <6>
data("AirPassengers")
(연도별평균 <- tapply(AirPassengers, floor(time(AirPassengers)), mean))
(연도별분산 <- tapply(AirPassengers, floor(time(AirPassengers)), var))
## ===============================

## =============================== <7>
data("AirPassengers")
(자기상관계수 <- acf(AirPassengers, plot = FALSE)$acf)
## ===============================

## =============================== <8>
par(family = font.h)
plot(자기상관계수)
## ===============================

## =============================== <9>
par(family = font.h)
pacf(AirPassengers)
## ===============================

## =============================== <10>
if (!require(urca)) install.packages("urca")
library(urca)
디키풀러검정결과 <- ur.df(AirPassengers)
summary(디키풀러검정결과)
## ===============================

## =============================== <11>
par(family = font.h)
일차차분 <- diff(AirPassengers, differences = 1)
plot(일차차분)
## ===============================

## =============================== <12>
par(family = font.h)
이차차분 <- diff(AirPassengers, differences = 2)
plot(이차차분)
## ===============================

## =============================== <13>
if (!require(forecast)) install.packages("forecast")
library(forecast)
arima모형 <- auto.arima(AirPassengers)
summary(arima모형)
## ===============================

## =============================== <14>
par(family = font.h)
arima모형예측 <- forecast(arima모형, h = 24) 
plot(arima모형예측)
## ===============================

## =============================== <15>
if (!require(lubridate)) install.packages("lubridate")
if (!require(xts)) install.packages("xts")
if (!require(zoo)) install.packages("zoo")
library(lubridate)
library(xts)
library(zoo)
dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "month")
values <- rnorm(length(dates), mean = 100, sd = 10)
## ===============================

## =============================== <16>
(months <- month(dates))
(years <- year(dates))
## ===============================

## =============================== <17>
is.Date("2010-10-13")
is.character("2010-10-13")
is.Date(ymd("2010-10-13"))
mdy("March-01-2015")
is.Date(mdy("March-01-2015"))
## ===============================

## =============================== <18>
(yearqtr <- as.yearqtr(dates))
## ===============================

## =============================== <19>
(ts_data <- xts(values, order.by = dates))
(quarterly_mean <- apply.quarterly(ts_data, mean))
## ===============================

