## =============================== <1>
data(mtcars)
선형모형 <- lm(mpg ~ wt, data = mtcars)
비선형모형 <- lm(mpg ~ wt + I(wt^2), data = mtcars)
summary(선형모형)
summary(비선형모형)
## ===============================

## =============================== <2>
par(family = font.h)
plot(mtcars$wt, mtcars$mpg, xlab = "중량", ylab = "연비", pch = 19)
abline(선형모형, col = "blue")
중량범위 <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 100)
lines(중량범위, predict(비선형모형, newdata = data.frame(wt = 중량범위)), col = "red")
legend("topright", legend = c("선형모형", "비선형모형"), col = c("blue", "red"), lty = 1)
## ===============================

## =============================== <3>
rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}
rmse(선형모형)
rmse(비선형모형)
summary(선형모형)$r.squared
summary(비선형모형)$r.squared
AIC(선형모형)
AIC(비선형모형)
BIC(선형모형)
BIC(비선형모형)
## ===============================

## =============================== <4>
library(ggplot2)
p <- ggplot(data.frame(x = c(0.1, 10)), aes(x)) +
  stat_function(fun = log, color = "blue") +
  labs(x = "x",
       y = "log(x)") +
  theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <5>
임의의점들 <- c(0.5, 1, 2.5, 5, 10)
접선함수 <- function(x0) {
  기울기 <- 1 / x0
  절편 <- log(x0) - 기울기 * x0
  function(w) 기울기 * w + 절편
}
## ===============================

## =============================== <6>
p1 <- p + mapply(function(x0) {
    stat_function(
                fun = 접선함수(x0), 
                color = "red", 
                linetype = "dashed",
                xlim = c(x0 - 1, x0 + 1)
            )
  }, 임의의점들)
print(p1)
## ===============================

## =============================== <7>
library(lmtest)
library(sandwich)
data(cars)
model <- lm(dist ~ speed, data = cars)
summary(model)
## ===============================

## =============================== <8>
coeftest(model, vcov = vcovHC(model, type = "HC3"))
## ===============================

