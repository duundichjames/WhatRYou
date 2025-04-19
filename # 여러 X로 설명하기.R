## =============================== <1>
data(mtcars)
lm(mpg ~ wt, data = mtcars)
cor(mtcars$mpg, mtcars$wt) 
## ===============================

## =============================== <2>
mpg정규화 <- scale(mtcars$mpg)
wt정규화 <- scale(mtcars$wt)
lm(mpg정규화 ~ wt정규화)
## ===============================

## =============================== <3>
n <- 100
x1 <- seq(0, 10, length.out = n)
x2 <- seq(0, 10, length.out = n)
y <- outer(x1, x2, function(x1, x2) 2*x1^2 - 0.2*x1^3 - 3*x2^2 + 0.3*x2^3)
par(family = font.h)
persp(x1, x2, y, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      xlab = "x1", ylab = "x2", zlab = "y")
## ===============================

## =============================== <4>
par(family = font.h)
plot(x1, y[ ,21], type = "l", lty = 1, 
     ylim = range(y), xlab = "X1", ylab = "Y")
lines(x1, y[ ,41], lty = 2)
lines(x1, y[ ,61], lty = 3)
lines(x1, y[ ,81], lty = 4)
legend("topleft", 
       legend = c("x2 = 2", "x2 = 4", "x2 = 6", "x2 = 8"),
       lty = 1:4)
## ===============================

## =============================== <5>
par(family = font.h)
plot(x2, y[21, ], type = "l", lty = 1,
     ylim = range(y), xlab = "x2", ylab = "y")
lines(x2, y[41,], lty = 2)
lines(x2, y[61,], lty = 3)
lines(x2, y[81,], lty = 4)
legend("bottomleft", 
       legend = c("x1 = 2", "x1 = 4", "x1 = 6", "x1 = 8"),
       lty = 1:4)
## ===============================

## =============================== <6>
마력과중량효과 <- lm(mpg ~ hp * wt, data = mtcars)
summary(마력과중량효과)
## ===============================

## =============================== <7>
library(margins)
print(margins(마력과중량효과))
## ===============================

## =============================== <8>
마력범위 <- seq(min(mtcars$hp), max(mtcars$hp), length.out = 30)
중량범위 <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 30)
한계효과 <- function(hp, wt) {
  coef(마력과중량효과)["wt"] + coef(마력과중량효과)["hp:wt"] * hp
}
z <- outer(마력범위, 중량범위, Vectorize(한계효과))
par(family = font.h)
persp(마력범위, 중량범위, z, theta = -30, phi = 45,
      xlab = "마력", ylab = "중량", zlab = "중량의 한계 효과",
      ticktype = "detailed", 
      col = "lightblue", shade = 0.5,
      cex.lab = 1, cex.axis = 1)
## ===============================

## =============================== <9>
par(family = font.h)
plot(중량범위, z[6, ], type = "l", lty = 1,
     ylim = range(z), xlab = "중량", ylab = "중량의 한계 효과")
lines(중량범위, z[16, ], lty = 2)
lines(중량범위, z[26, ], lty = 3)
legend("topright", 
       legend = c(paste("hp =", round(마력범위[6], 0)),
                  paste("hp =", round(마력범위[16], 0)),
                  paste("hp =", round(마력범위[26], 0))),
       lty = 1:3)
## ===============================

## =============================== <10>
head(mtcars)
## ===============================

## =============================== <11>
연속형변수모형 <- lm(mpg ~ cyl, data = mtcars)
summary(연속형변수모형)
## ===============================

## =============================== <12>
f.cyl <- factor(mtcars$cyl)
범주형변수모형 <- lm(mpg ~ f.cyl, data = mtcars)
summary(범주형변수모형)
## ===============================

## =============================== <13>
library("patchwork")
p1 <- ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point() +
  theme_minimal(base_size = 18, base_family = font.h) + 
  ggtitle("연속형 변수 모형")
## ===============================

## =============================== <14>
p2 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  geom_point() +
  theme_minimal(base_size = 18, base_family = font.h) + 
  ggtitle("범주형 변수 모형")
print(p1 / p2 )
## ===============================

## =============================== <15>
mtcars$cyl <- factor(mtcars$cyl)
실린더별중량효과 <- lm(mpg ~ wt * cyl, data = mtcars)
summary(실린더별중량효과)
## ===============================

## =============================== <16>
ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(18, font.h) +
  labs(x = "중량", 
       y = "연비",
       color = "실린더 수")
## ===============================

## =============================== <17>
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
변속기와실린더유형의효과 <- lm(mpg ~ cyl * am, data = mtcars)
summary(변속기와실린더유형의효과)
## ===============================

## =============================== <18>
if (!require(modelsummary)) install.packages("modelsummary")
library(modelsummary)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
model1 <- lm(mpg ~ wt, data = mtcars)
model2 <- lm(mpg ~ wt + cyl, data = mtcars)
model3 <- lm(mpg ~ wt + cyl + am, data = mtcars)
model4 <- lm(mpg ~ wt * cyl + am, data = mtcars)
modelsummary(list(model1, model2, model3, model4),
             stars = TRUE,
             vcov = "stata",
             gof_map = c("nobs", "aic", "adj.r.squared"),
             output = "kableExtra")
## ===============================

