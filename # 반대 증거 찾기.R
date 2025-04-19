## =============================== <1>
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
boston <- read.csv(url, header = TRUE)
boston.lm <- lm(medv ~ rm, data = boston)
print(boston.lm)
## ===============================

## =============================== <2>
summary(boston.lm)
## ===============================

## =============================== <3>
library(broom)
(boston.tidy <- tidy(boston.lm))
## ===============================

## =============================== <4>
추정치.rm <- boston.tidy[["estimate"]][[2]]
표준오차.rm <- boston.tidy[["std.error"]][[2]]
t값.rm <- boston.tidy[["statistic"]][[2]]
## ===============================

## =============================== <5>
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data.frame(x = c(-30, 30)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 표준오차.rm), 
    size = 1.5, color = "blue") +
  labs(x = "t-값", y = "밀도(Density)") 
print(p)
## ===============================

## =============================== <6>
p <- p +
  geom_vline(xintercept = t값.rm, linetype = "dashed", size = 1, color = "red") +
  theme_minimal(24, base_family = font.h) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
            fill = "green", alpha = .7)
print(p)
## ===============================

## =============================== <7>
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
p <- ggplot() +
  xlim(-3, 6) +
  labs(x = NULL, y = NULL) +
  theme_minimal(24, font.h) 
print(p)
## ===============================

## =============================== <8>
p <- p +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
    geom = "area", fill = "blue", alpha = 0.5) 
print(p)
## ===============================

## =============================== <9>
p <- p +
  stat_function(fun = dnorm, args = list(mean = 3, sd = 1), 
    geom = "area", fill = "red", alpha = 0.5) +
  geom_vline(data = data.frame(x = c(0, 3)), aes(xintercept = x), linetype = "dashed") +
  geom_text(data = data.frame(x = c(0, 3), y = c(0.4, 0.4), 
    label = c("H0: beta = 0", "H1: beta = 3")), 
    aes(x = x, y = y, label = label), family = font.h, size = 5) +
  geom_vline(xintercept = 1.96, linetype = "solid", color = "red", size = 1.5) 
print(p)
## ===============================

## =============================== <10>
p <-  p +
  stat_function(fun = dnorm, xlim = c(1.96, 6), args = list(mean = 0, sd = 1), 
    geom = "area", fill = "black", alpha = 0.5) +
  annotate("text", x = 1.96, y = 0.05, label = "Type I Error", 
    color = "black", size = 5, fontface = "bold", family = font.h, hjust = -.3) 
print(p)
## ===============================

## =============================== <11>
p <- p +
  stat_function(fun = dnorm, xlim = c(-3, 1.96), args = list(mean = 3, sd = 1), 
    geom = "area", fill = "orange", alpha = 0.5) +
  annotate("text", x = 1.96, y = 0.05, label = "Type II Error", 
    color = "black", size = 5, fontface = "bold", family = font.h, hjust = 1)
print(p)
## ===============================

## =============================== <12>
print(summary(boston.lm))
## ===============================

## =============================== <13>
boston.lm <- lm(medv ~ rm, data = boston)
print(anova(boston.lm))
## ===============================

## =============================== <14>
library(car)
boston.lm <- lm(medv ~ rm, data = boston)
print(linearHypothesis(boston.lm, "rm = 0", test = "F"))
## ===============================

## =============================== <15>
ggplot() +
  xlim(-4, 4) +
  labs(x = "z", y = "f(z)") +
  theme_minimal(24, font.h) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                geom = "line", size = 1) +
  geom_vline(xintercept = 0, linetype = "dotted", col = "red", size = 1) +
  annotate("text", x = 0, y = 0.42, 
    label = paste0("f(0) = ", dnorm(0, 0, 1)), family = font.h, size = 5)
## ===============================

## =============================== <16>
ggplot() +
  xlim(-4, 4) +
  labs(x = "z", y = "F(z)") +
  theme_minimal(24, font.h) +
  stat_function(fun = pnorm, args = list(mean = 0, sd = 1), 
                geom = "line", size = 1) +
  geom_vline(xintercept = 0, linetype = "dotted", col = "red", size = 1) +
  annotate("text", x = 0, y = pnorm(0, 0, 1), 
    label = paste0("F(0) = ", pnorm(0, 0, 1)), family = font.h, size = 5, vjust = -1) +
  geom_vline(xintercept = 2, linetype = "dotted", col = "blue", size = 1) +
  annotate("text", x = 2, y = pnorm(2, 0, 1), 
    label = paste0("F(2) = ", pnorm(2, 0, 1)), family = font.h, size = 5, vjust = -1)
## ===============================

## =============================== <17>
ggplot() +
  xlim(0, 1) +
  labs(x = "P(z <= a)", y = "z") +
  theme_minimal(24, font.h) +
  stat_function(fun = qnorm, args = list(mean = 0, sd = 1), 
                geom = "line", size = 1) +
  geom_vline(xintercept = 0.95, linetype = "dotted", col = "red", size = 1) +
  annotate("text", x = 0.95, y = qnorm(0.95, 0, 1), 
    label = paste0("F^-1(0.95) = ", qnorm(0.95, 0, 1)), 
    family = font.h, size = 5, vjust = -1) 
## ===============================

## =============================== <18>
library(dplyr)
library(ggplot2)
set.seed(1234)
표준정규분포난수 <- rnorm(100, 0, 1)
표준정규분포난수.df <- data.frame(x = 표준정규분포난수) 
ggplot(표준정규분포난수.df, aes(x = x)) +
  geom_density(fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1, color = "red") +
  labs(x = "z", y = "density") +
  theme_minimal(24, font.h)
## ===============================

