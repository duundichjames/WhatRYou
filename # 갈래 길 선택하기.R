## =============================== <1>
par(family = font.h)
x <- seq(-6, 6, length.out = 1000)
## ===============================

## =============================== <2>
plot(x, plogis(x), type = "l", 
     main = "Logistic Function", 
     xlab = expression(x == log(frac(p, `1 - p`))), ylab = "f(x) = p",
     col = "blue", lwd = 2)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
## ===============================

## =============================== <3>
plot(x, pnorm(x), type = "l", 
     main = "Cumulative Standard Normal Distribution", 
     xlab = "x = z", ylab = expression(f(x) == Phi(z)), 
     col = "red", lwd = 2)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
## ===============================

## =============================== <4>
par(mfrow = c(1, 2), family = font.h)
x <- seq(-6, 6, length.out = 1000)
plot(x, plogis(x), type = "l", 
     main = "Logistic Function", 
     xlab = expression(x == log(frac(p, `1 - p`))), ylab = "f(x) = p",
     col = "blue", lwd = 2)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
plot(x, pnorm(x), type = "l", 
     main = "Cumulative Standard Normal Distribution", 
     xlab = "x = z", ylab = expression(f(x) == Phi(z)), 
     col = "red", lwd = 2)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
## ===============================

## =============================== <5>
url <- "https://stats.idre.ucla.edu/stat/data/binary.csv"
admission <- read.csv(url)
head(admission)
## ===============================

## =============================== <6>
admission$rank <- factor(admission$rank)
## ===============================

## =============================== <7>
대학원입학결정요인 <- glm(admit ~ gre, data = admission, family = binomial) 
## ===============================

## =============================== <8>
summary(대학원입학결정요인)
## ===============================

## =============================== <9>
admission$predicted <- predict(대학원입학결정요인, type = "response")
table(admission$predicted > 0.5)
## ===============================

## =============================== <10>
table(admission$admit)
## ===============================

## =============================== <11>
대학원입학결정요인2 <- glm(admit ~ gre + gpa + rank, family = "binomial", data = admission)
summary(대학원입학결정요인2)
## ===============================

## =============================== <12>
admission2 <- subset(admission, gpa >= 3 & rank == 1) 
admission2$predicted <- predict(대학원입학결정요인2, admission2, type = "response")
p <- ggplot(admission2, aes(x = gre, y = admit)) +
          geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
          geom_smooth(method = "glm", method.args = list(family = "binomial"), 
            se = FALSE, color = "blue") +
          geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "red") +
          labs(x = "GRE 점수",
               y = "입학 확률") +
          theme_minimal(24, font.h) +
          scale_y_continuous(limits = c(-0.1, 1.1), breaks = c(0, 0.5, 1)) 
print(p)
## ===============================

## =============================== <13>
p1 <- p + geom_smooth(method = "glm", 
    method.args = list(family = gaussian(link = "identity")), 
    se = FALSE, color = "red") 
print(p1)
## ===============================

## =============================== <14>
대학원입학결정요인2의선형확률모형 <- 
    glm(admit ~ gre + gpa + rank, family = "gaussian", data = admission)
summary(대학원입학결정요인2의선형확률모형)
## ===============================

## =============================== <15>
if (!require(marginaleffects)) install.packages("marginaleffects")
library(marginaleffects)
평균한계효과 <- avg_slopes(대학원입학결정요인2)
print(평균한계효과)
## ===============================

## =============================== <16>
p <- plot_predictions(대학원입학결정요인2, condition = "gre") +
  theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <17>
특정x의한계효과 <- slopes(대학원입학결정요인2, 
    newdata = data.frame(gre = 700, gpa = 3.5, rank = factor(1, levels = 1:4)))
print(특정x의한계효과)
## ===============================

## =============================== <18>
if (!require(forcats)) install.packages("forcats")
library(forcats)
library(dplyr)
admission$rank <- factor(admission$rank)
p <- admission %>% 
  mutate(rank = fct_infreq(rank)) %>%
  ggplot(aes(x = rank)) +
  geom_bar() +
  theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <19>
p <- admission %>%
  mutate(rank_grouped = fct_collapse(rank,
                                     top = c("1", "2"),
                                     other = c("3", "4"))) %>%
  ggplot(aes(x = rank_grouped, y = gre)) +
  geom_boxplot() +
  theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <20>
p <- admission %>%
  mutate(rank = fct_recode(rank,
                           "최상위" = "1",
                           "상위" = "2",
                           "중위" = "3",
                           "하위" = "4")) %>%
  ggplot(aes(x = rank, y = gre)) +
  geom_boxplot() +
  theme_minimal(24, font.h)
print(p)
## ===============================

## =============================== <21>
admission$rank <- fct_relevel(admission$rank, "4")
기준4인경우결정요인 <- glm(admit ~ gre + gpa + rank, data = admission, family = binomial)
summary(기준4인경우결정요인)
## ===============================

## =============================== <22>
admission$rank <- factor(admission$rank, 1:4)
p <- admission %>%
  mutate(rank_rev = fct_rev(rank)) %>%
  ggplot(aes(x = rank_rev, y = gpa)) +
  geom_boxplot() +
  theme_minimal(24, font.h)
print(p)
## ===============================

