## =============================== <1>
if (!require(plm)) install.packages("plm")
if (!require(ggplot2)) install.packages("ggplot2")
library(plm)
library(ggplot2)
data("Grunfeld", package = "plm")
## ===============================

## =============================== <2>
head(Grunfeld)
## ===============================

## =============================== <3>
p <- ggplot(Grunfeld, aes(x = inv, y = value, shape = factor(firm))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal(24, font.h) +
  labs(x = "투자",
       y = "기업 가치",
       color = "기업") +
  scale_shape_manual(values = c(0:9)) 
print(p)
## ===============================

## =============================== <4>
전체선형모형 <- lm(value ~ inv, data = Grunfeld)
summary(전체선형모형)
## ===============================

## =============================== <5>
패널모형 <- plm(value ~ inv, 
                   data = Grunfeld, 
                   index = c("firm", "year"), 
                   model = "within")
summary(패널모형)
## ===============================

## =============================== <6>
p2 <- p +
  facet_wrap(~ factor(firm), scales = "free") +
  theme_minimal(16, font.h) +
  theme(axis.text.x = element_text(angle = 90)) 
print(p2)
## ===============================

## =============================== <7>
더미변수를이용한선형모형 <- lm(value ~ inv + factor(firm), 
                   data = Grunfeld)
summary(더미변수를이용한선형모형)
## ===============================

## =============================== <8>
Grunfeld$inv_demeaned <- with(Grunfeld, inv - ave(inv, firm, FUN = mean))
Grunfeld$value_demeaned <- with(Grunfeld, value - ave(value, firm, FUN = mean))
## ===============================

## =============================== <9>
p <- ggplot(Grunfeld, aes(x = inv_demeaned, y = value_demeaned, shape = factor(firm))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal(24, font.h) +
        labs(x = "투자 (평균 중심화)",
             y = "기업 가치 (평균 중심화)",
             shape = "기업") +
  scale_shape_manual(values = c(0:9)) 
print(p)
## ===============================

## =============================== <10>
set.seed(123)
국가수 <- 50
연도수 <- 20
## ===============================

## =============================== <11>
## data <- lapply(1:50, function(id) {...}
##           )
## ===============================

## =============================== <12>
data <- lapply(1:50, function(id) {
            기본교육투자수준 <- runif(1, 2, 8)  
            기본실질성장률 <- runif(1, 5, 15)     
            
            교육투자수준 <- 기본교육투자수준 + cumsum(rnorm(20, 0.1, 0.05))
            실질성장률 <- 기본실질성장률 + 0.5 * (교육투자수준 - 기본교육투자수준) + rnorm(20, 0, 0.5)
            
            data.frame(
              국가 = rep(paste0("국가", sprintf("%02d", id)), 20),
              시점 = 2001:2020,
              교육투자수준,
              실질성장률)}
          )
## ===============================

## =============================== <13>
length(data)
head(data, 3)
## ===============================

## =============================== <14>
data <- do.call(rbind, data)
DT::datatable(data)
## ===============================

## =============================== <15>
p <- ggplot(data, aes(x = 교육투자수준, y = 실질성장률)) +
        geom_point(aes(shape = 국가), alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        theme_minimal(16, font.h) +
        labs(x = "교육 투자",
             y = "실질성장률") +
  scale_shape_manual(values = c(0:49)) 
print(p)
## ===============================

## =============================== <16>
교육과성장률패널모형 <- plm(실질성장률 ~ 교육투자수준, 
                   data = data, 
                   index = c("국가", "시점"), 
                   model = "within")
summary(교육과성장률패널모형)
## ===============================

## =============================== <17>
data$예측치 <- predict(교육과성장률패널모형)
p <- ggplot(data, aes(x = 교육투자수준, y = 실질성장률, shape = 국가)) +
        geom_point(alpha = 0.6) +
        geom_line(aes(y = 예측치)) +
        theme_minimal(16, font.h) +
        labs(x = "교육 투자",
             y = "실질성장률") +
  scale_shape_manual(values = c(0:49)) 
print(p)
## ===============================

## =============================== <18>
library(fixest)
head(trade)
## ===============================

## =============================== <19>
gravity_ols = feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year, trade)
## ===============================

## =============================== <20>
summary(gravity_ols) 
## ===============================

## =============================== <21>
summary(gravity_ols, cluster = ~Origin + Destination + Product + Year)
## ===============================

## =============================== <22>
고정효과 <- fixef(gravity_ols)
summary(고정효과)
## ===============================

## =============================== <23>
par(family = font.h)
plot(고정효과)
## ===============================

