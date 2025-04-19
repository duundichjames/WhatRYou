## =============================== <1>
흰공 = rep("흰공", 30000)
검은공 = rep("검은공", 70000)
박스 = c(흰공, 검은공)
## ===============================

## =============================== <2>
# install.packages("dplyr", dependencies = TRUE)
library(dplyr)
# install.packages("magrittr", dependencies = TRUE)
library(magrittr)
## ===============================

## =============================== <3>
N <- c(10, 100, 1000, 10000)
r <- 1000
흰공뽑기 <- function(n) sample(박스, n, replace = TRUE)
흰공비율 <- function(x) length(x[x == "흰공"])/length(x)
## ===============================

## =============================== <4>
시뮬레이션 <- function(n) {
  replicate(r, 흰공뽑기(n) %>% 흰공비율)
}
## ===============================

## =============================== <5>
set.seed(1234)
결과 <- N %>%
  lapply(시뮬레이션)
## ===============================

## =============================== <6>
names(결과) <- N
## ===============================

## =============================== <7>
결과 %>% lapply(head, 20)
## ===============================

## =============================== <8>
흰공비율평균 <- 결과 %>%
  sapply(mean)
흰공비율표준편차 <- 결과 %>%
  sapply(sd)
## ===============================

## =============================== <9>
모평균 <- 0.3
## ===============================

## =============================== <10>
print(흰공비율평균)  
print(흰공비율표준편차)  
## ===============================

## =============================== <11>
모분산 <- 0.21
## ===============================

## =============================== <12>
par(family= font.h, 
   mfrow = c(2, 2))
hist(결과[["10"]], freq = FALSE, xlab = "표본평균 값", ylab = "밀도", 
  main = "표본크기 10인 표본평균 분포")
curve(dnorm(x, mean = 0.3, sd = 0.1449272), 
      col = "red", lwd = 2, add = TRUE)
hist(결과[["100"]], freq = FALSE, xlab = "표본평균 값", ylab = "밀도", 
  main = "표본크기 100인 표본평균 분포")
curve(dnorm(x, mean = 0.3, sd = 0.04583), 
      col = "red", lwd = 2, add = TRUE)
hist(결과[["1000"]], freq = FALSE, xlab = "표본평균 값", ylab = "밀도", 
  main = "표본크기 1000인 표본평균 분포")
curve(dnorm(x, mean = 0.3, sd = 0.0144927), 
      col = "red", lwd = 2, add = TRUE)
hist(결과[["10000"]], freq = FALSE, xlab = "표본평균 값", ylab = "밀도", 
  main = "표본크기 10000인 표본평균 분포")
curve(dnorm(x, mean = 0.3, sd = 0.004583), 
      col = "red", lwd = 2, add = TRUE)
## ===============================

## =============================== <13>
## filter(starwars, species == "Human")
## filter(starwars, mass > 1000)
## ===============================

## =============================== <14>
## starwars %>% select(height)
## starwars %>% select(homeworld, height, mass)
## starwars %>% select(name:mass)
## ===============================

## =============================== <15>
## starwars %>%
##   select(name, mass) %>%
##   mutate(
##     mass2 = mass * 2,
##     mass2_squared = mass2 * mass2
##   )
## starwars %>%
##   select(name, height, mass, homeworld) %>%
##   mutate(
##     mass = NULL,
##     height = height * 0.0328084
##   )
## ===============================

## =============================== <16>
## mtcars %>%
##   summarise(mean = mean(disp), n = n())
## mtcars %>%
##   summarise(disp = mean(disp), sd = sd(disp), .by = cyl)
## 
## ===============================

