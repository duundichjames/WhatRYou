## =============================== <1>
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
boston <- read.csv(url, header = TRUE)
## ===============================

## =============================== <2>
head(boston)
## ===============================

## =============================== <3>
par(family = font.h)
plot(boston$rm, boston$medv)
## ===============================

## =============================== <4>
lm(medv ~ rm, data = boston)
## ===============================

## =============================== <5>
par(family = font.h)
plot(boston$rm, boston$medv)
boston.lm <- lm(medv ~ rm, data = boston)
abline(boston.lm, col = "red")
## ===============================

## =============================== <6>
str(boston.lm)
## ===============================

## =============================== <7>
boston.lm[["coefficients"]]
## ===============================

## =============================== <8>
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
boston <- read.csv(url, header = TRUE)
boston.lm <- lm(medv ~ rm, data = boston)
## ===============================

## =============================== <9>
par(family = font.h)
plot(boston$rm, boston$medv, 
  xlab = "Number of Rooms", ylab = "Price of Houses", col = "gray", pch = 16)
abline(boston.lm, col = "red")
saved_plot <- recordPlot()
## ===============================

## =============================== <10>
replayPlot(saved_plot)
x평균 <- mean(boston$rm)
abline(v = x평균, lty = 2)
y평균 <- mean(boston$medv)
abline(h = y평균, lty = 2)
## ===============================

## =============================== <11>
replayPlot(saved_plot)
points(x평균, y평균, col = "black", pch = 16, cex = 1.5)
text(x평균, y평균, "C", cex = 1.5, pos = 4)
saved_plot <- recordPlot()
## ===============================

## =============================== <12>
replayPlot(saved_plot)
임의의점 <- boston[boston$rm > 7 & boston$rm < 8, ][1, ]
points(임의의점$rm, 임의의점$medv, col = "blue", pch = 16, cex = 1.5)
text(임의의점$rm, 임의의점$medv, "A", col = "blue", pch = 16, cex = 1.5, pos = 4)
saved_plot <- recordPlot()
## ===============================

## =============================== <13>
절편 = boston.lm[["coefficients"]][["(Intercept)"]]
기울기 = boston.lm[["coefficients"]][["rm"]]
## ===============================

## =============================== <14>
replayPlot(saved_plot)
적합선위의점x <- 임의의점$rm
적합선위의점y <- 절편 + 기울기*임의의점$rm
points(적합선위의점x, 적합선위의점y, col = "blue", pch = 16, cex = 1.5)
text(적합선위의점x, 적합선위의점y, "A'", col = "blue", pch = 16, cex = 1.5, pos = 4)
saved_plot <- recordPlot()
## ===============================

## =============================== <15>
replayPlot(saved_plot)
수평선위의점x <- 임의의점$rm
수평선위의점y <- y평균
points(수평선위의점x, 수평선위의점y, col = "blue", pch = 16, cex = 1.5)
text(수평선위의점x, 수평선위의점y, "A''", col = "blue", pch = 16, cex = 1.5, pos = 4)
saved_plot <- recordPlot()
## ===============================

## =============================== <16>
replayPlot(saved_plot)
library("shape")
Arrows(임의의점$rm, 임의의점$medv, 적합선위의점x, 적합선위의점y, 
  code = 3, arr.type = "triangle", 
  arr.adj = .3, arr.length = 0.3, arr.lwd = 0.3, segment = TRUE)
text(임의의점$rm, .5*(임의의점$medv + 적합선위의점y), 
  "What the error explains", pos = 4)
Arrows(적합선위의점x, 적합선위의점y, 수평선위의점x, 수평선위의점y, 
  code = 3, arr.type = "triangle", 
  arr.adj = .3, arr.length = 0.3, arr.lwd = 0.3, segment = TRUE)
text(적합선위의점x, .5*(적합선위의점y + 수평선위의점y), 
  "What the model explains", pos = 4)
## ===============================

## =============================== <17>
p <- ggplot(boston, aes(x = rm, y = medv)) +
  geom_point(color = "gray") +
  theme_minimal(base_size = 24, base_family = font.h)
print(p)
## ===============================

## =============================== <18>
p <- p +
  geom_vline(xintercept = x평균, linetype = 2) +
  geom_hline(yintercept = y평균, linetype = 2) +
  geom_abline(intercept = 절편, slope = 기울기, color = "red") 
print(p)
## ===============================

## =============================== <19>
p <- p +
  annotate("point", x = x평균, y = y평균, size  = 3) +
  annotate("text", x = x평균, y = y평균, label = "C", size  = 6, hjust = -1, 
    family = font.h) +
  geom_point(data = 임의의점, aes(x = rm, y = medv), color = "black", size = 3) +
  geom_text(data = 임의의점, aes(x = rm, y = medv), color = "black", size = 6, 
    label = "A", hjust = -1, family = font.h) +
  annotate("point", x = 적합선위의점x, y = 적합선위의점y, size  = 3) +
  annotate("text", x = 적합선위의점x, y = 적합선위의점y, label = "A'", 
    size  = 6, hjust = -1, family = font.h) +
  annotate("point", x = 수평선위의점x, y = 수평선위의점y, size  = 3) +
  annotate("text", x = 수평선위의점x, y = 수평선위의점y, label = "A''", 
    size  = 6, hjust = -1, family = font.h) +
  annotate("segment", x = 임의의점$rm, y = 임의의점$medv, 
    xend = 적합선위의점x, yend = 적합선위의점y, size  = .4, 
    arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed")) +
  annotate("text", x = 임의의점$rm, y = .5*(임의의점$medv + 적합선위의점y), 
    label = "What the error explains", size  = 6, hjust = -0.15, 
    family = font.h) +
  annotate("segment", x = 적합선위의점x, y = 적합선위의점y, 
    xend = 수평선위의점x, yend = 수평선위의점y, size  = .4, 
    arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed")) +
  annotate("text", x = 적합선위의점x, y = .5*(적합선위의점y + 수평선위의점y), 
    label = "What the model explains", size  = 6, hjust = -0.15, 
    family = font.h) 
print(p)
## ===============================

