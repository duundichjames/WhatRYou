## =============================== <1>
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
library(tidyverse)
library(lubridate)
url <- "https://www.datavis.ca/gallery/minard/ggplot2/ggplot2-minard-gallery.zip"
temp_dir <- tempdir()
temp_file <- file.path(temp_dir, "ggplot2-minard-gallery.zip")
## ===============================

## =============================== <2>
download.file(url, temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)
## ===============================

## =============================== <3>
cities_path <- file.path(temp_dir, "cities.txt")
troops_path <- file.path(temp_dir, "troops.txt")
temps_path <- file.path(temp_dir, "temps.txt")
cities <- read.table(cities_path, header = TRUE, stringsAsFactors = FALSE)
troops <- read.table(troops_path, header = TRUE, stringsAsFactors = FALSE) %>%
    mutate(direction = factor(direction, c("A", "R"), c("진군", "퇴각"))) 
temps <- read.table(temps_path, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(date = dmy(date))
## ===============================

## =============================== <4>
unlink(temp_file)
unlink(temp_dir, recursive = TRUE)
## ===============================

## =============================== <5>
head(cities)
## ===============================

## =============================== <6>
head(troops)
## ===============================

## =============================== <7>
head(temps)
## ===============================

## =============================== <8>
p <- ggplot(cities, aes(x = long, y = lat)) +
    theme_minimal(24, font.h)
p1 <- p + geom_point()
p2 <- p1 + geom_text(
    aes(label = city), hjust = 0, 
    vjust = 1, size = 6, family = font.h)
print(p2)
## ===============================

## =============================== <9>
if (!require(scales)) install.packages("scales")
p3 <- p2 + geom_path(
    aes(size = survivors, 
        colour = direction, group = group, alpha = .5), 
    data = troops) +
    labs(size = "생존자 수", colour = "방향") +
    guides(alpha = "none") +
    scale_size(labels = scales::comma)
p4 <- p3 + scale_colour_manual(
    values = c("grey50", "red")) + 
    scale_x_continuous(limits = c(24, 39)) +
    theme(legend.position = c(.85, .25))
p5 <- p4 + 
    theme(
            axis.title = element_blank(),
            panel.grid = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()
        )
print(p5)
## ===============================

## =============================== <10>
h <- ggplot(temps) + aes(long, temp) + 
    geom_line() +
    theme_minimal(24, font.h)
h1 <- h + geom_text(
    aes(label = paste(day, month)), vjust = 1, size = 6, family = font.h)
h2 <- h1 + 
    scale_x_continuous(
        limits = c(24, 39)) +
    theme(
            axis.title = element_blank(),
            panel.grid = element_blank()
        )
print(h2)
## ===============================

## =============================== <11>
if (!require(patchwork)) install.packages("patchwork")
library(patchwork)
combined_plot <- p5 / h2 + 
  plot_layout(heights = c(7, 3))
print(combined_plot)
## ===============================

## =============================== <12>
p6 <- p5 + 
    theme(
            legend.position = "top",
            legend.key.size = unit(0.8, "lines"),  
            legend.text = element_text(size = 12), 
            legend.title = element_text(size = 12),
            legend.margin = margin(-1.5, 0, 0, 0)
        ) +
    guides(
        colour = guide_legend(
            title.hjust = 0.5,
            label.position = "bottom",
            order = 1
        ),
        size = guide_legend(
            title.hjust = 0,
            order = 2
        )
    ) + 
    scale_size(range = c(1, 10), labels = scales::comma) + 
    scale_colour_manual(values = c("#DFC17E", "#252523"))

combined_plot <- p6 / h2 + 
  plot_layout(heights = c(8.5, 3))
print(combined_plot)
## ===============================

## =============================== <13>
if (!require(ggmap)) install.packages("ggmap")
if (!require(ggrepel)) install.packages("ggrepel")
library(ggmap)
library(ggrepel)
register_google(key = my.google.api)
## ===============================

## =============================== <14>
center_lon <- median(troops$long)
center_lat <- median(troops$lat)
map <- get_googlemap(center = c(lon = center_lon, lat = center_lat),
                     zoom = 5, 
                     maptype = "roadmap")
## ===============================

## =============================== <15>
p <- ggmap(map) +
    geom_point(data = cities, aes(x = long, y = lat), color = "red", size = 2) +
    geom_path(data = troops, 
            aes(x = long, y = lat, color = direction, size = survivors),
            alpha = .8) +
    geom_label_repel(data = cities, 
            aes(x = long, y = lat, label = city),
            family = font.h, size = 3, 
            max.overlaps = 6, alpha = .8) +
    theme_minimal(14, font.h) +
    scale_colour_manual(values = c("red", "blue")) +
    labs(x = "경도", y = "위도", color = "방향") +
    scale_size(range = c(.1, 5), guide = "none") 
print(p)
## ===============================

## =============================== <16>
quartz()
plot(1:10, main = "Quartz Example")
## ===============================

## =============================== <17>
quartz(type = "pdf", file = "quartz_plot.pdf", width = 8, height = 6)
dev.off()
## ===============================

## =============================== <18>
png("plot_example.png", width = 800, height = 600)
plot(1:10, main = "Example Plot in PNG")
graphics.off()  
## ===============================

