## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png" # <3
)
library(ggpointless)

## ----default------------------------------------------------------------------
library(ggplot2)
library(ggpointless)

x <- seq(-pi, pi, length.out = 100)
y <- outer(x, 1:5, function(x, y) sin(x*y))

df1 <- data.frame(
  var1 = x,
  var2 = rowSums(y)
)

p <- ggplot(df1, aes(x = var1, y = var2))
p + geom_pointless(location = c("first", "last", "minimum", "maximum"))

## ----location-----------------------------------------------------------------
p <- p + geom_line()
p + geom_pointless(location = "all", size = 3)

## ----color--------------------------------------------------------------------
p + geom_pointless(aes(color = after_stat(location)),
                   location = "all",
                   size = 3) +
  theme(legend.position = "bottom")

## ----spiral, fig.show='hold'--------------------------------------------------
x <- seq(5, -1, length.out = 1000) * pi
spiral <- data.frame(var1 = sin(x) * 1:1000, 
                     var2 = cos(x) * 1:1000)

p <- ggplot(spiral) +
  geom_path() +
  coord_equal(xlim = c(-1000, 1000), ylim = c(-1000, 1000)) +
  theme(legend.position = "none")

p + aes(x = var1, y = var2) +
  geom_pointless(aes(color = after_stat(location)),
                 location = "all",
                 size = 3) +
  labs(subtitle = "orientation = 'x'")

p + aes(y = var1, x = var2) +
  geom_pointless(aes(color = after_stat(location)),
                 location = "all",
                 size = 3) +
  labs(subtitle = "orientation = 'y'")

## ----overplotting, fig.show='hold'--------------------------------------------
df2 <- data.frame(var1 = 1:2,
                  var2 = 1:2)
p <- ggplot(df2, aes(x = var1, y = var2)) +
  geom_path() +
  coord_equal()

# same as location = 'all'
p + geom_pointless(aes(color = after_stat(location)),
                   location = c("first", "last", "minimum", "maximum"),
                   size = 3) +
  labs(subtitle = "same as location = 'all'")
# reversed order
p + geom_pointless(aes(color = after_stat(location)),
                   location = c("maximum", "minimum", "last", "first"),
                   size = 3) +
  labs(subtitle = "custom order")
# same as location = 'all' again
p + geom_pointless(aes(color = after_stat(location)),
                   location = c("maximum", "minimum", "last", "first", "all"),
                   size = 3) +
  labs(subtitle = "same as location = 'all' again")

## ----stat---------------------------------------------------------------------
set.seed(42)
ggplot(data.frame(x = 1:10, y = sample(1:10)), aes(x, y)) + 
  geom_line() + 
  stat_pointless(
    aes(yintercept = y, color = after_stat(location)),
    location = c("minimum", "maximum"),
    geom = "hline"
    ) + 
  guides(color = guide_legend(reverse = TRUE))

