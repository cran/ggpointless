## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png" # <3
)
library(ggpointless)

## ----default_pointless--------------------------------------------------------
library(ggplot2)
library(ggpointless)

x <- seq(-pi, pi, length.out = 100)
y <- outer(x, 1:5, function(x, y) sin(x * y))

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
  size = 3
) +
  theme(legend.position = "bottom")

## ----spiral, fig.show='hold'--------------------------------------------------
x <- seq(5, -1, length.out = 1000) * pi
spiral <- data.frame(
  var1 = sin(x) * 1:1000,
  var2 = cos(x) * 1:1000
)

p <- ggplot(spiral) +
  geom_path() +
  coord_equal(xlim = c(-1000, 1000), ylim = c(-1000, 1000)) +
  theme(legend.position = "none")

p + aes(x = var1, y = var2) +
  geom_pointless(aes(color = after_stat(location)),
    location = "all",
    size = 3
  ) +
  labs(subtitle = "orientation = 'x'")

p + aes(y = var1, x = var2) +
  geom_pointless(aes(color = after_stat(location)),
    location = "all",
    size = 3
  ) +
  labs(subtitle = "orientation = 'y'")

## ----overplotting-------------------------------------------------------------
cols <- c(
  "first" = "#f8766d",
  "last" = "#7cae00",
  "minimum" = "#00bfc4",
  "maximum" = "#c77cff"
)

df2 <- data.frame(
  var1 = 1:2,
  var2 = 1:2
)

p <- ggplot(df2, aes(x = var1, y = var2)) +
  geom_path() +
  coord_equal() +
  scale_color_manual(values = cols)

# same as location = 'all'
p + geom_pointless(aes(color = after_stat(location)),
  location = c("first", "last", "minimum", "maximum"),
  size = 3
) +
  labs(subtitle = "same as location = 'all'")

## -----------------------------------------------------------------------------
# reversed order
p + geom_pointless(aes(color = after_stat(location)),
  location = c("maximum", "minimum", "last", "first"),
  size = 3
) +
  labs(subtitle = "custom order")

## -----------------------------------------------------------------------------
# same as location = 'all' again
p + geom_pointless(aes(color = after_stat(location)),
  location = c("maximum", "minimum", "last", "first", "all"),
  size = 3
) +
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

## ----default_lexis------------------------------------------------------------
df1 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  x = c(0, 1, 6, 5, 6),
  y = c(5, 4, 10, 8, 10)
)

p <- ggplot(df1, aes(x = x, xend = y, color = key)) +
  coord_equal()
p + geom_lexis()

## ----gap_filler, fig.show='hold'----------------------------------------------
p + geom_lexis(gap_filler = FALSE)

## ----after_stat---------------------------------------------------------------
p + geom_lexis(
  aes(linetype = after_stat(type)),
  point_show = FALSE
) +
  scale_linetype_identity()

## ----numeric------------------------------------------------------------------

df1 <- data.frame(
  start = c(2019, 2021),
  end = c(2022, 2022),
  key = c("A", "B")
)

ggplot(df1, aes(x = start, xend = end, group = key)) +
  geom_lexis() +
  coord_fixed()

## ----dates-and-times, fig.show='hold'-----------------------------------------
# Date
fun <- function(i, class) as.Date(paste0(i, "-01-01"))
df1[, c("start", "end")] <- lapply(df1[, c("start", "end")], fun)
p1 <- ggplot(df1, aes(x = start, xend = end, group = key)) +
  geom_lexis() +
  labs(y = "days") +
  coord_fixed()

# POSIXct
df2 <- df1
df2[, c("start", "end")] <- lapply(df2[, c("start", "end")], as.POSIXct)
p2 <- ggplot(df2, aes(x = start, xend = end, group = key)) +
  geom_lexis() +
  labs(y = "seconds") +
  coord_fixed()

p1
p2

## ----transformation, fig.show='hold'------------------------------------------
# years, roughly
p1 +
  scale_y_continuous(
    breaks = 0:3 * 365.25, # or for p2: 0:3*365.25*86400
    labels = function(i) floor(i / 365.25) # floor(i / 365.25*86400)
  ) +
  labs(y = "years")

## ----geom-chaikin-intro, fig.show='hold'--------------------------------------
set.seed(42)
dat <- data.frame(
  x = seq.int(10),
  y = sample(15:30, 10)
)

p1 <- ggplot(dat, aes(x, y)) +
  geom_line(linetype = "12")

p1 +
  geom_chaikin()

## ----geom-catenary-intro, fig.show='hold'-------------------------------------
ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_catenary() +
  ylim(0, 1)

## ----geom-catenary-chainLength, fig.show='hold'-------------------------------
ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_catenary(chainLength = 1.5)  +
  ylim(0, 1)

## ----geom-catenary-chainLength-two-obs, fig.show='hold'-----------------------
ggplot(data.frame(x = c(0, 1, 4), y = c(1, 1, 1)), aes(x, y)) +
  geom_catenary()

## ----geom-catenary-chainLength-minimum, fig.show='hold'-----------------------
ggplot(data.frame(x = c(0, 1, 4), y = c(1, 1, 1)), aes(x, y)) +
  geom_catenary(chainLength = 4)

## ----geom-catenary-chainLength-individual, fig.show='hold'--------------------
ggplot(data.frame(x = c(0, 1), y = 1),
       aes(x, y)) + 
  lapply(2:10, function(chainLength) {
    geom_catenary(chainLength = chainLength)
  }
)

