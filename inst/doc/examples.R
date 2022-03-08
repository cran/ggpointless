## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  fig.width = 6,
  fig.height = 6,
  out.width = "95%",
  dev = "ragg_png" # <3
)

library(ggplot2)
library(ggpointless)
library(ggtext)

## ----libs---------------------------------------------------------------------
library(ggplot2)
library(ggpointless)
library(ggtext)

## ----theme--------------------------------------------------------------------
text_size <- 2.5
text_color <- "#4b4c4d"

theme_set(theme_minimal() + 
            theme(plot.caption = element_text(hjust = 0)) + 
            theme(plot.caption.position = "plot") +
            theme(text = element_text(size = 9, color = text_color)) +
            theme(axis.ticks.length.x = unit(0, "mm")) + 
            theme(axis.ticks.length.y = unit(0, "mm")) +
            # https://stackoverflow.com/a/17312440/8583393
            theme(axis.title = element_text(size = text_size * 1/0.352777778)) +
            theme(axis.title.x = element_text(hjust = 1)) + 
            theme(axis.title.y = element_text(hjust = 1, angle = 0)) +
            theme(panel.grid.minor = element_blank()) +
            theme(legend.position = "none") +
            theme(plot.title = element_text(face = "bold")) +
            theme(plot.title.position = "plot")
          )

## ----co2_ml-------------------------------------------------------------------
data(co2_ml)

## ----date_scale---------------------------------------------------------------
co2_ml$date_scale <- as.Date(sprintf("%d-%d-01", 1950 + (co2_ml$year %% 10), co2_ml$month))

## ----labeller_co2-------------------------------------------------------------
axis_labeller <- function(date) {
  year <- as.integer(format(date, "%Y"))
  tmp <- year - min(year, na.rm = TRUE)
  replace(tmp, !tmp, "")
}

## ----co2, echo=TRUE, warning=FALSE--------------------------------------------
# layers
p <- ggplot(co2_ml, aes(date_scale, co2_ppm, color = decade))
# sustainable level
p <- p + geom_hline(aes(yintercept = 350),
                    color = "#dbd9db",
                    size = 1)
p <- p + geom_text(aes(x = as.Date("1951-01-01"), y = 348), 
                   label = "sustainable level",
                   size = text_size + .3,
                   color = "#dbd9db",
                   hjust = "left",
                   inherit.aes = FALSE)
p <- p + geom_line()
p <- p + geom_pointless(location = c("first", "last"), size = 2)
# label decades
p <- p + geom_text(
  data = subset(co2_ml, subset = decade != "2020's"),
  aes(label = decade),
  size = text_size,
  stat = "pointless",
  location = "last",
  hjust = "left",
  nudge_x = 40
)
# label at first year in decade
p <- p + geom_text(
  data = subset(co2_ml, subset = decade != "1950's"),
  aes(label = year, group = decade),
  size = text_size,
  color = text_color,
  stat = "pointless",
  location = "first",
  vjust = "top",
  hjust = "left",
  nudge_y = -2
)

# label at last year in decade
p <- p + geom_text(
  data = subset(co2_ml, subset = !(decade %in% c("1950's", "2020's"))),
  aes(label = year, group = decade),
  size = text_size,
  color = text_color,
  stat = "pointless",
  location = "maximum",
  vjust = "bottom",
  nudge_y = 1
)

# label the years 2014 to 2018 in the format '%y
p <- p + lapply(2014:2018, function(yrs) {
  geom_text(
    data = subset(co2_ml, subset = year == yrs),
    aes(label = sprintf("'%i", year %% 100), group = year),
    size = text_size,
    color = text_color,
    stat = "pointless",
    location = "maximum",
    vjust = "bottom",
    nudge_y = 1
  )
})

#  highlight all-time maximum
p <- p + geom_pointless(
  data = subset(co2_ml, subset = decade == "2020's"),
  location = "maximum",
  size = 8,
  shape = 21,
  fill = NA,
  stroke = .9
)

# label all-time maximum
p <- p + ggtext::geom_richtext(
  aes(
    x = date_scale + 500,
    y = co2_ppm - 3,
    group = NULL,
    label = sprintf("**%g ppm**<br>%s %s", round(co2_ppm), month.name[month], year)
  ),
  stat = "pointless",
  location = "maximum",
  size = text_size,
  color = text_color,
  fill = NA,
  label.color = NA
)

# draw curve from all-time maximum to its label
p <- p + geom_curve(
  data = subset(co2_ml, subset = decade == "2020's"),
  aes(
    x = date_scale + 90,
    xend = date_scale + 500,
    y = co2_ppm + 2,
    yend = co2_ppm + 2
  ),
  stat = "pointless",
  location = "maximum",
  curvature = -.4,
  size = .35,
  color = text_color,
  inherit.aes = FALSE
)

# scales
p <- p + scale_x_date(
  breaks = as.Date(sprintf("%i-01-01", c(1950:1960))),
  labels = axis_labeller,
  expand = expansion(mult = c(0.01, -.03)),
  limits = as.Date(sprintf("%i-01-01", c(1950, 1961))))

# colors
p <- p + scale_color_manual(
    values = c(
      '#f4ae1b',
      '#e99950',
      '#dc8471',
      '#cc708f',
      '#b85baa',
      '#9f46c6',
      '#7a31e1',
      '#311dfc'
    )
  )

# title, subtitle, caption
p <- p + 
  labs(title = "Carbon Dioxide Concentration in the Atmosphere",
       subtitle = "Each line represents one decade, from 1958 to 2022. CO2 concentration is measured in\nparts per million* (ppm).",
       caption = "*The mole fraction of CO2, expressed as parts per million (ppm) is the number of molecules of CO2 in every million\nmolecules of dried air (water vapor removed). The 'sustainable level' of 350ppm, equivalent to the 1990 levels, has\nbeen identified by UN climate scientists.\nSource: National Oceanic & Atmospheric Adm. (NOAA)",
       x = "Years into decade",
       y = "ppm")


## ----co2_plot, fig.height=6.5, echo=FALSE, warning=FALSE----------------------
p

## -----------------------------------------------------------------------------
data(covid_vac)

## ----labeller_covid-----------------------------------------------------------
covid_labeller <- function(label) {
  sprintf("Average daily %ss", label)
}

## ----covid, echo=TRUE, include=TRUE-------------------------------------------
# layers
p <- ggplot(covid_vac,
            aes(
              x = date,
              y = incidence / 7,
              color = interaction(outcome, status)
              )
)
p <- p + geom_step(size = .65, direction = "vh")
p <- p + geom_text(
  aes(label = status),
  stat = "pointless",
  location = "last",
  size = text_size,
  nudge_x = 5,
  hjust = "left") +
  geom_pointless(size = 3)

# facets
p <- p + facet_wrap(
  vars(outcome),
  ncol = 1,
  scales = "free_y",
  labeller = as_labeller(covid_labeller)
  )

# scales
p <- p + scale_x_date(expand = expansion(mult = c(0, 0.2)))
p <- p + scale_y_continuous(n.breaks = 4)
p <- p + scale_color_manual(
  values = c(
    "case.unvaccinated" = "#050038",
    "case.fully vaccinated" = "#9187f7",
    "death.unvaccinated" = "#f14e1c",
    "death.fully vaccinated" = "#f8a187")
  )

# title, subtitle, caption
p <- p + 
  labs(
    title = "Rates for vaccinated and unvaccinated",
    subtitle = "Per 100,000",
    x = NULL,
    y = NULL,
    caption = "Source: Centers for Disease Control and Prevention\nRates of COVID-19 Cases and Deaths by Vaccination Status, Apr 2021 to Dec 2021."
  )

# theme
p <- p + theme(panel.grid.major.x = element_blank())
p <- p + theme(strip.text = element_text(hjust = 0, face = "bold"))

## ----covid_plot, fig.height=6, echo=FALSE, warning=FALSE----------------------
p
