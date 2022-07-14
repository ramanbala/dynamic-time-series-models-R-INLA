# Section 5.3.1: Example: Hourly traffic volumes
# Data - hourly Interstate 94 Westbound traffic volume
rm(list = ls())
# R packages
library(INLA)
library(astsa)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(knitr)
# Source custom functions
source("functions_custom.R")
# Load data
traffic <-
  read.csv(file = "Chapter-5/Metro_Interstate_94 Traffic_Volume_Hourly.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
traffic <- as_tibble(traffic)
traffic <- traffic %>%
  mutate(date_time = as.POSIXct(
    as.character(date_time),
    origin = "1899-12-30",
    format = "%m/%d/%Y %H",
    tz = "UTC"
  ))
traffic <- traffic %>%
  mutate(
    month = month(date_time, label = TRUE),
    day = day(date_time),
    year = year(date_time),
    dayofweeek = weekdays(date_time),
    hourofday = hour(date_time),
    weeknum = week(date_time)
  )
# Code for Figure 5.7
n <- 720 * 3
y.3mth <- traffic$traffic_volume[1:n] / 1000
par(mfrow = c(2, 2))
ts.plot(y.3mth)
acf(y.3mth, 200, main = "")
spectral <- mvspec(y.3mth , fast = FALSE, main = "")
amplitude <- 2 * spectral$spec
period <- 1 / spectral$freq
plot(
  period,
  amplitude,
  type = "h",
  col = "blue",
  lwd = 2,
  main = ""
)
harmonics <- n / period
all <- cbind(period, harmonics, amplitude)
#order periodogram amplitudes
samp <- all[order(-all[, 3]),]
# Harmonics
id <- 1:n
k <- c(90, 11, 20, 47, 62, 23, 9, 34, 29, 13)
sin.mat <- cos.mat <- matrix(0, nrow = n, ncol = length(k))
for (i in 1:length(k)) {
  sin.mat[, i] <- sin(2 * pi * id * k[i] / n)
  colnames(sin.mat) <- paste("S", c(1:(length(k))), sep = "")
  cos.mat[, i] <- cos(2 * pi * id * k[i] / n)
  colnames(cos.mat) <- paste("C", c(1:(length(k))), sep = "")
}
# Exogenous predictors
holiday <- traffic$holiday[1:n]
temp <- traffic$temp[1:n]
rain <- traffic$rain_1h[1:n]
snow <- traffic$snow_1h[1:n]
clouds <- traffic$clouds_all[1:n]
weather <- traffic$weather_main[1:n]
dayofweek <- traffic$dayofweeek[1:n]
hourofday <- traffic$hourofday[1:n]
# Data frame for modeling
trend <- 1:n
y.traffic <-
  cbind.data.frame(
    y.3mth,
    id,
    trend = trend,
    sin.mat,
    cos.mat,
    holiday,
    temp,
    rain,
    snow,
    clouds,
    weather,
    dayofweek,
    hourofday
  )
# Model E1
formula.3mth.1 <-
  y.3mth ~ 1 + sin.mat + cos.mat +
  f(trend, model = "ar1", constr = FALSE)
model.3mth.1 <- inla(
  formula.3mth.1,
  family = "gaussian",
  data = y.traffic,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)
summary(model.3mth.1)
# Model E2
formula.3mth.2 <- y.3mth ~ 1 + hourofday + dayofweek +
  holiday + temp + rain + snow + clouds + weather +
  f(trend, model = "ar1", constr = FALSE)
model.3mth.2 <- inla(
  formula.3mth.2,
  family = "gaussian",
  data = y.traffic,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE
  )
)
model.3mth.2$summary.hyperpar
model.3mth.2$mlik  # marginal likelihood
# Code for Table 5.1
ms.1 <- model.selection.criteria(model.3mth.1,
                                 plot.PIT = FALSE,
                                 n.train = nrow(y.traffic))
ms.2 <- model.selection.criteria(model.3mth.2,
                                 plot.PIT = FALSE,
                                 n.train = nrow(y.traffic))
ms <-
  rbind(cbind.data.frame(Model = "Model E1", ms.1),
        cbind.data.frame(Model = "Model E2", ms.2))
knitr::kable(
  ms,
  booktabs = TRUE,
  align = "c",
  caption = 'In-sample model comparisons for the hourly traffic data.',
  digits = 3
) 