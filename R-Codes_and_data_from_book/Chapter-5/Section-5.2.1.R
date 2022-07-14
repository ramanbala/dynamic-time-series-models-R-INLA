# Section 5.2.1: Example - monthly average cost of nightly hotel stay
# Data: Nightly hotel stay
# Inputs: NightlyHotel.csv

rm(list = ls())
# Required packages
library(INLA)
library(astsa)
library(lubridate)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# Load data
hotels <- read.csv("Chapter-5/NightlyHotel.csv")
str(hotels)
cost <- ts(hotels[, 2])
lcost <- log(cost)
# Code for Figure 5.1
c1 <-
  tsline(
    as.numeric(cost),
    xlab = "Month",
    ylab = "cost",
    line.size = 0.6,
    line.color = "black"
  )
lc1 <-
  tsline(
    as.numeric(lcost),
    xlab = "Month",
    ylab = "log cost",
    line.size = 0.6,
    line.color = "red"
  )
grid.arrange(c1, lc1, nrow = 1)

# Fitting a DLM for lcost
n <- length(lcost)
n.hold <- 6
n.train <- n - n.hold
test.lcost <- tail(lcost, n.hold)
train.lcost <- lcost[1:n.train]
y <- train.lcost
y.append <- c(y, rep(NA, n.hold))
n.seas <- 12
trend <- 1:length(y.append)
lcost.dat <- cbind.data.frame(y = y.append,
                              trend = trend,
                              seasonal = trend)
# Model without level alpha
formula.lcost.rw1 <- y.append ~ -1 +
  f(trend, model = "rw1", constr = FALSE) +
  f(seasonal, model = "seasonal",
    season.length = n.seas)
model.lcost.rw1 <- inla(
  formula.lcost.rw1,
  family = "gaussian",
  data = lcost.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.lcost.rw1)
# Items in summary
names(summary(model.lcost.rw1))
# Code for Figure 5.2
par(mfrow = c(3, 2))
ts.plot(model.lcost.rw1$summary.random$trend$mean, ylab = "posterior mean")
ts.plot(model.lcost.rw1$summary.random$seasonal$mean, ylab = "posteriormean")
spectrum(model.lcost.rw1$summary.random$seasonal$mean, main = "")
acf(model.lcost.rw1$summary.random$seasonal$mean, main = "")
pacf(model.lcost.rw1$summary.random$seasonal$mean, main = "")
# Marginal likelihood, DIC, and WAIC
summary(model.lcost.rw1)$mlik
summary(model.lcost.rw1)$dic$dic
summary(model.lcost.rw1)$waic$waic
# Forecasts for future states
tail(model.lcost.rw1$summary.random$trend, n.hold)
tail(model.lcost.rw1$summary.random$seasonal,
     n.hold)
# Forecast of y
tail(model.lcost.rw1$summary.fitted.values, n.hold)
# MAPE and MAE
yhold <- test.lcost
yfore.lcost.rw1 <-
  tail(model.lcost.rw1$summary.fitted.values, n.hold)$mean
mape.rw1 <-
  mape(yhold, yfore.lcost.rw1) %>%
  str_remove_all("MAPE is ") %>%
  as.numeric()
mae.rw1 <-
  mae(yhold, yfore.lcost.rw1) %>%
  str_remove_all("MAE is ") %>%
  as.numeric()
# Code for Figure 5.3
fit.lcost.rw1 <- model.lcost.rw1$summary.fitted.values$mean
plot.data.rw1 <-
  as_tibble(cbind.data.frame(
    time = trend,
    lcost = lcost,
    lcost.fit = fit.lcost.rw1
  ))
p.lcost <- multiline.plot(
  plot.data.rw1,
  xlab = "Month",
  ylab = "",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
# cost
fit.cost <- exp(fit.lcost.rw1)
plot.data <-
  as_tibble(cbind.data.frame(
    time = trend,
    cost = cost,
    cost.fit = fit.cost
  ))
p.cost <- multiline.plot(
  plot.data,
  xlab = "Month",
  ylab = "",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
grid.arrange(p.lcost, p.cost, layout_matrix = cbind(1, 2))
