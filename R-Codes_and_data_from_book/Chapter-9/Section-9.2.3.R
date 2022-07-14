# Section 9.2.3: Example: Daily bike rentals
# Data: Daily bike sharing customers and weather in the metro DC area
# Inputs: bikeusage_daily.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Read data
bike <- read.csv("Chapter-9/bikeusage_daily.csv")
n <- nrow(bike)
n.hold <- 7
n.train <- n - n.hold
test.data <- tail(bike$Total.Users, n.hold)
train.data <- bike[1:n.train,]
# Index and dataframe
n.seas <- s <-  7
y <- train.data$Total.Users
y.append <- c(y, rep(NA, n.hold))
id <- id.trend <-  id.seas <- 1:length(y.append)
bike.dat <-
  cbind.data.frame(id,
                   id.trend,
                   id.seas,
                   y.append,
                   select(bike, Temperature.F:Wind.Speed))
# Model formula and fit
bike.formula <- y.append ~  Temperature.F + Humidity + Wind.Speed +
  f(id.trend, model = "rw1") +
  f(id.seas, model = "seasonal", season.length = n.seas)
bike.model <- inla(
  bike.formula,
  family = "poisson",
  data = bike.dat,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.family = list(link = "log")
)
summary(bike.model)
# Code for Figure 9.5
fit.bike <- bike.model$summary.fitted.values$mean
fit.bike.df <-
  cbind.data.frame(
    time = 1:n,
    obs.bike = bike$Total.Users,
    fit.bike = fit.bike
  )
multiline.plot(
  fit.bike.df,
  line.size = 0.5,
  xlab = "t",
  ylab = "bike counts",
  line.color = c("red", "blue")
) +
  theme(legend.position = "none")
# MAE for holdout data
yfore <- tail(fit.bike, n.hold)
mae(test.data, round(yfore))
