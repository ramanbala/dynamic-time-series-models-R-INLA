# Section 12.2.2: Example: Simulated bivariate AR(1) series
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(Matrix)
library(mvtnorm)
library(marima)
library(vars)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulate data
# Observation error covariance V
V.11 <- 1.0
V.22 <- 1.0
R.12 <- 0.7
V.12 <- R.12 * sqrt(V.11) * sqrt(V.22)
V <- matrix(c(V.11, V.12, V.12, V.22), nrow = 2)
# State error covariance W
W.11 <- 0.5
W.22 <- 0.5
W.12 <- 0
W <- matrix(c(W.11, W.12, W.12, W.22), nrow = 2)
# Precision of state variables
phi.1 <- 0.8
phi.2 <- 0.4
prec.x1 <- (1 - phi.1 ^ 2) / W.11
prec.x2 <- (1 - phi.2 ^ 2) / W.22
# Generate data
set.seed(123457)
n <- 1000
x1 <- arima.sim(n = 1000, list(order = c(1, 0, 0), ar = phi.1),
                sd = sqrt(W.11))
x2 <- arima.sim(n = 1000, list(order = c(1, 0, 0), ar = phi.2),
                sd = sqrt(W.22))
err.v <- rmvnorm(n, mean = c(0, 0), sigma = V)
err.v1 <- err.v[, 1]
err.v2 <- err.v[, 2]
y1 <- x1 + err.v1
y2 <- x2 + err.v2
y <- c(y1, y2)
# Code for Figure 12.1
ccf(
  y1,
  y2,
  main = "",
  ylim = c(-1, 1),
  ylab = "ccf",
  xlab = "lag"
)
# Indexes and data frame
m <- n - 1
N <- 2 * n
id.V <- 1:N
id.b0.x1 <- c(1:n, rep(NA, n))
id.b0.x2 <- c(rep(NA, n), 1:n)
biv.dat <- cbind.data.frame(y, id.b0.x1, id.b0.x2, id.V)
# Model formula and fit
formula.biv <-  y ~ 1 +
  f(id.V, model = "iid2d", n = N) +
  f(id.b0.x1, model = "ar1") +
  f(id.b0.x2, model = "ar1")
result.biv <- inla(
  formula.biv,
  family = c("gaussian"),
  data = biv.dat,
  control.inla = list(h = 1e-5, tolerance = 1e-3),
  control.compute = list(dic = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE),
  control.family = list(hyper = list(prec = list(
    initial = 12, fixed = TRUE
  )))
)
summary(result.biv)