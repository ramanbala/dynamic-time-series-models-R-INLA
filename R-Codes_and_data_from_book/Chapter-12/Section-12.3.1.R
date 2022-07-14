# Section 12.3.1: Example: Simulated trivariate series
# Data: Simulated trivariate series
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
library(mvtnorm)
library(marima)
# Source custom functions
source("functions_custom.R")
# Simulate data
# Observation error covariance V
V.11 <- 1.0
V.22 <- 1.0
V.33 <- 1.0
R.12 <- 0.7
R.13 <- 0.4
R.23 <- 0.25
V.12 <- R.12 * sqrt(V.11) * sqrt(V.22)
V.13 <- R.13 * sqrt(V.11) * sqrt(V.33)
V.23 <- R.23 * sqrt(V.22) * sqrt(V.33)
V <- matrix(c(V.11, V.12, V.13, V.12, V.22, V.23,
              V.13, V.23, V.33), nrow = 3)
# State error covariance W
sig2.w <- 0.1
rho.w <- 0.75
W.11 <- sig2.w
W.22 <- sig2.w
W.33 <- sig2.w
W.12 <- sig2.w * rho.w
W.13 <- sig2.w * rho.w
W.23 <- sig2.w * rho.w
W <- matrix(c(W.11, W.12, W.13, W.12, W.22, W.23,
              W.13, W.23, W.33), nrow = 3)
# Phis
phi.1 <- -0.5
phi.2 <- -0.5
phi.3 <- -0.5
# Set up Phi as be a 3-dim array
Phi <- array(0, dim = c(3, 3, 2))
Phi[1, 1, 1] <- 1.0
Phi[2, 2, 1] <- 1.0
Phi[3, 3, 1] <- 1.0
Phi[1, 1, 2] <- phi.1
Phi[2, 2, 2] <- phi.2
Phi[3, 3, 2] <- phi.3
# Simulate states from a VAR(1) model using the marima package
n <- n.sim <- 1000
q <- 3
n.start <- 0
avg <- rep(0, 3)
X <-
  marima.sim(
    3,
    ar.model = Phi,
    ar.dif = NULL,
    ma.model = NULL,
    averages = rep(0, 3),
    resid.cov = W,
    seed = 1234,
    nstart = n.start,
    nsim = n.sim
  )
x1 <- X[, 1]
x2 <- X[, 2]
x3 <- X[, 3]
# Code for Figure 12.5
par(mfrow = c(1, 3))
acf(x1, main = "", sub = "ACF of x1")
acf(x2, main = "", sub = "ACF of x2")
acf(x3, main = "", sub = "ACF of x3")
# Generate observation errors
set.seed(123457)
err.v <- rmvnorm(n.sim, mean = c(0, 0, 0), sigma = V)
err.v1 <- err.v[, 1]
err.v2 <- err.v[, 2]
err.v3 <- err.v[, 3]
# Add observation errors to states to get responses
y1 <- x1 + err.v1
y2 <- x2 + err.v2
y3 <- x3 + err.v3
y <- c(y1, y2, y3)
# Code for Figure 12.6
par(mfrow = c(2, 3))
ccf(x1,
    x2,
    ylab = "CCF",
    main = "",
    sub = "CCF of (x1,x2)")
ccf(x1,
    x3,
    ylab = "CCF",
    main = "",
    sub = "CCF of (x1,x3)")
ccf(x2,
    x3,
    ylab = "CCF",
    main = "",
    sub = "CCF of (x2,x3)")
ccf(y1, y2, main = "", sub = "CCF of y1 and y2")
ccf(y1, y3, main = "", sub = "CCF of y1 and y3")
ccf(y2, y3, main = "", sub = "CCF of y2 and y3")
# Indexes and data frame
m <- n - 1
N <- 3 * n
id.V <- 1:N
id.b0 <- c(1:n, 1:n, 1:n)
id.b0.g <- rep(1:3, each = n)
triv.dat <- cbind.data.frame(y, id.b0, id.b0.g, id.V)
# Model formula and fit
formula.triv <-  y ~ 1 +
  f(id.V, model = "iid3d", n = N) +
  f(id.b0,
    model = "ar1",
    constr = FALSE,
    group = id.b0.g)
result.triv <-
  inla(
    formula.triv,
    family = c("gaussian"),
    data = triv.dat,
    control.inla = list(h = 1e-5, tolerance = 1e-3),
    control.compute = list(dic = TRUE, config = TRUE),
    control.predictor = list(compute = TRUE),
    control.family = list(hyper = list(prec = list(
      initial = 12, fixed = TRUE
    )))
  )
summary(result.triv)