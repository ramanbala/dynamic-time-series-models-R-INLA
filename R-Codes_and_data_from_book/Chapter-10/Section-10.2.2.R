# Section 10.2.2: Example: Simulated SV data with Student-tν errors
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulate data
set.seed(1234)
n <- 1000
phi <- 0.9
mu <- -2.0
h <- rep(mu, n)
sigma2.w <- 0.25
prec.x <- prec.state.ar1(sigma2.w, phi)
for (i in 2:n) {
  h[i] <- mu + phi * (h[i - 1] - mu) + rnorm(1, 0, sqrt(sigma2.w))
}
ret <- exp(h / 2) * rt(n, df = 5)
# Indexes, data frame
time <- 1:n
data.sim.sv <- cbind.data.frame(ret = ret, time = time)
# Model formula and fit
formula.sim.sv <- ret ~ 0 + f(time,
                              model = "ar1",
                              hyper = list(prec = list(param = c(1, 0.0001)),
                                           mean = list(fixed = FALSE)))
model.sim.t <-
  inla(
    formula.sim.sv,
    family = "stochvol_t",
    data = data.sim.sv,
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = TRUE
    ),
  )
summary(model.sim.t)
