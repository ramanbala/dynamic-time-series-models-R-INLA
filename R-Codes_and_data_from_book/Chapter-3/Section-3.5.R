# Section 3.5: Random walk with drift plus noise model
# Data: Simulated
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Data simulation
sim.rw1.drift <-
  simulation.from.rw1(
    sample.size = 500,
    burn.in = 100,
    level = 0,
    drift = 0.1,
    V = 0.5,
    W = 0.05,
    plot.data = TRUE,
    seed = 1
  )
# Code for Figure 3.11
sim.rw1.drift$sim.plot
# Method-1 implementation
y.rw1.drift <- sim.rw1.drift$sim.data
n <- length(y.rw1.drift)
id.w <- id.delta <- 1:n
rw1.drift.dat <- cbind.data.frame(y.rw1.drift, id.w, id.delta)
formula.rw1.drift.1 <-
  y.rw1.drift ~ f(id.w, model = "rw1", constr = FALSE) - 1 + id.delta
model.rw1.drift.1 <- inla(
  formula.rw1.drift.1,
  family = "gaussian",
  data = rw1.drift.dat,
  control.predictor = list(compute = TRUE)
)
summary(model.rw1.drift.1)
# Method-2 implementation
formula.rw1.drift.2 <-
  y.rw1.drift ~ f(id.w, model = "rw1", constr = FALSE) - 1 +
  f(
    id.delta,
    model = "linear",
    mean.linear = 0,
    prec.linear = 0.001
  )
model.rw1.drift.2 <- inla(
  formula.rw1.drift.2,
  family = "gaussian",
  data = rw1.drift.dat,
  control.predictor = list(compute = TRUE)
)
summary(model.rw1.drift.2)
# Posterior estimates
sigma2.v.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1.drift.1$internal.marginals.hyperpar$`Log precision for the Gaussian observations`
  )
sigma2.w.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1.drift.1$internal.marginals.hyperpar$`Log precision for id.w`
  )
cat(paste(
  "Estimated observation noise variance, sigma2.v",
  round(sigma2.v.hat, 2),
  sep = " = "
),
"\n")
cat(paste(
  "Estimated state noise variance, sigma2.w",
  round(sigma2.w.hat, 2),
  sep = " = "
),
"\n")
delta.hat <- model.rw1.drift.1$summary.fixed$mean
cat(paste("Estimated drift, delta", round(delta.hat, 2), sep = " = "), "\n")