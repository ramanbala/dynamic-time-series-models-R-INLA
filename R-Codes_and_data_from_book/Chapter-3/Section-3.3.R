# Section 3.3: AR(1) with level plus noise model - also includes codes used in Section 3.7
# Data: Simulated
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Data simulation
sim.ar1.level <-
  simulation.from.ar(
    sample.size = 500,
    burn.in = 100,
    phi = 0.6,
    level = 1.4,
    drift = 0,
    V = 0.2,
    W = 0.1,
    plot.data = TRUE,
    seed = 123457
  )
# Code for Figure 3.7
sim.ar1.level$sim.plot
# Indexes, model formula and model execution
y.ar1.level <- sim.ar1.level$sim.data
n <- length(y.ar1.level)
id.x <- 1:n
data.ar1.level <- cbind.data.frame(y.ar1.level, id.x)
formula.ar1.level <-
  y.ar1.level ~ f(id.x, model = "ar1", constr = FALSE)
model.ar1.level <- inla(
  formula.ar1.level,
  family = "gaussian",
  data = data.ar1.level,
  control.predictor = list(compute = TRUE)
)
summary(model.ar1.level)
# Variance of the state variable
var.x.dist <-  inla.tmarginal(
  fun = function(x)
    exp(-x),
  marginal =
    model.ar1.level$internal.marginals.hyperpar$`Log precision for id`
)
var.x.zm <- inla.zmarginal(var.x.dist)
# Posterior summaries for fixed effect
format.inla.out(model.ar1.level$summary.fixed[, c(1:5)])
# HPD for level alpha
inla.hpdmarginal(0.95, model.ar1.level$marginals.fixed$`(Intercept)`)
# Alternative way to obtain posterior summaries for fixed effect
alpha.fe <-  inla.tmarginal(
  fun = function(x)
    x,
  marginal = model.ar1.level$marginals.fixed$`(Intercept)`
)
alpha.fe.zm <- inla.zmarginal(alpha.fe, silent = TRUE)
alpha.hpd.interval <- inla.hpdmarginal(p = 0.95, alpha.fe)

# Model without a level
formula.ar1.nolevel <-
  y.ar1.level ~ -1 + f(id.x, model = "ar1", constr = FALSE)
model.ar1.nolevel <- inla(
  formula.ar1.nolevel,
  family = "gaussian",
  data = data.ar1.level,
  control.predictor = list(compute = TRUE)
)
summary(model.ar1.nolevel)
