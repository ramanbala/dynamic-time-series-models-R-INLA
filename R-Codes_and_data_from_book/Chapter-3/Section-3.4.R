# Section 3.4: Dynamic linear models with higher order AR lags
# Data: Simulated
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Data simulation of AR(2) with level plus noise model
sim.ar2 <-
  simulation.from.ar(
    sample.size = 500,
    burn.in = 100,
    phi = c(1.5,-0.75),
    level = 10,
    drift = 0,
    V = 1.25,
    W = 0.05,
    plot.data = TRUE,
    seed = 123457
  )
# Code for Figure 3.8
sim.ar2$sim.plot
# Indexes, model formula and model execution
y.ar2 <- sim.ar2$sim.data
n <- length(y.ar2)
id.x <- 1:n
ar2.dat <- cbind.data.frame(y.ar2, id.x)
formula.ar2 <- y.ar2 ~  f(id.x,
                          model = "ar",
                          order = 2,
                          constr = FALSE) 
model.ar2 <- inla(
  formula.ar2,
  family = "gaussian",
  data = ar2.dat,
  control.predictor = list(compute = TRUE)
)
summary(model.ar2)
# Estimate phi coefficients
pacf <- model.ar2$summary.hyperpar$mean[3:4]
phi <- inla.ar.pacf2phi(pacf)
print(phi)
# Estimate of state noise variance
phi <- c(1.5, -0.75)
psiwt.ar2 <- ARMAtoMA(ar = phi, ma = 0, 100)
precision.x.hat <- model.ar2$summary.hyperpar$mean[2]
sigma2.w.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.ar2$internal.marginals.hyperpar$`Log precision for id`
  ) / sum(psiwt.ar2 ^ 2)
cat(paste(
  "Estimated state noise variance, sigma2.w",
  round(sigma2.w.hat, 2),
  sep = " = "
),
"\n")
# Posterior marginals of phi_1 and phi_2 using inla.hyperpar.sample()
n.samples <- 10000
pacfs <- inla.hyperpar.sample(n.samples, model.ar2)[, 3:4]
phis <- apply(pacfs, 1L, inla.ar.pacf2phi)
# Code for Figure 3.9
par(mfrow = c(1, 2))
plot(
  density(phis[1, ]),
  type = "l",
  main="",
  xlab = expression(phi[1]),
  ylab = "density"
)
abline(v = 1.5)
plot(
  density(phis[2, ]),
  type = "l",
  main="",
  xlab = expression(phi[2]),
  ylab = "density"
)
abline(v = -0.75)
# Code for Figure 3.10
pacf1.coeff <-
  inla.tmarginal(
    fun = function(x)
      2 * exp(x) / (1 + exp(x)) - 1,
    marginal = model.ar2$internal.marginals.hyperpar$`Intern PACF1 for id`
  )
pacf1.coeff.zm <- inla.zmarginal(pacf1.coeff)
pacf2.coeff <-
  inla.tmarginal(
    fun = function(x)
      2 * exp(x) / (1 + exp(x)) - 1,
    marginal = model.ar2$internal.marginals.hyperpar$`Intern PACF2 for id`
  )
pacf2.coeff.zm <- inla.zmarginal(pacf2.coeff)
par(mfrow = c(1, 2))
plot(
  pacf1.coeff,
  type = "l",
  main = "",
  xlab = "",
  ylab = ""
)
plot(
  pacf2.coeff,
  type = "l",
  main  = "",
  xlab = "",
  ylab = ""
)
