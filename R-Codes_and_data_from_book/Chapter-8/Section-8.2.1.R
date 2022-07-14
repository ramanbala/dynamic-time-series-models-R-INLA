# Section 8.2.1: Example: Simulated single binomial response series
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulation
set.seed(123457)
n <- 1000
burn.in <- 500
n.tot <- n + burn.in
bin.size <- 10
sigma2.w <- 0.05
phi <- 0.8
alpha <- 1.3
yit <- beta.t0 <- pi.t <- eta.t <- vector("double", n.tot)
beta.t0 <-
  arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w))
for (i in 1:n.tot) {
  eta.t[i] <- alpha + beta.t0[i]
  pi.t[i] <- exp(eta.t[i]) / (1 + exp(eta.t[i]))
  yit[i] <- rbinom(n = 1, size = bin.size, prob = pi.t[i])
}
yit <- tail(yit, n)
# True precision of state variable
prec.state.ar1(state.sigma2 = sigma2.w, phi = phi)
# Model fit
id.beta0 <- 1:n
data.bin <- cbind.data.frame(id.beta0, yit)
prior.bin <- list(
  prec = list(prior = "loggamma", param = c(0.1, 0.1)),
  rho = list(prior = "normal", param = c(0, 0.15))
)
formula.bin <-
  yit ~ 1 + f(id.beta0,
              model = "ar1",
              constr = FALSE,
              hyper = prior.bin)
model.bin <- inla(
  formula.bin,
  family = "binomial",
  Ntrials = bin.size,
  control.family = list(link = "logit"),
  data = data.bin,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.bin)
# Model with no alpha
formula.bin.noalpha <-
  yit ~ -1 + f(id.beta0,
               model = "ar1",
               constr = FALSE)
model.bin.noalpha <- inla(
  formula.bin.noalpha,
  family = "binomial",
  Ntrials = bin.size,
  control.family = list(link = "logit"),
  data = data.bin,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
model.bin.noalpha$summary.hyperpar
