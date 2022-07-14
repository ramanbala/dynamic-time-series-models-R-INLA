# Section 8.3.1: Example: Dynamic aggregated model for multiple binomial response time series
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulate data
set.seed(123457)
g <- 300
n <- 1000 # original value w
burn.in <- 100
n.tot <- n + burn.in
bin.size <- sample(1:12, size = g, replace = TRUE)
alpha <- 1.3
beta.2 <- 2.0
sigma2.w0 <- 0.15
sigma2.w1 <- 0.05
phi.0 <- 0.8
phi.1 <- 0.4
# Simulation with g = 300 and n = 100
yit.all <- z1.all <- z2.all <- vector("list", g)
for (j in 1:g) {
  z1 <- rnorm(n.tot, 0, 1)
  z2 <- rnorm(n.tot, 0, 1)
  yit <-
    beta.0t <- beta.1t <- pi.t <- eta.t <- vector("double", n.tot)
  beta.0t <-
    arima.sim(list(ar = phi.0, ma = 0),
              n = n.tot,
              sd = sqrt(sigma2.w0))
  beta.1t <-
    arima.sim(list(ar = phi.1, ma = 0),
              n = n.tot,
              sd = sqrt(sigma2.w1))
  for (i in 1:n.tot) {
    eta.t[i] <- alpha + beta.0t[i] + beta.1t[i] * z1[i] + beta.2 * z2[i]
    pi.t[i] <- exp(eta.t[i]) / (1 + exp(eta.t[i]))
    yit[i] <- rbinom(n = 1,
                     size = bin.size[j],
                     prob = pi.t[i])
  }
  yit.all[[j]] <- tail(yit, n)
  z1.all[[j]] <- tail(z1, n)
  z2.all[[j]] <- tail(z2, n)
}
yit.all <- unlist(yit.all)
z1.all <- unlist(z1.all)
z2.all <- unlist(z2.all)
# Indexes and bin size
id.beta0 <- id.beta1 <- rep(1:n, g)
re.beta0 <- re.beta1 <- rep(1:g, each = n)
bin.size.all <- rep(bin.size, each = n)
data.simul.hbin <-
  cbind.data.frame(id.beta0,
                   id.beta1,
                   yit.all,
                   z1.all,
                   re.beta0,
                   re.beta1,
                   bin.size.all)
# Define priors
prior.hbin <- list(
  prec = list(prior = "loggamma", param = c(0.1, 0.1)),
  rho = list(prior = "normal", param = c(0, 0.15))
)
# Model formula and fit
formula.simul.hbin <-
  yit.all ~ 1 + z2.all +
  f(id.beta0,
    model = "ar1",
    replicate = re.beta0,
    hyper = prior.hbin) +
  f(
    id.beta1,
    z1.all,
    model = "ar1",
    replicate = re.beta1,
    hyper = prior.hbin
  )
model.simul.hbin <- inla(
  formula.simul.hbin,
  family = "binomial",
  Ntrials = bin.size.all,
  control.family = list(link = "logit"),
  data = data.simul.hbin,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.simul.hbin)