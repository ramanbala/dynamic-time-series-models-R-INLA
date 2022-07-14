# Section 9.3.1: Example: Simulated hierarchical univariate Poisson counts
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulate data
# Case 1. Common level model
set.seed(123457)
g <- 300
n <- 100
burn.in <- 500
n.tot <- n + burn.in
sigma2.w <- 0.25
phi.0 <- 0.8
alpha <- 1.3
yit.all <- yit.all.list <- z1.all <- z2.all <- vector("list", g)
for (j in 1:g) {
  yit <- beta.0t <- lambda.t <- eta.t <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  beta.0t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w))
  for (i in 1:n.tot) {
    eta.t[i] <- alpha + beta.0t[i]
    lambda.t[i] <- exp(eta.t[i])
    yit[i] <- rpois(1, lambda.t[i])
  }
  yit.all.list[[j]] <- tail(yit, n)
}
yit.all <- unlist(yit.all.list)
# Indexes and data frame
id.beta0 <- rep(1:n, g)
re.beta0 <- rep(1:g, each = n)
data.hpois.alpha.1 <-
  cbind.data.frame(id.beta0, yit.all, re.beta0)
# Model formula and fit
formula.hpois.alpha.1 <-
  yit.all ~ 1 + f(id.beta0, model = "ar1", replicate = re.beta0)
model.hpois.alpha.1 <- inla(
  formula.hpois.alpha.1,
  family = "poisson",
  data = data.hpois.alpha.1,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.hpois.alpha.1)
summary.hpois.alpha.1 <- summary(model.hpois.alpha.1)
# Model with non-default prior
prior.hpois <- list(
  prec = list(prior = "loggamma", param = c(0.1, 0.1)),
  rho = list(prior = "normal", param = c(0, 0.15))
)
formula.hpois.alpha.1.nd.prior <-
  yit.all ~ 1 + f(id.beta0,
                  model = "ar1",
                  replicate = re.beta0,
                  hyper = prior.hpois)
model.hpois.alpha.1.nd.prior <- inla(
  formula.hpois.alpha.1.nd.prior,
  family = "poisson",
  data = data.hpois.alpha.1,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.hpois.alpha.1.nd.prior)
model.hpois.alpha.1.nd.prior$summary.fixed
# Case 2. Model with different fixed levels
# Simulation
set.seed(123457)
g <- 10
n <- 100
burn.in <- 500
n.tot <- n + burn.in
sigma2.w0 <- 0.25
phi.0 <- 0.8
yit.all <-
  yit.all.list <-
  z1.all <-
  z2.all <- alpha <- z.all <- sigma2.w <- vector("list", g)
for (j in 1:g) {
  yit <- beta.1t <- lambda.t <- eta.t <- z <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  us <- runif(1, min = -0.15, max = 0.25)
  sigma2.w[[j]] <- sigma2.w0 + us
  beta.1t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w[[j]]))
  alpha[[j]] <- sample(seq(-2, 2, by = 0.1), 1)
  z <- rnorm(n.tot)
  for (i in 1:n.tot) {
    eta.t[i] <- alpha[[j]] + beta.1t[i] * z[i]
    lambda.t[i] <- exp(eta.t[i])
    yit[i] <- rpois(1, lambda.t[i])
  }
  yit.all.list[[j]] <- tail(yit, n)
  z.all[[j]] <- tail(z, n)
}
yit.all <- unlist(yit.all.list)
z.all <- unlist(z.all)
alpha.all <- unlist(alpha)
# Indexes and replicates
id.beta1 <- rep(1:n, g)
re.beta1 <- id.alpha <- rep(1:g, each = n)
fact.alpha <- as.factor(id.alpha)
# Model formula and fit
formula.hpois.alpha.2 <-
  yit.all ~ -1 + fact.alpha +
  f(id.beta1, z.all, model = "ar1", replicate = re.beta1)
data.hpois.alpha.2 <-
  cbind.data.frame(id.beta1, z.all, yit.all, re.beta1, fact.alpha)
model.hpois.alpha.2 <- inla(
  formula.hpois.alpha.2,
  family = "poisson",
  data = data.hpois.alpha.2,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.hpois.alpha.2)
alpha.all
#Case 3. Model with different random levels
# Data simulation
yit.all <-
  yit.all.list <-
  z1.all <-
  z2.all <- alpha <- z.all <- sigma2.w <- vector("list", g)
for (j in 1:g) {
  yit <- beta.1t <- lambda.t <- eta.t <- z <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  us <- runif(1, min = -0.15, max = 0.25)
  sigma2.w[[j]] <- sigma2.w0 + us
  beta.1t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w[[j]]))
  alpha[[j]] <- sample(seq(-2, 2, by = 0.1), 1)
  z <- rnorm(n.tot)
  for (i in 1:n.tot) {
    eta.t[i] <- alpha[[j]] + beta.1t[i] * z[i]
    lambda.t[i] <- exp(eta.t[i])
    yit[i] <- rpois(1, lambda.t[i])
  }
  yit.all.list[[j]] <- tail(yit, n)
  z.all[[j]] <- tail(z, n)
}
yit.all <- unlist(yit.all.list)
z.all <- unlist(z.all)
# Indexes
id.beta1 <- rep(1:n, g)
re.beta1 <- id.alpha <- rep(1:g, each = n)
prec.prior <- list(prec = list(param = c(0.001, 0.001)))
# Model formula and fit
formula.hpois.alpha.3 <-
  yit.all ~ -1 + f(id.alpha, model = "iid") +
  f(id.beta1, z.all, model = "ar1", replicate = re.beta1)
data.hpois.alpha.3 <-
  cbind.data.frame(id.beta1, z.all, yit.all, re.beta1, id.alpha)
model.hpois.alpha.3 <- inla(
  formula.hpois.alpha.3,
  family = "poisson",
  data = data.hpois.alpha.3,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.hpois.alpha.3)