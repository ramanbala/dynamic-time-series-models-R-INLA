# Section 6.2.2: Example - Simulated homogeneous panel time series with different levels
# Data - simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)

# Source custom functions
source("functions_custom.R")

# Code for Case-1: g = 10 and alpha_i are fixed effects
set.seed(123457)
g <- 10
n <- 250
burn.in <- 500
n.tot <- n + burn.in
alpha <- 10.0
sigma2.w0 <- 0.25
sigma2.v0 <- 1.0
phi.0 <- 0.8
# Response and predictors
yit.all <-
  yit.all.list <-
  z1.all <-
  z2.all <- alpha <- z.all <- sigma2.w <- vector("list", g)
for (j in 1:g) {
  yit <- beta.1t <- z <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  us <- runif(1, min = -0.15, max = 0.25)
  sigma2.w[[j]] <- sigma2.w0 + us
  beta.1t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w[[j]]))
  alpha[[j]] <- sample(seq(-2, 2, by = 0.1), 1)
  z <- rnorm(n.tot)
  v <- rnorm(n.tot)
  for (i in 1:n.tot) {
    yit[i] <- alpha[[j]] + beta.1t[i] * z[i] + v[i]
  }
  yit.all.list[[j]] <- tail(yit, n)
  z.all[[j]] <- tail(z, n)
}
yit.all <- unlist(yit.all.list)
z.all <- unlist(z.all)
# Indexes
id.beta1 <- rep(1:n, g)
re.beta1 <- id.alpha <- rep(1:g, each = n)
fact.alpha <- as.factor(id.alpha)
data.panelts.2 <-
  cbind.data.frame(id.beta1, z.all, yit.all, re.beta1, fact.alpha)
formula.panelts.2 <-
  yit.all ~ -1 + fact.alpha +
  f(id.beta1, z.all, model = "ar1", replicate = re.beta1)
# Model
model.panelts.2 <- inla(
  formula.panelts.2,
  family = "gaussian",
  data = data.panelts.2,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
model.panelts.2$summary.fixed
model.panelts.2$summary.hyperpar

# Code for Case-2: g = 100 and alpha_i are random
set.seed(123457)
g <- 100
n <- 250
burn.in <- 500
n.tot <- n + burn.in
sigma2.w0 <- 0.25
sigma2.v0 <- 1.0
phi.0 <- 0.8
# Response and predictors
yit.all <-
  yit.all.list <-
  z1.all <-
  z2.all <- alpha <- z.all <- sigma2.w <- vector("list", g)
for (j in 1:g) {
  yit <- beta.1t  <- z <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  us <- runif(1, min = -0.15, max = 0.25)
  sigma2.w[[j]] <- sigma2.w0 + us
  beta.1t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w[[j]]))
  alpha[[j]] <- sample(seq(-2, 2, by = 0.1), 1)
  z <- rnorm(n.tot)
  v <- rnorm(n.tot)
  for (i in 1:n.tot) {
    yit[i] <- alpha[[j]] + beta.1t[i]  * z[i] + v[i]
  }
  yit.all.list[[j]] <- tail(yit, n)
  z.all[[j]] <- tail(z, n)
}
yit.all <- unlist(yit.all.list)
z.all <- unlist(z.all)
# Indexes
id.beta1 <- rep(1:n, g)
re.beta1 <- id.alpha <- rep(1:g, each = n)
data.panelts.3 <-
  cbind.data.frame(id.beta1, z.all, yit.all, re.beta1, id.alpha)
formula.panelts.3 <-
  yit.all ~ -1 + f(id.alpha, model = "iid") +
  f(id.beta1, z.all, model = "ar1", replicate = re.beta1)
# Model
model.panelts.3 <- inla(
  formula.panelts.3,
  family = "gaussian",
  data = data.panelts.3,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.panelts.3)
model.panelts.3$summary.hyperpar
1 / var(unlist(alpha))
