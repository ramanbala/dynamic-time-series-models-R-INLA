# Section 6.2.1: Example - Simulated homogeneous panel time series with the same level
# Data - simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)

# Source custom functions
source("functions_custom.R")
set.seed(123457)
g <- 100
n <- 250
burn.in <- 500
n.tot <- n + burn.in
alpha <- 10.0
sigma2.w0 <- 0.25
sigma2.v0 <- 1.0
phi.0 <- 0.8
# Generate response and predictors
yit.all <-
  yit.all.list <-
  z1.all <- z2.all <- z.all <- sigma2.w <- vector("list", g)
for (j in 1:g) {
  yit <- beta.1t <- z <- vector("double", n.tot)
  u <- runif(1, min = -0.1, max = 0.05)
  phi <- phi.0 + u
  us <- runif(1, min = -0.15, max = 0.25)
  sigma2.w[[j]] <- sigma2.w0 + us
  beta.1t <-
    arima.sim(list(ar = phi, ma = 0), n = n.tot, sd = sqrt(sigma2.w[[j]]))
  z <- rnorm(n.tot)
  v <- rnorm(n.tot)
  for (i in 1:n.tot) {
    yit[i] <- alpha + beta.1t[i] * z[i] + v[i]
  }
  yit.all.list[[j]] <- tail(yit, n)
  z.all[[j]] <- tail(z, n)
}
yit.all <- unlist(yit.all.list)
z.all <- unlist(z.all)
# Indexes
id.beta1 <- rep(1:n, g)
re.beta1 <- rep(1:g, each = n)
data.panelts.1 <-
  cbind.data.frame(id.beta1, z.all, yit.all, re.beta1)
formula.panelts.1 <-
  yit.all ~ 1 +
  f(id.beta1, z.all, model = "ar1", replicate = re.beta1)
# Model
model.panelts.1 <- inla(
  formula.panelts.1,
  family = "gaussian",
  data = data.panelts.1,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
model.panelts.1$summary.fixed
model.panelts.1$summary.hyperpar
