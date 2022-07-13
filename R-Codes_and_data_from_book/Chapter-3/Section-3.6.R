# Section 3.6: Second-order polynomial model
# Data: Simulated
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Data simulation
sim.secondorder <-
  simulation.from.second.order.dlm(
    sample.size = 200,
    V = 0.01,
    W1 = 1e-04,
    W2 = 1e-04,
    plot.data = TRUE,
    seed = 123457
  )
# Code for Figure 3.12
sim.secondorder$sim.plot
# Augmented model - indexes
y.sec <- sim.secondorder$sim.data
n <- length(y.sec)
m <- n - 1
Y <- matrix(NA, n + 2 * m, 3)
# actual observations (y.sec)
Y[1:n, 1] <- y.sec
Y[1:m + n, 2] <- 0
Y[1:m + (n + m), 3] <- 0
# Indexes
i = c(1:n, 2:n, rep(NA, m))              # indices for x_t
j = c(rep(NA, n), 2:n - 1, rep(NA, m))     # indices for x_{t-1}
cj = c(rep(NA, n), rep(-1, m), rep(NA, m)) # weights for x_{t-1}
l       = c(rep(NA, n + n - 1), 2:n)         # indices for delta_t
cl      = c(rep(NA, n + m), rep(1, m))      # weights for delta_t
k       = c(rep(NA, n), 2:n - 1, 2:n - 1)  # indices for delta_{t-1}
ck      = c(rep(NA, n), rep(-1, 2 * m))     # weights for delta_{t-1}
q       = c(rep(NA, n), 1:m, rep(NA, m))  # indices for w_{t,1}
s       = c(rep(NA, n + m), 1:m)           # indices for  w_{t,2}
cq      = c(rep(NA, n), rep(-1, m), rep(NA, m)) # weights for w_{t,1}
cs      = c(rep(NA, n + m), rep(-1, m))          # weights for w_{t,2}

# INLA formula and model 
formula.second.poly = Y ~ f(i,
                            model = "iid",
                            initial = -10,
                            fixed = TRUE) +
  f(j, cj, copy = "i") +
  f(
    l,
    cl,
    model = "iid",
    values = 1:n,
    initial = -10,
    fixed = TRUE
  ) +
  f(k, ck, copy = "l") +
  f(q, cq, model = "iid") +
  f(s, cs, model = "iid") - 1
model.second.poly = inla(
  formula.second.poly,
  data = data.frame(i, j, cj, k, ck, l, q, s, cq, cs),
  family = rep("gaussian", 3),
  control.family = list(list(), list(initial = 10, fixed =
                                       TRUE), list(initial = 10, fixed = TRUE)),
  control.predictor = list(compute = TRUE)
)
# Model summary
summary(model.second.poly)
# Posterior marginals
post.mean.V <- inla.emarginal(
  fun = function(x)
    exp(-x),
  marginal = model.second.poly$internal.marginals.hyperpar$`Log precision for the Gaussian observations`
)
post.mean.W11 <- inla.emarginal(
  fun = function(x)
    exp(-x),
  marginal = model.second.poly$internal.marginals.hyperpar$`Log precision for s`
)
post.mean.W22 <- inla.emarginal(
  fun = function(x)
    exp(-x),
  marginal = model.second.poly$internal.marginals.hyperpar$`Log precision for q`
)
