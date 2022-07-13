# Section 3.2: Random walk plus noise model
# Data: Simulated

rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Data simulation
sim.rw1 <-
  simulation.from.rw1(
    sample.size = 500,
    burn.in = 100,
    level = 0,
    drift = 0,
    V = 0.5,
    W = 0.25,
    plot.data = TRUE,
    seed = 123457
  )
# Code for Figure 3.1
sim.rw1$sim.plot
# Section 3.2.1 R-INLA model formula
y.rw1 <- sim.rw1$sim.data
n <- length(y.rw1)
id.w <- 1:n
rw1.dat <- cbind.data.frame(y.rw1, id.w)
# Section 3.2.2 Model execution
formula.rw1 <- y.rw1 ~ f(id.w, model = "rw1",
                         constr = FALSE) - 1
model.rw1 <- inla(
  formula.rw1,
  family = "gaussian",
  data = rw1.dat,
  control.predictor = list(compute = TRUE)
)
summary(model.rw1)
# Section 3.2.4 Posterior distributions of hyperparameters
sigma2.v.dist <- inla.tmarginal(
  fun = function(x)
    exp(-x),
  marginal = model.rw1$internal.marginals.hyperpar$`Log precision for the Gaussian observations`
)
# Code for Figure 3.2
plot(
  sigma2.v.dist,
  type = "l",
  xlab = expression(paste(
    "Observation noise variance ", sigma[v] ^ 2, sep = " "
  )),
  ylab = "density"
)
# Code for Figure 3.3
sigma2.w.dist <- inla.tmarginal(
  fun = function(x)
    exp(-x),
  marginal = model.rw1$internal.marginals.hyperpar$`Log precision for id`
)
plot(
  sigma2.w.dist,
  type = "l",
  xlab = expression(paste("State noise variance ", sigma[w] ^ 2, sep = " ")),
  ylab = "density"
)
# Posterior expectations
sigma2.v.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1$internal.marginals.hyperpar$`Log precision for the Gaussian observations`
  )
sigma2.w.hat <-
  inla.emarginal(
    fun = function(x)
      exp(-x),
    marginal = model.rw1$internal.marginals.hyperpar$`Log precision for id`
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
))
# Section 3.2.5 Fitted values for latent states and responses
head(model.rw1$summary.random$id.w)
# Code for Figure 3.4
plot.data <- model.rw1$summary.random$id %>%
  select(time = ID, mean,
         "0.025quant", "0.975quant")
multiline.plot(
  plot.data,
  xlab = "t",
  ylab = "posterior mean",
  line.color = c("black", "red", "red"),
  line.size = 0.6
)
# Code for Figure 3.5
fit <- model.rw1$summary.linear.predictor$mean
df <- as_tibble(cbind.data.frame(time = 1:n, y.rw1, fit))
multiline.plot(
  plot.data = df,
  title = "",
  xlab = "t",
  ylab = "y",
  line.type = "dashed",
  line.color = c("red", "blue"),
  line.size = 0.6
)
# Alternate ways to obtain fitted values
fit.alt.1 <- model.rw1$summary.fitted.values$mean
fit.alt.2 <- model.rw1$marginals.linear.predictor %>%
  sapply(function(x)
    inla.emarginal(
      fun = function(y)
        y,
      marginal = x
    ))
# Section 3.2.6 Filtering and smoothing in DLM
out <-
  filt.inla(
    data.series = y.rw1,
    model = "rw1",
    trend = "no",
    alpha = "no"
  )
# Plot for Figure 3.6
filt.all.rw1 <- out$filt.all.bind
filt.estimate.rw1 <- c(NA, filt.all.rw1$mean)
smooth.est <- model.rw1$summary.random$id$mean
plot.df <- as_tibble(cbind.data.frame(
  time = id.w,
  y = y.rw1,
  filter = filt.estimate.rw1,
  smooth = smooth.est
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = " ",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "yellow", "green")
)