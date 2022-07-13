# Section 3.10 - Posterior sampling of latent factors and hyperparameters
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# AR(1) with level plus noise model
sim.ar1.level <-
  simulation.from.ar(
    sample.size = 500,
    burn.in = 100,
    phi = 0.6,
    level = 1.4,
    drift = 0,
    V = 0.2,
    W = 0.1,
    plot.data = FALSE,
    seed = 123457
  )
y.ar1.level <- sim.ar1.level$sim.data
n <- length(y.ar1.level)
n.hold <- 5
hold.y <- tail(y.ar1.level, n.hold)
train.y <- y.ar1.level[1:(length(y.ar1.level) - n.hold)]
# Append training data with NA
y <- train.y
y.append <- c(y, rep(NA, n.hold))
id.x <- 1:n
data.ar1.level <- cbind.data.frame(y.ar1.level, id.x)
formula.ar1.level <-
  y.ar1.level ~ f(id.x, model = "ar1", constr = FALSE)
model.ar1.level <- inla(
  formula.ar1.level,
  family = "gaussian",
  data = data.ar1.level,
  control.compute = list(config = TRUE),
  control.predictor = list(compute = TRUE)
)
summary(model.ar1.level)
# Posterior samples of hyperparameters
n.samp <- 2000
set.seed(123457)
ar1.level.hyperpar.samples <-
  inla.hyperpar.sample(n.samp, model.ar1.level, improve.marginals = TRUE)
# Code for Figure 3.15
par(mfrow = c(2, 2))
hist(
  ar1.level.hyperpar.samples[, 2],
  xlab = expression(1/Var(x[t])),
  ylab = "frequency",
  main = "",
  sub = "(a)"
)
hist(
  ar1.level.hyperpar.samples[, 3],
  xlab = expression(phi),
  ylab = "frequency",
  main = "",
  sub = "(b)"
)
hist(
  ar1.level.hyperpar.samples[, 1],
  xlab = expression(1/sigma[v]^2),
  ylab = "frequency",
  main = "",
  sub = "(c)"
)
# Posterior sample of x_t
ar1.level.latent_hyper.samples <-
  inla.posterior.sample(n.samp,
                        model.ar1.level,
                        seed = 123457,
                        num.threads = 1)
length(ar1.level.latent_hyper.samples)
model.ar1.level$misc$configs$contents
names(ar1.level.latent_hyper.samples[[1]])
# Code for Figure 3.16
p500 <-
  tsline(
    ar1.level.latent_hyper.samples[[500]]$latent[501:1000],
    line.color = "black",
    title = expression(500^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = expression(x[t])
  ) + theme(plot.title = element_text(size = 10))

p1000 <-
  tsline(
    ar1.level.latent_hyper.samples[[1000]]$latent[501:1000],
    line.color = "black",
    title = expression(1000^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = expression(x[t])
  )+ theme(plot.title = element_text(size = 10))
p1500 <-
  tsline(
    ar1.level.latent_hyper.samples[[1500]]$latent[501:1000],
    line.color = "black",
    title = expression(1500^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = expression(x[t])
  )+ theme(plot.title = element_text(size = 10))
p2000 <-
  tsline(
    ar1.level.latent_hyper.samples[[2000]]$latent[501:1000],
    line.color = "black",
    title = expression(2000^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = expression(x[t])
  )+ theme(plot.title = element_text(size = 10))
grid.arrange(p500, p1000, p1500, p2000)
# Code for Figure 3.17
latent.sample.time <- ar1.level.latent_hyper.samples %>%
  sapply(function(x)
    x$latent[501:1000])
par(mfrow = c(1, 2))
hist(latent.sample.time[, 250],
     xlab = expression(x[250]),
     ylab = "frequency",
     main = "")
hist(latent.sample.time[, 500],
     xlab = expression(x[500]),
     ylab = "frequency",
     main = "")