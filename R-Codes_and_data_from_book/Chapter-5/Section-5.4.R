# Section 5.4: Latent AR(1) model with covariates plus noise
# Data: Simulated
rm(list = ls())
# R packages
library(INLA)
library(astsa)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(knitr)
# Source custom functions
source("functions_custom.R")
# Data simulation
y.ar1c <-
  simulation.from.ar1c(
    sample.size = 500,
    burn.in = 100,
    phi.ar1 = 0.6,
    ncol.z = 2,
    beta.z = c(2,-0.75),
    V = 0.5,
    W = 0.1,
    plot.data = FALSE,
    seed = 123457
  )
y <- y.ar1c$sim.data$y
Z <- y.ar1c$sim.data$Z
# Model formula and fit
idx <- 1:length(y)
Q <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
data.ar1c <- cbind.data.frame(y, idx)
formula.ar1c <- y ~ -1 + f(
  idx,
  model = "ar1c",
  args.ar1c = list(Z = Z,
                   Q.beta = Q))
model.ar1c <- inla(
  formula.ar1c,
  data = data.frame(y, idx),
  family = "gaussian",
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.ar1c)
