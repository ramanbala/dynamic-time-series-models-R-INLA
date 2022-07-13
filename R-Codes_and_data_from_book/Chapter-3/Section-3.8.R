# Section 3.8 - Model comparisons
rm(list = ls())
# Required Packages
library(tidyverse)
library(INLA)
# Source custom functions
source("functions_custom.R")
# Section 3.8.1 In-sample model comparisons
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
y.rw1 <- sim.rw1$sim.data
n <- length(y.rw1)
id.w <- 1:n
rw1.dat <- cbind.data.frame(y.rw1, id.w)
# Model formula and execution
formula.rw1 <- y.rw1 ~ f(id.w, model = "rw1",
                         constr = FALSE) - 1
model.rw1 <- inla(
  formula.rw1,
  family = "gaussian",
  data = rw1.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)
# DIC
model.rw1$dic$dic
# CPO
cpo.rw1 <- model.rw1$cpo$cpo
# PSBF
psBF.rw1 <- sum(log(cpo.rw1))
#PIT
pit.rw1 <- model.rw1$cpo$pit
# Code for Figure 3.14
# Marginal likelihood
model.rw1$mlik
hist(pit.rw1,main =" ", ylab="frequency")
psBF.rw1