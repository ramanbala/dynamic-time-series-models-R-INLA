# Section 9.2.2: Example: Modeling crash counts in CT
# Data: Crashes on state-maintained roads in Connecticut
# Inputs: crashcount_rle.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Read data
dat <-
  read.csv(file = "Chapter-9/crashcount_rle.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
n <- nrow(dat)
# Code for Figure 9.3
kabct <- dat$KAB_FREQ
vmt <-  log(dat$vmt_exp)
tsline(
  kabct,
  xlab = "t",
  ylab = "crash counts",
  line.color = "red",
  line.size = 0.6
)
# Indexes
id.b0 <- id.b1 <- 1:n
data.crash <- data.frame(id.b0, id.b1, kabct, vmt)
# Model formula and fit
formula.crashct <-
  kabct ~ -1 + f(id.b0,
                 model = "rw1",
                 initial = 5,
                 constr = FALSE) +
  f(id.b1,
    vmt,
    model = "rw1",
    initial = 5,
    constr = FALSE)
model.crashct = inla(
  formula.crashct,
  family = "nbinomial",
  data = data.crash,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.family = list(hyper = list(theta = list(
    prior = "loggamma",
    param = c(1, 1)
  )))
)
summary(model.crashct)
# Code for Figure 9.4
bt0 <-
  ts(model.crashct$summary.random$id.b0["mean"],
     start = 1995,
     frequency = 12)
bt1 <-
  ts(model.crashct$summary.random$id.b1["mean"],
     start = 1995,
     frequency = 12)
par(mfrow = c(1, 2))
plot(bt0, xlab = "t", ylab = expression(paste("post. mean of ", beta[0], sep =
                                                "")))
abline(v = 1997.9, lty = 3)
abline(v = 2000.9, lty = 3)
plot(bt1, xlab = "t", ylab = expression(paste("post. mean of ", beta[1], sep =                                               "")))  # dynamic slope beta_t1
abline(v = 1997.9, lty = 3)
abline(v = 2000.9, lty = 3)
lines(ts(
  model.crashct[[15]]$id.b1["0.025quant"],
  start = 1995,
  frequency =
    12
))
# Static negative binomial model
reg.negbin <- MASS::glm.nb(kabct ~ vmt, link = log)
summary(reg.negbin)