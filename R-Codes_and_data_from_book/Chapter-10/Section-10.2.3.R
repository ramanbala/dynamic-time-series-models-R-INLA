# Section 10.2.3: Example: IBM stock returns
# Data: Daily stock prices of IBM
# Inputs: IBM from package quantmod
rm(list = ls())
# Required packages
library(quantmod)
library(gridExtra)
library(INLA)
library(tidyverse)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Read data
getSymbols(
  "IBM",
  from = '2018-01-01',
  to = "2020-12-31",
  warnings = FALSE,
  auto.assign = TRUE
)
ret <- ts(diff(log(IBM$IBM.Adjusted)))[-1]
# Code for Figure 10.1
adj.plot <-
  tsline(
    as.numeric(IBM$IBM.Adjusted),
    xlab = "t",
    ylab = "Adjusted price",
    line.color = "black",
    line.size = 0.6
  )
ret.plot <- tsline(
  ret,
  xlab = "t",
  ylab = "Returns",
  line.color = "red",
  line.size = 0.6
)
grid.arrange(adj.plot, ret.plot, nrow = 1)
# Indexes and data frame
set.seed(1234)
id.1 <- 1:length(ret)
data.ret <- cbind.data.frame(ret, id.1)
# Model formula and fit
formula.ret <- ret ~ 0 + f(id.1,
                           model = "ar1",
                           hyper = list(prec = list(param = c(1, 0.0001)),
                                        mean = list(fixed = FALSE)))
model.g <- inla(
  formula.ret,
  family = "stochvol",
  data = data.ret,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.predictor = list(compute = TRUE, link = 1)
)
summary(model.g)
# Fitted values
fit.model.g <- exp(model.g$summary.linear.predictor$mean)
# Model with Student-t errors
set.seed(1234)
model.t <- inla(
  formula.ret,
  family = "stochvol_t",
  data = data.ret,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.predictor = list(compute = TRUE, link = 1)
)
summary(model.t)
# Fitted values
fit.model.t <-
  exp(model.t$summary.linear.predictor$mean)
# Code for Figure 10.2
retsq <- ret ^ 2
pg <-
  multiline.plot(
    cbind.data.frame(time = 1:length(ret), retsq, fit.model.g),
    line.size = 0.5,
    line.color = c("red", "blue")
  )
pt <-
  multiline.plot(
    cbind.data.frame(time = 1:length(ret), retsq, fit.model.t),
    line.size = 0.5,
    line.color = c("red", "blue")
  )
grid.arrange(pg, pt)
# Code for Table 10.1
dic <- c(model.g$dic$dic, model.t$dic$dic)
waic <- c(model.g$waic$waic, model.t$waic$waic)
ml <- c(model.g$mlik[1, 1], model.t$mlik[1, 1])
comp.ibm <-
  cbind.data.frame(Model = c("model.g", "model.t"), dic, waic, ml)
comp.ibm %>%
  kable(
    caption = "In-sample model comparisons.",
    col.names = c("Model", "DIC", "WAIC", "Marginal Likelihood"),
    escape = FALSE,
    booktabs = TRUE,
    digits = 3
  ) %>%
  kable_styling(
    bootstrap_options = "striped",
    position = "center",
    fixed_thead = TRUE,
    full_width = FALSE
  ) 