# Section 10.2.4: Example: NYSE returns
# Data: Returns of NYSE
# Inputs: nyse data from package astsa
rm(list = ls())
# Required packages
library(astsa)
library(gridExtra)
library(INLA)
library(tidyverse)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Index and data frame
id.1 <- 1:length(nyse)
data.nyse <- cbind.data.frame(nyse, id.1)
# Model formula and fit
formula.nyse <- nyse ~ 0 + f(id.1,
                             model = "ar1",
                             hyper = list(prec = list(param = c(1, 0.001)),
                                          mean = list(fixed = FALSE)))
model.nyse <- inla(
  formula.nyse,
  family = "stochvol_t",
  data = data.nyse,
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.predictor = list(compute = TRUE, link = 1)
)
summary(model.nyse)