# Section 7.3: Weibull state space model
# Data: Volatility index time series
# Input: VIX.csv
rm(list = ls())
# Required packages
library(INLA)
library(astsa)
library(readxl)
library(lubridate)
library(tidyverse)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Read volatility index data
index <- read.csv("Chapter-7/VIX.csv", header = TRUE)
vix <- index[, 1]
n <- nrow(index)
# Model W1
# Index, formula and model fit
id.x <- 1:length(vix)
vix.data <- cbind.data.frame(vix, id.x)
formula.vix.W1 <- vix ~ 1 + f(id.x, model = "ar1")
model.vix.W1 <- inla(
  formula.vix.W1,
  data = vix.data,
  family = "weibull",
  control.family = list(variant = 0),
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.vix.W1)
# Fitted values
post.sample.W1 <-
  inla.posterior.sample(n = 1000, model.vix.W1, seed = 123457)
fun.pred <- function(x) {
  return(gamma(1 + 1 / x$hyperpar[1]) /
           (exp(x$latent[1:n]) ^ (1 / x$hyperpar[1])))
}
fits <- post.sample.W1 %>%
  sapply(function(x)
    fun.pred(x))
fit.vix.W1 <- rowMeans(fits)
# Code for Figure 7.6
plot.df <- as_tibble(cbind.data.frame(
  time = id.x,
  vix.obs = vix.data$vix,
  vix.fit = fit.vix.W1
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "VIX",
  line.size = 0.8,
  line.color = c("red", "blue")
)
# Model W2
vxn <- index[, 2]
id.vxn <- 1:length(vxn)
vix.data <- cbind.data.frame(vix.data, id.vxn, vxn)
formula.vix.W2 = vix ~ 1 + f(id.vxn, vxn, model = "ar1")
model.vix.W2 = inla(
  formula.vix.W2,
  data = vix.data,
  family = "weibull",
  control.family = list(variant = 1),
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.vix.W2)

# Section 7.3.1 - forecasting from Weibull models
mean <-
  post.sample.W1[[1]]$hyperpar['Rho for id.x'] *
  post.sample.W1[[1]]$latent[2 * n]
sigma2.w <- 1 / post.sample.W1[[1]]$hyperpar['Precision for id.x']
x.np1 <- rnorm(500, mean = mean, sd = sqrt(sigma2.w))
eta.np1 <- c(tail(post.sample.W1[[1]]$latent, 1)) + x.np1
lambda.np1 <- exp(eta.np1)
gamma.w <-
  post.sample.W1[[1]]$hyperpar['alpha parameter for weibull']
y.np1 <- gamma(1 + 1 / gamma.w) /
  (lambda.np1 ^ (1 / gamma.w))
mean(y.np1)
