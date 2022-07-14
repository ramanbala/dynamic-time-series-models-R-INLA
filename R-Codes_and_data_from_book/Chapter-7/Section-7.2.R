# Section 7.2: Gamma state space model
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
# Code for Figure 7.1
tsline(
  as.numeric(vix),
  ylab = "VIX",
  xlab = "t",
  line.color = "red",
  line.size = 0.6
) +
  ylim(9, 30) +
  scale_x_continuous(
    breaks = c(1, 13, 25, 37, 49, 60),
    labels = c("2012", "2013", "2014", "2015", "2016", "2017")
  )
# Model G1
# Index, formula and model fit
id.x <- 1:length(vix)
vix.data <- cbind.data.frame(vix, id.x)
formula.vix.G1 <- vix ~ 1 + f(id.x, model = "ar1")
model.vix.G1 <- inla(
  formula.vix.G1,
  data = vix.data,
  family = "gamma",
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.vix.G1)
# Code for Figure 7.2
fit.vix.G1 <- model.vix.G1$summary.fitted.values$mean
plot.df <- as_tibble(cbind.data.frame(
  time = id.x,
  vix.obs = vix.data$vix,
  vix.fit = fit.vix.G1
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "VIX",
  line.size = 0.8,
  line.color = c("red", "blue")
)
# Model G2
# Code for Figure 7.3
vxn <- index[, 2]
plot.df <- as_tibble(cbind.data.frame(
  time = id.x,
  vix.obs = vix.data$vix,
  vxn = vxn
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "VIX",
  line.size = 0.8,
  line.color = c("red", "black")
)
# Index, formula and model fit
id.vxn <- id.x
formula.vix.G2 <- vix ~ 1 + f(id.vxn, vxn, model = "ar1")
model.vix.G2 <- inla(
  formula.vix.G2,
  data = vix.data,
  family = "gamma",
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.vix.G2)
# Code for Figure 7.4
fit.vix.G2 <- model.vix.G2$summary.fitted.values$mean
plot.df <- as_tibble(cbind.data.frame(
  time = id.x,
  vix.obs = vix.data$vix,
  vix.fit = fit.vix.G2
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "VIX",
  line.size = 0.8,
  line.color = c("red", "blue")
)
# Model G3
# Index, formula and model fit
formula.vix.G3 <-  vix ~ 1 + vxn + f(id.x, model = "ar1")
model.vix.G3 <- inla(
  formula.vix.G3,
  data = vix.data,
  family = "gamma",
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.vix.G3)
# Code for Figure 7.5
fit.vix.G3 <- model.vix.G3$summary.fitted.values$mean
plot.df <- as_tibble(cbind.data.frame(
  time = id.x,
  vix.obs = vix.data$vix,
  vix.fit = fit.vix.G3
))
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "VIX",
  line.size = 0.8,
  line.color = c("red", "blue")
)
# Code for Table 7.1
g1 <- model.selection.criteria(model.vix.G1, n.train = n)
g1.ml <- model.vix.G1$mlik[2]
g2 <- model.selection.criteria(model.vix.G2, n.train = n)
g2.ml <- model.vix.G2$mlik[2]
g3 <- model.selection.criteria(model.vix.G3, n.train = n)
g3.ml <- model.vix.G3$mlik[2]
gamma.tab <- rbind.data.frame(g1, g2, g3)
rownames(gamma.tab) <- c("Model G1", "Model G2", "Model G3")
gamma.tab$mlik <- c(g1.ml, g2.ml, g3.ml)
gamma.tab <- rownames_to_column(gamma.tab, var = "Model")
gamma.tab %>%
  kable(
    caption = "Comparison of Models G1, G2 and G3.",
    col.names = c("Model", "DIC", "WAIC", "PsBF", "Marginal likelihood"),
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
