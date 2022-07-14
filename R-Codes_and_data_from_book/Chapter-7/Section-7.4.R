# Section 7.4: Beta state space model
# Data: Crest market share
# Input: TPASTE.csv
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
mshare <- read.csv("Chapter-7/TPASTE.csv", header = TRUE)
crestms <- mshare[, 1]
# Code for Figure 7.7
tsline(
  crestms,
  ylab = "market share",
  xlab = "week",
  line.color = "red",
  line.size = 0.6
) +
  geom_vline(xintercept = 135,
             linetype = "dashed",
             color = "black")
# Model B1
id.x <- 1:length(crestms)
crest.data <- cbind.data.frame(mshare, id.x)
formula.crest.B1 = crestms ~ 1 + f(id.x, model = "ar1")
model.crest.B1 = inla(
  formula.crest.B1,
  data = crest.data,
  family = "beta",
  scale = 1,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.crest.B1)
# Code for Figure 7.8
fit.crest.B1 <- model.crest.B1$summary.fitted.values$mean
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x,
    Crest.obs = crest.data$crestms,
    Crest.fit = fit.crest.B1
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "week",
  ylab = "market share",
  line.size = 0.6,
  line.color = c("red", "blue")
) 
# Model B2
colgms <- mshare[, 2]
crestpr <- mshare[, 3]
colgpr <- mshare[, 4]
formula.crest.B2 <-
  crestms ~ 1 + colgms + crestpr + colgpr + f(id.x, model = "ar1")
model.crest.B2 <- inla(
  formula.crest.B2,
  data = crest.data,
  family = "beta",
  scale = 1,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.crest.B2)
# Code for Figure 7.9
fit.crest.B2 <- model.crest.B2$summary.fitted.values$mean
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x,
    Crest.obs = crest.data$crestms,
    Crest.fit = fit.crest.B2
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "week",
  ylab = "market share",
  line.size = 0.6,
  line.color = c("red", "blue")
)
# Model B3
id.colgms <- id.crestpr <- id.colgpr <- 1:length(crestms)
formula.crest.B3 <-
  crestms ~ 1 + f(id.colgms, colgms, model = "ar1") +
  f(id.crestpr, crestpr, model = "ar1") +
  f(id.colgpr, colgpr, model = "ar1")
model.crest.B3 <- inla(
  formula.crest.B3,
  data = crest.data,
  family = "beta",
  scale = 1,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.crest.B3)
# Code for Figure 7.10
fit.crest.B3 <- model.crest.B3$summary.fitted.values$mean
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x,
    Crest.obs = crest.data$crestms,
    Crest.fit = fit.crest.B3
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "week",
  ylab = "market share",
  line.size = 0.6,
  line.color = c("red", "blue")
)
# Model B4
formula.crest.B4 <- crestms ~ 1 + colgms + crestpr + colgpr
model.crest.B4 <- inla(
  formula.crest.B4,
  data = crest.data,
  family = "beta",
  scale = 1,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.crest.B4)
# Code for Figure 7.11
fit.crest.B4 <- model.crest.B4$summary.fitted.values$mean
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x,
    Crest.obs = crest.data$crestms,
    Crest.fit = fit.crest.B4
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "week",
  ylab = "market share",
  line.size = 0.8,
  line.color = c("red", "blue")
)
# Code for Table 7.2
n <- nrow(crest.data)
b1 <- model.selection.criteria(model.crest.B1, n.train = n)
b1.ml <- model.crest.B1$mlik[2]
b2 <- model.selection.criteria(model.crest.B2, n.train = n)
b2.ml <- model.crest.B2$mlik[2]
b3 <- model.selection.criteria(model.crest.B3, n.train = n)
b3.ml <- model.crest.B3$mlik[2]
b4 <- model.selection.criteria(model.crest.B4, n.train = n)
b4.ml <- model.crest.B4$mlik[2]

beta.tab <- rbind.data.frame(b1, b2, b3, b4)
rownames(beta.tab) <-
  c("Model B1", "Model B2", "Model B3", "Model B4")
beta.tab$mlik <- c(b1.ml, b2.ml, b3.ml, b4.ml)
beta.tab <- rownames_to_column(beta.tab, var = "Model")
beta.tab %>%
  kable(
    caption = "Comparison of Models B1, B2, B3 and B4.",
    col.names = c("Model", "DIC", "WAIC", "PsBF", "Marginal Likelihood"),
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
