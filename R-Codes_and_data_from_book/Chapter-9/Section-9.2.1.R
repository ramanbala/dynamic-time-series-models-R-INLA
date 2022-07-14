# Section 9.2.1: Example: Simulated univariate Poisson counts
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Simulate data
sim.y.ar1.pois <-
  simulation.from.ar.poisson(
    sample.size = 500,
    burn.in = 100,
    phi = 0.65,
    level = 0,
    drift = 0,
    W = 0.25,
    plot.data = TRUE,
    seed = 123457
  )
# Code for Figure 9.1
sim.y.ar1.pois$sim.plot
# Indexes
y.ar1.pois <- sim.y.ar1.pois$sim.data
n <- length(sim.y.ar1.pois$sim.data)
id <- 1:n
sim.pois.dat <- cbind.data.frame(y.ar1.pois, id)
# Model formula and fit
formula <- y.ar1.pois ~ f(id, model = "ar1") - 1
model.sim.pois <- inla(
  formula,
  family = "poisson",
  data = sim.pois.dat,
  control.predictor = list(compute = TRUE, link = 1)
)
summary(model.sim.pois)
# Marginal distribution of state variance
state.var <- inla.tmarginal(
  fun = function(x)
    exp(-x),
  marginal = model.sim.pois$internal.marginals.hyperpar$`Log precision for id`
)
var.state.hpd.interval <- inla.hpdmarginal(p = 0.95, state.var)
round(var.state.hpd.interval, 3)
# Predicted values
head(model.sim.pois$summary.fitted.values$mean) %>%
  round(3)
## [1] 1.012 1.293 1.318 1.582 1.255 1.123
exp(head(model.sim.pois$summary.linear.predictor$mean)) %>%
  round(3)
## [1] 0.906 1.173 1.200 1.444 1.142 1.018
fitted.from.lp <- model.sim.pois$marginals.linear.predictor %>%
  sapply(function(x)
    inla.emarginal(
      fun = function(y)
        exp(y),
      marginal = x
    ))
head(fitted.from.lp) %>%
  round(3)
# Code for Figure 9.2
fit.sim.pois <- model.sim.pois$summary.fitted.values$mean
upper.sim.pois <- model.sim.pois$summary.fitted.values$`0.975quant`
lower.sim.pois <- model.sim.pois$summary.fitted.values$`0.025quant`
plot.sim.pois <-
  as_tibble(
    cbind.data.frame(
      time = 1:n,
      observed = y.ar1.pois,
      fitted = fit.sim.pois,
      lower.sim.pois,
      upper.sim.pois
    )
  )
ggplot(plot.sim.pois, aes(time, fitted)) +
  geom_line(colour = "#56B4E9") +
  geom_ribbon(aes(ymin = lower.sim.pois, ymax = upper.sim.pois), alpha =
                .2) +
  ylab("Posterior fits") + xlab("t")