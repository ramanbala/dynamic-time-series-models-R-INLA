# Section 8.2.2: Example: Weekly shopping trips for a single household
# Data: Number of weekly shopping trips
# Inputs: weekly_shopping_single_household.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Read data
trips.single.household <-
  read.csv(
    "Chapter-8/weekly_shopping_single_household.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
# Code for Figure 8.1
tsline(
  trips.single.household$n.trips,
  xlab = "week",
  ylab = "shopping trips",
  line.color = "red",
  line.size = 0.6
)
# Split data into train and test
n <- nrow(trips.single.household)
n.hold <- 6
n.train <- n - n.hold
train.trips.single.household <-
  trips.single.household$n.trips[1:n.train]
test.trips.single.household <-
  tail(trips.single.household$n.trips, n.hold)
bin.size <- max(train.trips.single.household)
id.beta0 <- 1:n
data.single.household <-
  cbind.data.frame(n.trips = c(train.trips.single.household,
                               rep(NA, n.hold)), id.beta0)
# Priors
prior.single.household <- list(
  prec = list(prior = "loggamma", param = c(0.1, 0.1)),
  rho = list(prior = "normal", param = c(0, 1))
)
# Model fit
formula.single.household <-
  n.trips ~ 1 + f(id.beta0,  model = "ar1", hyper = prior.single.household)
inla.single.household <- inla(
  formula.single.household,
  family = 'binomial',
  Ntrials = bin.size,
  data = data.single.household,
  control.family = list(link = 'logit'),
  control.predictor = list(link = 1, compute = TRUE),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE
  )
)
summary(inla.single.household)
# Forecasts
set.seed(1234579)
post.samp <- inla.posterior.sample(n = 1000, inla.single.household, seed = 1234)
inla.single.household$misc$configs$contents
head(post.samp[[1]]$latent)
tail(post.samp[[1]]$latent)
# Posterior predictive distributions
fit.post.samp <- vector("list", 1000)
for (j in 1:1000) {
  eta <- post.samp[[j]]$latent[1:n]
  p <- exp(eta) / (1 + exp(eta))
  fit.post.samp[[j]] <- rbinom(n, bin.size, p)
}
fore.mean <- bind_cols(fit.post.samp) %>%
  apply(1, mean)
fore.sd <- bind_cols(fit.post.samp) %>%
  apply(1, sd) %>%
  round(3)
fore.quant <- bind_cols(fit.post.samp) %>%
  apply(1, function(x)
    quantile(x, probs = c(0.025, 0.975)))
obs.fit.single.household <-
  cbind.data.frame(
    observed = trips.single.household$n.trips,
    fore.mean,
    fore.sd,
    quant0.025 = fore.quant[1, ],
    quant0.975 = fore.quant[2, ]
  )
# Code for Figure 8.2
fit.single.household <- vector("integer", n)
house1.dat <-
  cbind.data.frame(
    time = 1:n,
    obs = obs.fit.single.household$observed,
    forecast.mean = obs.fit.single.household$fore.mean
  )
multiline.plot(
  house1.dat,
  title = "",
  xlab = "week",
  line.color = c("red", "blue"),
  line.size = 0.6
) +
  geom_vline(xintercept = (n.train), size = 0.3) +
  theme(legend.position = "none")
# Model without level alpha
formula.single.household.noalpha <-
  n.trips ~ -1 + f(id.beta0, model = "ar1", hyper = prior.single.household)
inla.single.household.noalpha <- inla(
  formula.single.household.noalpha,
  family = 'binomial',
  Ntrials = bin.size,
  data = data.single.household,
  control.family = list(link = 'logit'),
  control.predictor = list(link = 1, compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic =
                           TRUE)
)
inla.single.household.noalpha$summary.hyperpar
