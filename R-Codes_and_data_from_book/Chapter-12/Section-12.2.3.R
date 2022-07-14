# Section 12.2.3: Example: Ridesourcing data in NYC for a single taxi zone
# Data: TNC usage data for Brooklyn heights
# Inputs: "ride.brooklyn.csv"
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# Read data
ride.brooklyn <-
  read.csv("Chapter-12/ride.brooklyn.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
n <- n_distinct(ride.brooklyn$date)
n.hold <- 5
n.train <- n - n.hold
train.df <- ride.brooklyn[1:n.train, ]
test.df <- tail(ride.brooklyn, n.hold)
# Indexes and data frame
N <- 2 * n
y <-
  c(c(train.df$tnc, rep(NA, n.hold)), c(train.df$taxi, rep(NA, n.hold)))
alpha.tnc <- c(rep(1, n), rep(NA, n))
alpha.taxi <- c(rep(NA, n), rep(1, n))
subway.tnc <- c(ride.brooklyn$subway, rep(NA, n))
subway.taxi <- c(rep(NA, n), ride.brooklyn$subway)
holidays.tnc <- c(ride.brooklyn$holidays, rep(NA, n))
holidays.taxi <- c(rep(NA, n), ride.brooklyn$holidays)
precip.tnc <- c(ride.brooklyn$precip, rep(NA, n))
precip.taxi <- c(rep(NA, n), ride.brooklyn$precip)
t.index <- 1:N
b0.tnc <- c(1:n, rep(NA, n))
b0.taxi <- c(rep(NA, n), 1:n)
ride.mvn.data <- cbind.data.frame(
  y,
  alpha.tnc,
  alpha.taxi,
  subway.tnc,
  subway.taxi,
  holidays.tnc,
  holidays.taxi,
  precip.tnc,
  precip.taxi,
  b0.tnc,
  b0.taxi,
  t.index
)
# Code for Figure 12.2
ccf(
  train.df$tnc,
  train.df$taxi,
  lag.max = 30,
  ylim = c(-1, 1),
  main = "",
  ylab = "ccf",
  xlab = "lag"
)
# Model formula and fit
formula.ride.mvn.model1 <-  y ~ -1 + alpha.tnc + alpha.taxi +
  subway.tnc + subway.taxi +
  holidays.tnc + holidays.taxi +
  precip.tnc + precip.taxi +
  f(t.index, model = "iid2d", n = N) +
  f(b0.tnc, model = "ar1") +
  f(b0.taxi, model = "ar1")
ride.mvn.model1 <-
  inla(
    formula.ride.mvn.model1,
    family = c("gaussian"),
    data = ride.mvn.data,
    control.compute = list(dic = TRUE, config = TRUE),
    control.predictor = list(compute = TRUE),
    control.family =
      list(hyper = list(prec = list(
        initial = 10,
        fixed = TRUE
      ))),
  )
summary(ride.mvn.model1)
# Fitted values and forecasts
# Approach 1
post.sample <-
  inla.posterior.sample(1000, ride.mvn.model1)
# Function to compute fits
fit.post.sample <- function(x) {
  sigma2.v1 <- 1 / x$hyperpar[1]
  sigma2.v2 <- 1 / x$hyperpar[2]
  rho.v1v2 <- x$hyperpar[3]
  cov.v1v2 <- rho.v1v2 * sqrt(sigma2.v1) * sqrt(sigma2.v2)
  sigma.v <-
    matrix(c(sigma2.v1, cov.v1v2, cov.v1v2, sigma2.v2), nrow = 2)
  V <- rmvnorm(n, mean = c(0, 0), sigma = sigma.v)
  V1 <- V[, 1]
  V2 <- V[, 2]
  fit.tnc <- x$latent[grep("alpha.tnc", rownames(x$latent))] +
    x$latent[grep("b0.tnc", rownames(x$latent))] +
    x$latent[grep("subway.tnc", rownames(x$latent))] *
    ride.brooklyn$subway +
    x$latent[grep("holidays.tnc", rownames(x$latent))] *
    ride.brooklyn$holidays +
    x$latent[grep("precip.tnc", rownames(x$latent))] *
    ride.brooklyn$precip +
    V1
  fit.taxi <- x$latent[grep("alpha.taxi", rownames(x$latent))] +
    x$latent[grep("b0.taxi", rownames(x$latent))] +
    x$latent[grep("subway.taxi", rownames(x$latent))] *
    ride.brooklyn$subway +
    x$latent[grep("holidays.taxi", rownames(x$latent))] *
    ride.brooklyn$holidays +
    x$latent[grep("precip.taxi", rownames(x$latent))] *
    ride.brooklyn$precip +
    V2
  return(list(fit.tnc = fit.tnc, fit.taxi = fit.taxi))
}
fits <- post.sample %>%
  lapply(function(x)
    fit.post.sample(x))
tnc.fit <- fits %>%
  sapply(function(x)
    x$fit.tnc) %>%
  rowMeans()
taxi.fit <- fits %>%
  sapply(function(x)
    x$fit.taxi) %>%
  rowMeans()
# Code for Figure 12.3
tnc.obs <- ride.brooklyn$tnc
taxi.obs <- ride.brooklyn$taxi
plot.data.tnc <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    tnc.obs = tnc.obs,
    tnc.fit = tnc.fit
  ))
plot.tnc <- multiline.plot(
  plot.data.tnc,
  xlab = "weeks",
  ylab = "TNC usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)

plot.data.taxi <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    taxi.obs = taxi.obs,
    taxi.fit = taxi.fit
  ))
plot.taxi <- multiline.plot(
  plot.data.taxi,
  xlab = "weeks",
  ylab = "Taxi usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
grid.arrange(plot.tnc, plot.taxi)
# Approach 2  - using linear combinations
lc1 <- c()
tnc.dat.lc <- ride.brooklyn %>%
  select(subway, holidays:precip)
for (i in 1:n) {
  idx0 <- idx1 <- idx2 <- idx3 <-  rep(0, n)
  idx0[i] <-  1
  idx1[i] <- tnc.dat.lc[i, 1]
  idx2[i] <- tnc.dat.lc[i, 2]
  idx3[i] <- tnc.dat.lc[i, 3]
  lcc <-
    inla.make.lincomb(
      alpha.tnc = 1,
      b0.tnc = idx0,
      subway.tnc = idx1[i],
      holidays.tnc = idx2[i],
      precip.tnc = idx3[i]
    )
  for (k in 1:length(lcc[[1]])) {
    if (length(lcc[[1]][[k]][[1]]$weight) == 0) {
      lcc[[1]][[k]][[1]]$idx <- i
      lcc[[1]][[k]][[1]]$weight <- 0
    }
  }
  names(lcc) <-  paste0("tnc", i)
  lc1 <-  c(lc1, lcc)
}
lc2 <- c()
taxi.dat.lc <- ride.brooklyn %>%
  select(subway, holidays:precip)
for (i in 1:n) {
  idx0 <- idx1 <- idx2 <- idx3 <-  rep(0, n)
  idx0[i] <-  1
  idx1[i] <- taxi.dat.lc[i, 1]
  idx2[i] <- taxi.dat.lc[i, 2]
  idx3[i] <- taxi.dat.lc[i, 3]
  lcc <-
    inla.make.lincomb(
      alpha.taxi = 1,
      b0.taxi = idx0,
      subway.taxi = idx1[i],
      holidays.taxi = idx2[i],
      precip.taxi = idx3[i]
    )
  for (k in 1:length(lcc[[1]])) {
    if (length(lcc[[1]][[k]][[1]]$weight) == 0) {
      lcc[[1]][[k]][[1]]$idx <- i
      lcc[[1]][[k]][[1]]$weight <- 0
    }
  }
  names(lcc) <-  paste0("taxi", i)
  lc2 <-  c(lc2, lcc)
}
lc <- c(lc1, lc2)
# Use lc in inla()
ride.mvn.model1 <-
  inla(
    formula.ride.mvn.model1,
    family = c("gaussian"),
    data = ride.mvn.data,
    control.compute = list(dic = TRUE, config = TRUE),
    control.predictor = list(compute = TRUE),
    control.family =
      list(hyper = list(prec = list(
        initial = 10,
        fixed = TRUE
      ))),
    lincomb = lc
  )
summary(ride.mvn.model1)
# Code for Figure 12.4
tnc.fit <- ride.mvn.model1$summary.lincomb.derived$mean[1:n]
tnc.obs <- ride.brooklyn$tnc
plot.data.tnc <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    tnc.obs = tnc.obs,
    tnc.fit = tnc.fit
  ))
plot.tnc <- multiline.plot(
  plot.data.tnc,
  xlab = "weeks",
  ylab = "TNC usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)

taxi.fit <- tail(ride.mvn.model1$summary.lincomb.derived$mean, n)
taxi.obs <- ride.brooklyn$taxi
plot.data.taxi <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    taxi.obs = taxi.obs,
    taxi.fit = taxi.fit
  ))
plot.taxi <- multiline.plot(
  plot.data.taxi,
  xlab = "weeks",
  ylab = "Taxi usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
grid.arrange(plot.tnc, plot.taxi)
# Remark 2 - including effects of exogenous predictors
# Indexes and data frame
t.index <- 1:N
alpha.tnc <- c(rep(1, n), rep(NA, n))
alpha.taxi <- c(rep(NA, n), rep(1, n))
b0.tnc <- id.sub.tnc <-
  id.holidays.tnc <- id.precip.tnc <- c(1:n, rep(NA, n))
b0.taxi <- id.sub.taxi <- id.events.taxi <-
  id.holidays.taxi <- id.precip.taxi <- c(rep(NA, n), 1:n)
ride.mvn.data <- cbind.data.frame(
  y,
  alpha.tnc,
  alpha.taxi,
  subway.tnc,
  subway.taxi,
  holidays.tnc,
  holidays.taxi,
  precip.tnc,
  precip.taxi,
  b0.tnc,
  b0.taxi,
  id.sub.tnc,
  id.sub.taxi,
  id.holidays.tnc,
  id.holidays.taxi,
  id.precip.tnc,
  id.precip.taxi,
  t.index
)
# Model formula and fit
formula.ride.mvn.model1 <-  y ~ -1 + alpha.tnc + alpha.taxi +
  f(t.index, model = "iid2d", n = N) +
  f(b0.tnc, model = "ar1", constr = FALSE) +
  f(b0.taxi, model = "ar1", constr = FALSE) +
  f(id.sub.tnc, subway.tnc, model = "ar1", constr = FALSE) +
  f(id.sub.taxi, subway.taxi, model = "ar1", constr = FALSE) +
  f(id.holidays.tnc,
    holidays.tnc,
    model = "ar1",
    constr = FALSE) +
  f(id.holidays.taxi,
    holidays.taxi,
    model = "ar1",
    constr = FALSE) +
  f(id.precip.tnc,
    precip.tnc,
    model = "ar1",
    constr = FALSE) +
  f(id.precip.taxi,
    precip.taxi,
    model = "ar1",
    constr = FALSE)
# Set up lc
lc1 <- c()
tnc.dat.lc <- ride.brooklyn %>%
  select(subway, holidays:precip)
for (i in 1:n) {
  idx0 <- idx1 <- idx2 <- idx3 <-  rep(0, n)
  idx0[i] <-  1
  idx1[i] <- tnc.dat.lc[i, 1]
  idx2[i] <- tnc.dat.lc[i, 2]
  idx3[i] <- tnc.dat.lc[i, 3]
  lcc <-  inla.make.lincomb(
    b0.tnc = idx0,
    id.sub.tnc = idx1,
    id.holidays.tnc = idx2,
    id.precip.tnc = idx3
  )
  for (k in 1:length(lcc[[1]])) {
    if (length(lcc[[1]][[k]][[1]]$weight) == 0) {
      lcc[[1]][[k]][[1]]$idx <- i
      lcc[[1]][[k]][[1]]$weight <- 0
    }
  }
  names(lcc) <-  paste0("tnc", i)
  lc1 <-  c(lc1, lcc)
}
# Remark-3
for (k in 1:length(lcc[[1]])) {
  if (length(lcc[[1]][[k]][[1]]$weight) == 0) {
    lcc[[1]][[k]][[1]]$idx <- i
    lcc[[1]][[k]][[1]]$weight <- 0
  }
}
