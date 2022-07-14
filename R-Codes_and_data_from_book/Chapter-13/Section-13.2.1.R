# Section 13.2.1: Example: Analysis of TNC and Taxi as responses
# Data: Weekly TNC and taxi usage in Manhattan
# Inputs: "tnc_weekly_data.csv"
rm(list = ls())
# Required packages
library(mvtnorm)
library(INLA)
library(tidyverse)
library(gridExtra)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Read data
ride <-
  read.csv("Chapter-13/tnc_weekly_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
k <- 10000
ride$tnc <- ride$tnc / k
ride$taxi <- ride$taxi / k
ride$subway <- ride$subway / k
# Filter by borough = Manhattan
ride.m <- ride %>%
  filter(borough == "Manhattan")
n <- n_distinct(ride.m$date)
g <- n_distinct(ride.m$zoneid)
# Code for Figure 13.1
ride.zone.plot <- ride.m %>%
  group_split(zoneid)
plot.zone <- vector("list", 9)
for (z in 1:9) {
  plot.df <- ride.zone.plot[[z]] %>%
    mutate(time = 1:n) %>%
    select(time, tnc, taxi)
  plot.zone[[z]] <- multiline.plot(
    plot.df,
    xlab = "day",
    ylab = "ride usage",
    line.size = 0.3,
    line.color = c("red", "black")
  ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 7),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 7)
    )
}
grid.arrange(
  plot.zone[[1]],
  plot.zone[[2]],
  plot.zone[[3]],
  plot.zone[[4]],
  plot.zone[[5]],
  plot.zone[[6]],
  plot.zone[[7]],
  plot.zone[[8]],
  plot.zone[[9]]
)
# Create hold out data
n.hold <- 5
n.train <- n - n.hold
df.zone <- ride.m %>%
  group_split(zoneid,)
test.df <- df.zone %>%
  lapply(function(x)
    tail(x, n.hold)) %>%
  bind_rows()
df.zone.append <- df.zone
for (i in 1:length(df.zone)) {
  df.zone.append[[i]]$tnc[(n.train + 1):n] <- NA
  df.zone.append[[i]]$taxi[(n.train + 1):n] <- NA
}
df.zone.append <- bind_rows(df.zone.append)
# Stack data by zone
y <- df.zone.append %>%
  group_split(zoneid) %>%
  lapply(function(x)
    unlist(select(x, tnc, taxi))) %>%
  unlist()
subway.tnc <- ride.m %>%
  select(zoneid, subway) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(unlist(select(x, subway)), rep(NA, n))) %>%
  unlist()
subway.taxi <- ride.m %>%
  select(zoneid, subway) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(rep(NA, n), unlist(select(x, subway)))) %>%
  unlist()
holidays.tnc <- ride.m %>%
  select(zoneid, holidays) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(unlist(select(x, holidays)), rep(NA, n))) %>%
  unlist()
holidays.taxi <- ride.m %>%
  select(zoneid, holidays) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(rep(NA, n), unlist(select(x, holidays)))) %>%
  unlist()
precip.tnc <- ride.m %>%
  select(zoneid, precip) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(unlist(select(x, precip)), rep(NA, n))) %>%
  unlist()
precip.taxi <- ride.m %>%
  select(zoneid, precip) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(rep(NA, n), unlist(select(x, precip)))) %>%
  unlist()
# Indexes and replicates
N <- 2 * n
t.index <- rep(c(1:N), g)
b0.tnc <- rep(c(1:n, rep(NA, n)), g)
b0.taxi <- rep(c(rep(NA, n), 1:n), g)
re.b0.taxi <- re.b0.tnc <- re.tindex <- rep(1:g, each = N)
# Model L1: Same levels for TNC and Taxi usage across all zones
# Data frame
alpha.tnc <- rep(c(rep(1, n), rep(NA, n)), g)
alpha.taxi <- rep(c(rep(NA, n), rep(1, n)), g)
tnctaxi.hmv.data <- cbind.data.frame(
  y,
  t.index,
  re.tindex,
  alpha.tnc,
  alpha.taxi,
  b0.tnc,
  re.b0.tnc,
  b0.taxi,
  re.b0.taxi,
  subway.tnc,
  subway.taxi,
  holidays.tnc,
  holidays.taxi,
  precip.tnc,
  precip.taxi
)
# Model formula and fit
formula.tnctaxi.hmv <-
  y  ~ -1 + alpha.tnc + alpha.taxi + subway.tnc + subway.taxi +
  holidays.tnc + holidays.taxi + precip.tnc + precip.taxi +
  f(t.index,
    model = "iid2d",
    n = N,
    replicate = re.tindex) +
  f(b0.tnc,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.tnc) +
  f(b0.taxi,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.taxi)
model.tnctaxi.hmv <-
  inla(
    formula.tnctaxi.hmv,
    family = c("gaussian"),
    data = tnctaxi.hmv.data,
    control.inla = list(h = 1e-5,
                        tolerance = 1e-3),
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = TRUE
    ),
    control.predictor = list(compute = TRUE),
    control.family =
      list(hyper = list(prec = list(
        initial = 10,
        fixed = TRUE
      ))),
  )
summary(model.tnctaxi.hmv)
# Fitted values and forecasts
fit.post.sample.hmv <- function(x) {
  set.seed(123457)
  sigma2.v1 <- 1 / x$hyperpar[1]
  sigma2.v2 <- 1 / x$hyperpar[2]
  rho.v1v2 <- x$hyperpar[3]
  cov.v1v2 <- rho.v1v2 * sqrt(sigma2.v1) * sqrt(sigma2.v2)
  sigma.v <-
    matrix(c(sigma2.v1, cov.v1v2, cov.v1v2, sigma2.v2), nrow = 2)
  b0.tnc <-
    matrix(x$latent[grep("b0.tnc", rownames(x$latent))], nrow = n)
  b0.taxi <-
    matrix(x$latent[grep("b0.taxi", rownames(x$latent))], nrow = n)
  fit.tnc <- fit.taxi <- vector("list", n)
  for (j in 1:g) {
    V <- rmvnorm(n, mean = c(0, 0), sigma = sigma.v)
    V1 <- V[, 1]
    V2 <- V[, 2]
    fit.tnc[[j]] <-
      x$latent[grep("alpha.tnc", rownames(x$latent))] +
      b0.tnc[, j] +
      x$latent[grep("subway.tnc", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.tnc", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.tnc", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V1
    fit.taxi[[j]] <-
      x$latent[grep("alpha.taxi", rownames(x$latent))] +
      b0.taxi[, j] +
      x$latent[grep("subway.taxi", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.taxi", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.taxi", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V2
  }
  fit.tnc <-
    bind_cols(fit.tnc,
              .name_repair = ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  fit.taxi <-
    bind_cols(fit.taxi,
              .name_repair = ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  return(list(fit.tnc = fit.tnc, fit.taxi = fit.taxi))
}
post.sample.hmv <-
  inla.posterior.sample(n = 1000, model.tnctaxi.hmv, seed = 1234)
# Fits for Zone ID100
fits <- post.sample.hmv %>%
  mclapply(function(x)
    fit.post.sample.hmv(x))
zone <- match("ID100", unique(df.zone.append$zoneid))
tnc.fit <- fits %>%
  lapply(function(x)
    x$fit.tnc[, zone]) %>%
  bind_cols() %>%
  rowMeans()
taxi.fit <- fits %>%
  lapply(function(x)
    x$fit.taxi[, zone]) %>%
  bind_cols() %>%
  rowMeans()
## DIC, WAIC
dic.l1 <- model.tnctaxi.hmv$dic$dic
waic.l1 <- model.tnctaxi.hmv$waic$waic
## MAE
n <- n_distinct(ride.m$date)
merge.fits.tnc <- fits %>%
  lapply(function(x)
    x$fit.tnc) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows
merge.fits.tnc <- merge.fits.tnc %>%
  group_by(time) %>%
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.tnc <- tail(merge.fits.tnc, n.hold) %>%
  select(-time)
holdout.tnc <- test.df$tnc %>% matrix(nrow = n.hold)
mae.tnc <- holdout.tnc - holdout.fits.tnc
mae.tnc.l1 <- mae.tnc %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)
# mae for taxi
merge.fits.taxi <- fits %>%
  lapply(function(x)
    x$fit.taxi) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows %>%
  group_by(time) %>%
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.taxi <- tail(merge.fits.taxi, n.hold) %>%
  select(-time)
holdout.taxi <- test.df$taxi %>% matrix(nrow = n.hold)

mae.taxi <- holdout.taxi - holdout.fits.taxi
mae.taxi.l1 <- mae.taxi %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)
# Code for Figure 13.2
tnc.obs <- df.zone[[zone]]$tnc
plot.data.tnc <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    tnc.obs = tnc.obs,
    tnc.fit = tnc.fit
  ))
plot.tnc <- multiline.plot(
  plot.data.tnc,
  xlab = "week",
  ylab = "TNC usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)

taxi.obs <- df.zone[[zone]]$taxi
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


# Model L2. Different fixed-effect zone-specific levels
# Indexes and data frame
id.alpha.tnc <- id.alpha.taxi <- c()
for (i in 1:g) {
  temp.tnc <- c(rep(i, n), rep(NA, n))
  id.alpha.tnc <- c(id.alpha.tnc, temp.tnc)
  temp.taxi <- c(rep(NA, n), rep(i, n))
  id.alpha.taxi <- c(id.alpha.taxi, temp.taxi)
}
fact.alpha.tnc <- as.factor(id.alpha.tnc)
fact.alpha.taxi <- as.factor(id.alpha.taxi)
tnctaxi.hmvfe.data <- cbind.data.frame(
  y,
  t.index,
  re.tindex,
  fact.alpha.tnc,
  fact.alpha.taxi,
  b0.tnc,
  re.b0.tnc,
  b0.taxi,
  re.b0.taxi,
  subway.tnc,
  subway.taxi,
  holidays.tnc,
  holidays.taxi,
  precip.tnc,
  precip.taxi
)
# Model formula and fit
formula.tnctaxi.hmvfe <-
  y  ~ -1 +
  fact.alpha.tnc + fact.alpha.taxi + subway.tnc + subway.taxi +
  holidays.tnc + holidays.taxi + precip.tnc + precip.taxi +
  f(t.index,
    model = "iid2d",
    n = N,
    replicate = re.tindex) +
  f(b0.tnc,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.tnc) +
  f(b0.taxi,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.taxi)
model.tnctaxi.hmvfe <-
  inla(
    formula.tnctaxi.hmvfe,
    family = c("gaussian"),
    data = tnctaxi.hmvfe.data,
    control.inla = list(h = 1e-5,
                        tolerance = 1e-3),
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = TRUE
    ),
    control.predictor = list(compute = TRUE),
    control.family =
      list(hyper = list(prec = list(
        initial = 10,
        fixed = TRUE
      ))),
    control.fixed = list(expand.factor.strategy = "inla"),
  )
summary(model.tnctaxi.hmvfe)
# Fitted values
fit.post.sample.hmv.fe <- function(x) {
  sigma2.v1 <- 1 / x$hyperpar[1]
  sigma2.v2 <- 1 / x$hyperpar[2]
  rho.v1v2 <- x$hyperpar[3]
  cov.v1v2 <- rho.v1v2 * sqrt(sigma2.v1) * sqrt(sigma2.v2)
  sigma.v <-
    matrix(c(sigma2.v1, cov.v1v2, cov.v1v2, sigma2.v2), nrow = 2)
  b0.tnc <-
    matrix(x$latent[grep("b0.tnc", rownames(x$latent))], nrow = n)
  b0.taxi <-
    matrix(x$latent[grep("b0.taxi", rownames(x$latent))], nrow = n)
  fit.tnc <- fit.taxi <- vector("list", g)
  for (j in 1:g) {
    V <- rmvnorm(n, mean = c(0, 0), sigma = sigma.v)
    V1 <- V[, 1]
    V2 <- V[, 2]
    fit.tnc[[j]] <-
      x$latent[grep(paste0("alpha.tnc", j), rownames(x$latent))][1] +
      b0.tnc[, j] +
      x$latent[grep("subway.tnc", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.tnc", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.tnc", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V1
    fit.taxi[[j]] <-
      x$latent[grep(paste0("alpha.taxi", j), rownames(x$latent))][1] +
      b0.taxi[, j] +
      x$latent[grep("subway.taxi", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.taxi", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.taxi", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V2
  }
  fit.tnc <-
    bind_cols(fit.tnc, .name_repair =
                ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  fit.taxi <-
    bind_cols(fit.taxi, .name_repair =
                ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  return(list(fit.tnc = fit.tnc, fit.taxi = fit.taxi))
}
post.sample.hmv.fe <-
  inla.posterior.sample(n = 1000, model.tnctaxi.hmvfe, seed = 1234)
fits <- post.sample.hmv.fe %>%
  mclapply(function(x)fit.post.sample.hmv.fe(x))
#### DIC, WAIC
dic.l2 <- model.tnctaxi.hmvfe$dic$dic
waic.l2 <- model.tnctaxi.hmvfe$waic$waic

## MAE
# obtain fits for each zone - tnc
merge.fits.tnc <- fits %>%
  lapply(function(x)
    x$fit.tnc) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows %>%
  group_by(time) %>%
  # summarise_all(mean)
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.tnc <- tail(merge.fits.tnc, n.hold) %>%
  select(-time)
holdout.tnc <- test.df$tnc %>% matrix(nrow = n.hold)

mae.tnc <- holdout.tnc - holdout.fits.tnc
mae.tnc.l2 <- mae.tnc %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)

# mae for taxi
merge.fits.taxi <- fits %>%
  lapply(function(x)
    x$fit.taxi) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows %>%
  group_by(time) %>%
  # summarise_all(mean)
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.taxi <- tail(merge.fits.taxi, n.hold) %>%
  select(-time)
holdout.taxi <- test.df$taxi %>% matrix(nrow = n.hold)

mae.taxi <- holdout.taxi - holdout.fits.taxi
mae.taxi.l2 <- mae.taxi %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)


# Model L3. Different random-effect zone-specific levels
# Indexes and data frame
id.alpha.tnc <- id.alpha.taxi <- c()
for (i in 1:g) {
  temp.tnc <- c(rep(i, n), rep(NA, n))
  id.alpha.tnc <- c(id.alpha.tnc, temp.tnc)
  temp.taxi <- c(rep(NA, n), rep(i, n))
  id.alpha.taxi <- c(id.alpha.taxi, temp.taxi)
}
tnctaxi.hmvre.data <- cbind.data.frame(
  y,
  t.index,
  id.alpha.tnc,
  id.alpha.taxi,
  b0.tnc,
  b0.taxi,
  re.b0.tnc,
  re.b0.taxi,
  re.tindex,
  subway.tnc,
  subway.taxi,
  holidays.tnc,
  holidays.taxi,
  precip.tnc,
  precip.taxi
)
# Model formula and fit
prec.prior <- list(prec = list(param = c(0.001, 0.001)))
formula.tnctaxi.hmvre <-
  y  ~ -1 +
  f(id.alpha.tnc, model = "iid", hyper = prec.prior) +
  f(id.alpha.taxi, model = "iid", hyper = prec.prior) +
  subway.tnc + subway.taxi + holidays.tnc + holidays.taxi +
  precip.tnc + precip.taxi +
  f(t.index,
    model = "iid2d",
    n = N,
    replicate = re.tindex) +
  f(b0.tnc,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.tnc) +
  f(b0.taxi,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0.taxi)
model.tnctaxi.hmvre <-
  inla(
    formula.tnctaxi.hmvre,
    family = c("gaussian"),
    data = tnctaxi.hmvre.data,
    control.inla = list(h = 1e-5,
                        tolerance = 1e-3),
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = TRUE
    ),
    control.predictor = list(compute = TRUE),
    control.family =
      list(hyper = list(prec = list(
        initial = 10,
        fixed = TRUE
      ))),
  )
summary(model.tnctaxi.hmvre)
# Fitted values
fit.post.sample.hmv.re <- function(x) {
  sigma2.v1 <- 1 / x$hyperpar[1]
  sigma2.v2 <- 1 / x$hyperpar[2]
  rho.v1v2 <- x$hyperpar[3]
  cov.v1v2 <- rho.v1v2 * sqrt(sigma2.v1) * sqrt(sigma2.v2)
  sigma.v <-
    matrix(c(sigma2.v1, cov.v1v2, cov.v1v2, sigma2.v2), nrow = 2)
  b0.tnc <-
    matrix(x$latent[grep("b0.tnc", rownames(x$latent))], nrow = n)
  b0.taxi <-
    matrix(x$latent[grep("b0.taxi", rownames(x$latent))], nrow = n)
  fit.tnc <- fit.taxi <- vector("list", g)
  for (j in 1:g) {
    V <- rmvnorm(n, mean = c(0, 0), sigma = sigma.v)
    V1 <- V[, 1]
    V2 <- V[, 2]
    fit.tnc[[j]] <-
      x$latent[grep("id.alpha.tnc", rownames(x$latent))][j] +
      b0.tnc[, j] +
      x$latent[grep("subway.tnc", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.tnc", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.tnc", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V1
    fit.taxi[[j]] <-
      x$latent[grep("id.alpha.taxi", rownames(x$latent))][j] +
      b0.taxi[, j] +
      x$latent[grep("subway.taxi", rownames(x$latent))] *
      df.zone[[j]]$subway +
      x$latent[grep("holidays.taxi", rownames(x$latent))] *
      df.zone[[j]]$holidays +
      x$latent[grep("precip.taxi", rownames(x$latent))] *
      df.zone[[j]]$precip +
      V2
  }
  
  fit.tnc <- bind_cols(fit.tnc, .name_repair =
                         ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  # colnames(fit.tnc) <- paste0("g",1:g)
  fit.taxi <- bind_cols(fit.taxi, .name_repair =
                          ~ vctrs::vec_as_names(names = paste0("g", 1:g)))
  # colnames(fit.taxi) <- paste0("g",1:g)
  return(list(fit.tnc = fit.tnc, fit.taxi = fit.taxi))
}
post.sample.hmv.re <-
  inla.posterior.sample(n = 1000, model.tnctaxi.hmvre, seed = 1234)
fits <- post.sample.hmv.re %>%
  mclapply(function(x)fit.post.sample.hmv.re(x))
#### DIC, WAIC
dic.l3 <- model.tnctaxi.hmvre$dic$dic
waic.l3 <- model.tnctaxi.hmvre$waic$waic
## MAE
# obtain fits for each zone - tnc
merge.fits.tnc <- fits %>%
  lapply(function(x)
    x$fit.tnc) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows %>%
  group_by(time) %>%
  # summarise_all(mean)
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.tnc <- tail(merge.fits.tnc, n.hold) %>%
  select(-time)
holdout.tnc <- test.df$tnc %>% matrix(nrow = n.hold)
mae.tnc <- holdout.tnc - holdout.fits.tnc
mae.tnc.l3 <- mae.tnc %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)
# mae for taxi
merge.fits.taxi <- fits %>%
  lapply(function(x)
    x$fit.taxi) %>%
  lapply(function(x)
    mutate(x, time = 1:n)) %>%
  bind_rows %>%
  group_by(time) %>%
  # summarise_all(mean)
  summarise(across(c(g1:g36), ~ mean(.x)))
holdout.fits.taxi <- tail(merge.fits.taxi, n.hold) %>%
  select(-time)
holdout.taxi <- test.df$taxi %>% matrix(nrow = n.hold)
mae.taxi <- holdout.taxi - holdout.fits.taxi
mae.taxi.l3 <- mae.taxi %>%
  map_df(function(x)
    abs(x)) %>%
  colMeans() %>%
  round(3)
# Code for Table 13.1
dic <- c(dic.l1, dic.l2, dic.l3)
waic <- c(waic.l1, waic.l2, waic.l3)
mae.tnc.avg <-
  c(mean(mae.tnc.l1), mean(mae.tnc.l2), mean(mae.tnc.l3))
mae.taxi.avg <-
  c(mean(mae.taxi.l1), mean(mae.taxi.l2), mean(mae.taxi.l3))
tab.hmv <-
  cbind.data.frame(
    Model = c("L1", "L2", "L3"),
    DIC = dic,
    waic = waic,
    mae.tnc.avg,
    mae.taxi.avg
  )
tab.hmv %>%
  kable(
    caption = "Comparison of Models L1-L3.",
    col.names = c(
      "Model",
      "DIC",
      "WAIC",
      "Average Out-of-Sample MAE (TNC)",
      "Average Out-of-Sample MAE(Taxi)"
    ),
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
