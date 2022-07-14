# Section 13.3.1: Example: TNC and Taxi counts based on daily data
# Data: Daily TNC and taxi usage in Manhattan for 3 years
# Inputs: "tnctaxi_daily_data_Manhattan.csv"
rm(list = ls())
# Required packages
library(mvtnorm)
library(INLA)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# Read data
ride.m <-
  read.csv(
    "Chapter-13/tnctaxi_daily_data_Manhattan.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
n <- n_distinct(ride.m$date)
g1 <- n_distinct(ride.m$zoneid)
# Split data into train and test
n.hold <- 14 # 2 weeks
n.train <- n - n.hold
df.zone <- ride.m %>%
  group_split(zoneid)
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
# Model LCM1 for TNC and Taxi counts
# Indexes and replicates
N <- 2 * n
trend <- rep(1:n, 2)
htrend <- rep(trend, g1)
seas.per <- 7
costerm <- cos(2 * pi * trend / seas.per)
sinterm <- sin(2 * pi * trend / seas.per)
hcosterm <- rep(costerm, g1)
hsinterm <- rep(sinterm, g1)
t.index <- rep(c(1:N), g1)
b0.tnc <- rep(c(1:n, rep(NA, n)), g1)
b0.taxi <- rep(c(rep(NA, n), 1:n), g1)
re.b0.taxi <- re.b0.tnc <- re.tindex <- rep(1:g1, each = N)
id.alpha.tnc <- rep(1:g1, each = N)
fact.alpha.tnc <- as.factor(id.alpha.tnc)
id.alpha.taxi <- rep(1:g1, each = N)
fact.alpha.taxi <- as.factor(id.alpha.taxi)
# Data frame
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
data.ride.lcm1 <-
  cbind.data.frame(
    y,
    t.index,
    htrend,
    hcosterm,
    hsinterm,
    fact.alpha.tnc,
    fact.alpha.taxi,
    b0.tnc,
    b0.taxi,
    re.b0.tnc,
    re.b0.taxi,
    re.tindex,
    subway.tnc,
    subway.taxi
  )
# Poisson - Model formula and fit
formula.ride.lcm1 <-
  y ~ -1 + fact.alpha.tnc + fact.alpha.taxi +
  htrend + hcosterm + hsinterm +
  subway.tnc + subway.taxi +
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
model.pois.lcm1 <- inla(
  formula.ride.lcm1,
  data = data.ride.lcm1,
  family = "poisson",
  control.inla = list(h = 1e-5, tolerance = 1e-3),
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.fixed = list(expand.factor.strategy = "inla")
)
summary(model.pois.lcm1)
# Posterior samples and fitted values
n.sample <- 1000
post.sample.pois.lcm1 <-
  inla.posterior.sample(n = n.sample, result = model.pois.lcm1)
model.pois.lcm1$misc$configs$contents
x <- post.sample.pois.lcm1[[1]]
fit.tnc <- fit.taxi <- list("vector", g1)
fit.post.sample.lcm1 <- function(x) {
  fit.zone <-
    cbind.data.frame(
      fit = exp(x$latent[grep("Predictor", rownames(x$latent))]),
      zone = rep(unique(ride.m$zoneid), each = N),
      tnc.taxi = rep(c("tnc", "taxi"), each = n)
    )
  fit.tnc <-
    fit.zone %>% filter(tnc.taxi == "tnc") %>% select(c(fit, zone)) %>%
    mutate(time = rep(1:n, g1))
  fit.taxi <-
    fit.zone %>% filter(tnc.taxi == "taxi") %>% select(c(fit, zone)) %>%
    mutate(time = rep(1:n, g1))
  return(list(fit.tnc = fit.tnc, fit.taxi = fit.taxi))
}
fits <-
  post.sample.pois.lcm1 %>% lapply(function(x)
    fit.post.sample.lcm1(x))
# Fitted values
tnc.fit <- fits %>% lapply(function(x)
  x$fit.tnc) %>%
  bind_rows %>%
  group_by(zone, time) %>%
  summarise_all(mean) %>%
  pivot_wider(id_cols = time,
              names_from = zone,
              values_from = fit) %>%
  select(-time)
taxi.fit <- fits %>% lapply(function(x)
  x$fit.taxi) %>%
  bind_rows %>%
  group_by(zone, time) %>%
  summarise_all(mean) %>%
  pivot_wider(id_cols = time,
              names_from = zone,
              values_from = fit) %>%
  select(-time)
fits <- list(tnc.fit = tnc.fit, taxi.fit = taxi.fit)
# Code for Figure 13.3
library(gridExtra)
tnc.fit <- fits$tnc.fit
taxi.fit <- fits$taxi.fit
zone <- "ID107"
zone <- match(zone, unique(df.zone.append$zoneid))
tnc.obs <- df.zone[[zone]]$tnc
plot.data.tnc <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    tnc.obs = tnc.obs,
    tnc.fit = unlist(tnc.fit[, zone])
  ))
plot.tnc <- multiline.plot(
  plot.data.tnc,
  xlab = "day",
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
    taxi.fit = unlist(taxi.fit[, zone])
  ))
plot.taxi <- multiline.plot(
  plot.data.taxi,
  xlab = "day",
  ylab = "Taxi usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
grid.arrange(plot.tnc, plot.taxi)
## DIC and WAIC
dic.pois.lcm1 <- summary.model.pois.lcm1$dic$dic
waic.pois.lcm1 <- summary.model.pois.lcm1$waic$waic
## MAE
tnc.fit <- fits$tnc.fit
taxi.fit <- fits$taxi.fit
tnc.fit.oos <- tail(tnc.fit, n.hold)
taxi.fit.oos <- tail(taxi.fit, n.hold)
tnc.holdout <- test.df %>%
  group_split(zoneid) %>%
  lapply(function(x)
    select(x, tnc)) %>%
  bind_cols()
colnames(tnc.holdout) <- colnames(tnc.fit.oos)
mae.tnc.pois.lcm1 <- abs(tnc.holdout - tnc.fit.oos) %>%
  colMeans() %>% mean()
taxi.holdout <- test.df %>%
  group_split(zoneid) %>%
  lapply(function(x)
    select(x, taxi)) %>%
  bind_cols()
colnames(taxi.holdout) <- colnames(taxi.fit.oos)
mae.taxi.pois.lcm1 <- abs(taxi.holdout - taxi.fit.oos) %>%
  colMeans() %>% mean()
# Alternate code
y <- df.zone.append %>%
  group_split(zoneid) %>%
  lapply(function(x)
    unlist(select(x, tnc, taxi))) %>%
  unlist()
y <- df.zone.append %>%
  group_split(zoneid) %>%
  lapply(function(x)
    unlist(select(x, tnc, taxi))) %>%
  unlist()
y.alt.tnc <- df.zone.append %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(x$tnc, rep(NA, n))) %>%
  unlist()
y.alt.taxi <- df.zone.append %>%
  group_split(zoneid) %>%
  lapply(function(x)
    c(rep(NA, n), x$taxi)) %>%
  unlist()
Y <- matrix(c(y.alt.tnc, y.alt.taxi), ncol = 2)
formula.pois.pois.lcm1 <-
  Y ~ -1 +
  f(id.alpha.tnc, model = "iid", hyper = prec.prior) +
  f(id.alpha.taxi, model = "iid", hyper = prec.prior) +
  htrend + hcosterm + hsinterm +
  subway.tnc + subway.taxi +
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
model.pois.pois.lcm1 <- inla(
  formula.pois.pois.lcm1,
  data = data.ride.lcm1,
  family = c("poisson", "poisson"),
  control.inla = list(h = 1e-5,
                      tolerance = 1e-3),
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
)
summary(model.pois.pois.lcm1)
# Model LCM2 for TNC and Taxi counts
# Indexes and replicates
n.hold <- 14 # 2 weeks
n.train <- n - n.hold
df.zone <- ride.m %>%
  group_split(zoneid)
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
N <- 2 * n
trend <- rep(1:n, 2)
htrend <- rep(trend, g1)
seas.per <- 7
costerm <- cos(2 * pi * trend / seas.per)
sinterm <- sin(2 * pi * trend / seas.per)
hcosterm <- rep(costerm, g1)
hsinterm <- rep(sinterm, g1)
t.index <- rep(c(1:N), g1)
b0.tnc <- id.re.b0.tnc <- rep(c(1:n, rep(NA, n)), g1)
b0.taxi <- id.re.b0.taxi <- rep(c(rep(NA, n), 1:n), g1)
re.tindex <- rep(1:g1, each = N)
re.b0.taxi <- re.b0.tnc <- rep(rep(c(1, 2), each = n), each = g1)
id.alpha.tnc <- rep(1:g1, each = N)
fact.alpha.tnc <- as.factor(id.alpha.tnc)
id.alpha.taxi <- rep(1:g1, each = N)
fact.alpha.taxi <- as.factor(id.alpha.taxi)
# Data frame
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
data.ride.lcm2 <-
  cbind.data.frame(
    y,
    t.index,
    htrend,
    hcosterm,
    hsinterm,
    id.alpha.tnc,
    id.alpha.taxi,
    b0.tnc,
    b0.taxi,
    re.b0.tnc,
    re.b0.taxi,
    re.tindex,
    subway.tnc,
    subway.taxi,
    id.re.b0.tnc,
    id.re.b0.taxi
  )
# Model formula and fit
formula.ride.lcm2 <-
  y ~ -1 + fact.alpha.tnc + fact.alpha.taxi +
  htrend + hcosterm + hsinterm +
  subway.tnc + subway.taxi +
  f(t.index,
    model = "iid2d",
    n = N,
    replicate = re.tindex) +
  f(b0.tnc,
    model = "rw1",
    constr = FALSE) +
  f(
    id.re.b0.tnc,
    model = "iid",
    replicate = re.b0.tnc,
    hyper = list(prec = list(fixed = T, initial = 20))
  ) +
  f(b0.taxi,
    model = "rw1",
    constr = FALSE) +
  f(
    id.re.b0.taxi,
    model = "iid",
    replicate = re.b0.taxi,
    hyper = list(prec = list(fixed = T, initial = 20))
  )
model.pois.lcm2 <- inla(
  formula.ride.lcm2,
  data = data.ride.lcm2,
  family = "poisson",
  control.inla = list(h = 1e-5,
                      tolerance = 1e-3),
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.fixed = list(expand.factor.strategy = "inla")
)
summary(model.pois.lcm2)
# Code for Figure 13.4
tnc.fit <- fits$tnc.fit
taxi.fit <- fits$taxi.fit
zone <- "ID107"
zone <- match(zone, unique(df.zone.append$zoneid))
tnc.obs <- df.zone[[zone]]$tnc
plot.data.tnc <-
  as_tibble(cbind.data.frame(
    time = 1:n,
    tnc.obs = tnc.obs,
    tnc.fit = unlist(tnc.fit[, zone])
  ))
plot.tnc <- multiline.plot(
  plot.data.tnc,
  xlab = "day",
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
    taxi.fit = unlist(taxi.fit[, zone])
  ))
plot.taxi <- multiline.plot(
  plot.data.taxi,
  xlab = "day",
  ylab = "Taxi usage",
  line.type = "solid",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  geom_vline(xintercept = n.train, size = 0.2)
grid.arrange(plot.tnc, plot.taxi)
## DIC and WAIC
dic.pois.lcm2 <- summary.model.pois.lcm2$dic$dic
waic.pois.lcm2 <- summary.model.pois.lcm2$waic$waic
## MAE
tnc.fit <- fits$tnc.fit
taxi.fit <- fits$taxi.fit
tnc.fit.oos <- tail(tnc.fit, n.hold)
taxi.fit.oos <- tail(taxi.fit, n.hold)
tnc.holdout <- test.df %>%
  group_split(zoneid) %>%
  lapply(function(x)
    select(x, tnc)) %>%
  bind_cols()
colnames(tnc.holdout) <- colnames(tnc.fit.oos)
mae.tnc.pois.lcm2 <- abs(tnc.holdout - tnc.fit.oos) %>%
  colMeans() %>% mean()
taxi.holdout <- test.df %>%
  group_split(zoneid) %>%
  lapply(function(x)
    select(x, taxi)) %>%
  bind_cols()
colnames(taxi.holdout) <- colnames(taxi.fit.oos)
mae.taxi.pois.lcm2 <- abs(taxi.holdout - taxi.fit.oos) %>%
  colMeans() %>% mean()
# Code for Table 13.2
# Criteria
mod.sel.tab <-
  as_tibble(cbind.data.frame(
    Model = c("LCM1", "LCM2"),
    DIC = c(dic.pois.lcm1, dic.pois.lcm2),
    WAIC = c(waic.pois.lcm1, waic.pois.lcm2),
    MAE.tnc = c(mae.tnc.pois.lcm1, mae.tnc.pois.lcm2),
    MAE.taxi = c(mae.taxi.pois.lcm1, mae.taxi.pois.lcm2)
  ))
kable(
  mod.sel.tab,
  caption = 'Comparing DIC, WAIC and MAE values from Models LCM1 and LCM2.',
  col.names = c("Model", "DIC", "WAIC", "Average MAE (TNC)", "Average MAE (Taxi)"),
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
