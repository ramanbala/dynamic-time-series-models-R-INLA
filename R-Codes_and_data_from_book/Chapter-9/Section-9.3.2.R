# Section 9.3.2: Example: Modeling Daily TNC usage in NYC
# Data: Daily TNC usage in Manhattan
# Inputs: tnc_daily_data.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Read data
ride.3yr <-
  read.csv("Chapter-9/tnc_daily_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
ride <- ride.3yr
# Filter borough Manhattan
ride <- ride %>%
  filter(borough == "Manhattan")
n <- n_distinct(ride$date)
g <- n_distinct(ride$zoneid)
uid <- unique(ride$zoneid) #36 zones
ride.zone <- ride
# Code for Figure 9.6
ride.zone.plot <- ride %>%
  group_split(zoneid)
plot.zone <- vector("list", 9)
for (z in 1:9) {
  plot.df <- ride.zone.plot[[z]] %>%
    mutate(time = 1:n) %>%
    select(time, tnc, taxi)
  plot.zone[[z]] <-
    multiline.plot(
      plot.df,
      xlab = "day",
      ylab = "counts",
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
# Model Z1
# Indexes
id.t <- 1:n
trend <- 1:n
seas.per <- 7
costerm <- cos(2 * pi * trend / seas.per)
sinterm <- sin(2 * pi * trend / seas.per)
corr.zone <- vector("list", g)
prec.int <- vector("list", g)
alpha.coef <- taxi.coef <- subway.coef <- vector("list", g)
trend.coef <- cos.coef <- sin.coef <- vector("list", g)
dic.zone <- waic.zone <- mae.zone <- vector("list", g)
# Models for all zones
for (k in 1:g) {
  ride.temp <- ride.zone[ride.zone$zoneid == uid[k],]
  ride.temp$tnc <- round(ride.temp$tnc / 100)
  ride.temp$taxi <- round(ride.temp$taxi / 100)
  ride.temp$subway <- round(ride.temp$subway / 1000)
  zone.dat <-
    cbind.data.frame(ride.temp, trend, costerm, sinterm, id.t)
  lm.fit.tnc <-
    lm(ride.temp$tnc ~ trend + costerm + sinterm, data = zone.dat)
  res.tnc <- lm.fit.tnc$resid
  lm.fit.taxi <-
    lm(ride.temp$taxi ~ trend + sin(2 * pi * trend / 7) +
         cos(2 * pi * trend / 7),
       data = zone.dat)
  res.taxi <- lm.fit.taxi$resid
  corr.zone[[k]] <- cor(res.tnc, res.taxi)
  #ccf(res.tnc, res.taxi)
  formula.zone.Z1 <-
    tnc ~ 1 + trend + costerm + sinterm + taxi + subway +
    f(id.t, model = "rw1", constr = FALSE)
  model.zone.Z1 <- inla(
    formula.zone.Z1,
    family = "poisson",
    data = zone.dat,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = TRUE,
      cpo = TRUE
    )
  )
  prec.int[[k]] <- model.zone.Z1$summary.hyperpar$mean[1]
  alpha.coef[[k]] <- model.zone.Z1$summary.fixed$mean[1]
  trend.coef[[k]] <- model.zone.Z1$summary.fixed$mean[2]
  cos.coef[[k]] <- model.zone.Z1$summary.fixed$mean[3]
  sin.coef[[k]] <- model.zone.Z1$summary.fixed$mean[4]
  taxi.coef[[k]] <- model.zone.Z1$summary.fixed$mean[5]
  subway.coef[[k]] <- model.zone.Z1$summary.fixed$mean[6]
  dic.zone[[k]] <- model.zone.Z1$dic$dic
  waic.zone[[k]] <- model.zone.Z1$waic$waic
  fit.zone <- model.zone.Z1$summary.fitted.values$mean
  mae.zone[[k]] <- sum(abs(ride.temp$tnc - fit.zone)) / n
}
# Model results
model.zone.Z1.result <-
  list(
    corr.zone = unlist(corr.zone),
    prec.int = unlist(prec.int),
    alpha.coef = unlist(alpha.coef),
    trend.coef = unlist(trend.coef),
    cos.coef = unlist(cos.coef),
    sin.coef = unlist(sin.coef),
    taxi.coef = unlist(taxi.coef),
    subway.coef = unlist(subway.coef),
    dic.zone = unlist(dic.zone),
    waic.zone = unlist(waic.zone),
    mae.zone = unlist(mae.zone)
  )
out.corr <- model.zone.Z1.result$corr.zone
summary(out.corr)
# Code for Figure 9.7
hist(out.corr,
     main = "",
     xlab = "correlation",
     ylab = "")
# Code for Figure 9.8
par(mfrow = c(3, 3))
out.prec.int <- model.zone.Z1.result$prec.int
#summary(out.prec.int)
hist(
  out.prec.int,
  main = "",
  xlab = expression(sigma[w][0] ^ 2),
  ylab = ""
)
out.alpha <- model.zone.Z1.result$alpha.coef
#summary(out.alpha)
hist(out.alpha,
     main = "",
     xlab = "alpha",
     ylab = "")
out.trend <- model.zone.Z1.result$trend.coef
#summary(out.trend)
hist(out.trend,
     main = "",
     xlab = "trend",
     ylab = "")
out.cos <- model.zone.Z1.result$cos.coef
#summary(out.cos)
hist(out.cos,
     main = "",
     xlab = expression(cos),
     ylab = "")
out.sin <- model.zone.Z1.result$sin.coef
#summary(out.sin)
hist(out.sin,
     main = "",
     xlab = expression(sin),
     ylab = "")
out.taxi <- model.zone.Z1.result$taxi.coef
#summary(out.taxi)
hist(out.taxi,
     main = "",
     xlab = "taxi",
     ylab = "")
out.subway <- model.zone.Z1.result$subway.coef
#summary(out.subway)
hist(out.subway,
     main = "",
     xlab = "subway",
     ylab = "")

# Model Z2
id.t <- 1:n
trend <- 1:n
seas.per <- 7
costerm <- cos(2 * pi * trend / seas.per)
sinterm <- sin(2 * pi * trend / seas.per)
prec.int <-  phi.int <- vector("list", g)
alpha.coef <- subway.coef <- taxi.coef <- vector("list", g)
trend.coef <- cos.coef <- sin.coef <- vector("list", g)
dic.zone <- waic.zone <- mae.zone <- vector("list", g)
for (k in 1:g) {
  ride.temp <- ride.zone[ride.zone$zoneid == uid[k],]
  ride.temp$tnc <- round(ride.temp$tnc / 100)
  ride.temp$taxi <- round(ride.temp$taxi / 100)
  ride.temp$subway <- round(ride.temp$subway / 1000)
  zone.dat <-
    cbind.data.frame(ride.temp, trend, costerm, sinterm, id.t)
  formula.zone.Z2 <-
    tnc ~ 1 + trend + costerm + sinterm + taxi + subway +
    f(id.t, model = "ar1")
  model.zone.Z2 <- inla(
    formula.zone.Z2,
    family = "poisson",
    data = zone.dat,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = TRUE,
      cpo = TRUE
    )
  )
  prec.int[[k]] <- model.zone.Z2$summary.hyperpar$mean[1]
  phi.int[[k]] <- model.zone.Z2$summary.hyperpar$mean[2]
  alpha.coef[[k]] <- model.zone.Z2$summary.fixed$mean[1]
  trend.coef[[k]] <- model.zone.Z2$summary.fixed$mean[2]
  cos.coef[[k]] <- model.zone.Z2$summary.fixed$mean[3]
  sin.coef[[k]] <- model.zone.Z2$summary.fixed$mean[4]
  taxi.coef[[k]] <- model.zone.Z2$summary.fixed$mean[5]
  subway.coef[[k]] <- model.zone.Z2$summary.fixed$mean[6]
  dic.zone[[k]] <- model.zone.Z2$dic$dic
  waic.zone[[k]] <- model.zone.Z2$waic$waic
  fit.zone <- model.zone.Z2$summary.fitted.values$mean
  mae.zone[[k]] <- sum(abs(ride.temp$tnc - fit.zone)) / n
}
model.zone.Z2.result <- list(
  prec.int = unlist(prec.int),
  phi.int = unlist(phi.int),
  alpha.coef = unlist(alpha.coef),
  trend.coef = unlist(trend.coef),
  cos.coef = unlist(cos.coef),
  sin.coef = unlist(sin.coef),
  taxi.coef = unlist(taxi.coef),
  subway.coef = unlist(subway.coef),
  dic.zone = unlist(dic.zone),
  waic.zone = unlist(waic.zone),
  mae.zone = unlist(mae.zone)
)
summary(model.zone.Z2)
# Model Z2 fitted values
fit.model.zone.Z2 <- model.zone.Z2$summary.fitted.values
# Model Z3
id.t <- 1:n
trend <- 1:n
seas.per <- 7
costerm <- cos(2 * pi * trend / seas.per)
sinterm <- sin(2 * pi * trend / seas.per)
#corr.zone <- vector("list", g)
prec.int <- vector("list", g)
alpha.coef <- subway.coef <- taxi.coef <- vector("list", g)
cos.coef <- sin.coef <- vector("list", g)
dic.zone <- waic.zone <- mae.zone <- vector("list", g)
#par(mfrow=c(3,3))
for (k in 1:g) {
  ride.temp <- ride.zone[ride.zone$zoneid == uid[k], ]
  ride.temp$tnc <- round(ride.temp$tnc / 100)
  ride.temp$taxi <- round(ride.temp$taxi / 100)
  ride.temp$subway <- round(ride.temp$subway / 1000)
  zone.dat <-
    cbind.data.frame(ride.temp, trend, costerm, sinterm, id.t)
  # # Some EDA
  #   lm.fit.tnc <- lm(ride.temp$tnc~ trend + costerm + sinterm, data=zone.dat)
  #   res.tnc <- lm.fit.tnc$resid
  #   lm.fit.taxi<- lm(ride.temp$taxi~ trend + costerm + sinterm,data=zone.dat)
  #   res.taxi <- lm.fit.taxi$resid
  # #  ccf(res.tnc,res.taxi)
  #   corr.zone[[k]] <- cor(res.tnc,res.taxi)
  
  # Model Z3: Dynamic rw1 intercept. No Linear trend.
  formula.zone.Z3 <- tnc ~ 1 +  costerm + sinterm + taxi + subway +
    f(id.t, model = "rw1", constr = FALSE)
  model.zone.Z3 <- inla(
    formula.zone.Z3,
    family = "poisson",
    data = zone.dat,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(
      dic = TRUE,
      cpo = TRUE,
      waic = TRUE,
      config = TRUE,
      cpo = TRUE
    )
  )
  prec.int[[k]] <- model.zone.Z3$summary.hyperpar$mean[1]
  alpha.coef[[k]] <- model.zone.Z3$summary.fixed$mean[1]
  cos.coef[[k]] <- model.zone.Z3$summary.fixed$mean[2]
  sin.coef[[k]] <- model.zone.Z3$summary.fixed$mean[3]
  taxi.coef[[k]] <- model.zone.Z3$summary.fixed$mean[4]
  subway.coef[[k]] <- model.zone.Z3$summary.fixed$mean[5]
  dic.zone[[k]] <- model.zone.Z3$dic$dic
  waic.zone[[k]] <- model.zone.Z3$waic$waic
  # fitted values
  fit.zone <- model.zone.Z3$summary.fitted.values$mean
  #ts.plot(ride.temp$tnc)
  #lines(fit.zone,col="blue")
  # In-sample MAE
  mae.zone[[k]] <- sum(abs(ride.temp$tnc - fit.zone)) / n
}
summary(model.zone.Z3)
fit.model.zone.Z3 <- model.zone.Z3$summary.fitted.values
model.zone.Z3.result <- list(
  prec.int = unlist(prec.int),
  phi.int = unlist(phi.int),
  alpha.coef = unlist(alpha.coef),
  trend.coef = unlist(trend.coef),
  cos.coef = unlist(cos.coef),
  sin.coef = unlist(sin.coef),
  taxi.coef = unlist(taxi.coef),
  subway.coef = unlist(subway.coef),
  dic.zone = unlist(dic.zone),
  waic.zone = unlist(waic.zone),
  mae.zone = unlist(mae.zone)
)
# Model Z4. Hierarchical model aggregating across zones
ride.3yr <-
  read.csv("Chapter-9/tnc_daily_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
ride <- ride.3yr
ride <- ride %>%
  filter(borough == "Manhattan")
ride$tnc <- round(ride$tnc / 100)
ride$taxi <- round(ride$taxi / 100)
ride$subway <- round(ride$subway / 1000)
n <- n_distinct(ride$date)
g <- n_distinct(ride$zoneid)
uid <- unique(ride$zoneid)
ride.zone <- ride
ride.hzone <- ride.zone %>%
  filter(zoneid != "ID127" & zoneid != "ID153")
g1 <- n_distinct(ride.hzone$zoneid)
trend <- 1:n
htrend <- rep(trend, g1)
hcosterm <- rep(costerm, g1)
hsinterm <- rep(sinterm, g1)
# Case Z4a
id.b0 <- rep(1:n, g1)
re.b0 <- id.alpha <- rep(1:g1, each = n)
fact.alpha <- as.factor(id.alpha)
ride.hzone.data.fe <-
  cbind.data.frame(ride.hzone, htrend, hcosterm, hsinterm, id.b0, re.b0,
                   fact.alpha)
formula.hzone.fe <-
  tnc ~ -1 + fact.alpha + htrend + hcosterm + hsinterm + taxi + subway +
  f(id.b0, model = "rw1", replicate = re.b0)
model.hzone.fe <- inla(
  formula.hzone.fe,
  data = ride.hzone.data.fe,
  family = "poisson",
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)
summary(model.hzone.fe)
summary.model.hzone.fe <- summary(model.hzone.fe)
format.inla.out(summary.model.hzone.fe$fixed[30:39, c(1, 2)])
format.inla.out(summary.model.hzone.fe$hyperpar[, c(1, 2)])
tail(model.hzone.fe$summary.fixed[c(1, 2)], 2)
# Case 4b
id.b0 <- rep(1:n, g1)
re.b0 <- id.alpha <- rep(1:g1, each = n)
prec.prior <-
  list(prec = list(param = c(0.001, 0.001), initial = 20))
ride.hzone.data.re <-
  cbind.data.frame(ride.hzone, htrend, hcosterm, hsinterm, id.b0, re.b0,
                   id.alpha)
formula.hzone.re <-
  tnc ~ -1 + htrend + hcosterm + hsinterm + taxi + subway +
  f(id.alpha, model = "iid", hyper = prec.prior) +
  f(id.b0, model = "rw1", replicate = re.b0)
model.hzone.re <- inla(
  formula.hzone.re,
  family = "poisson",
  data = ride.hzone.data.re,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary(model.hzone.re)
summary.model.hzone.re <- summary(model.hzone.re)
format.inla.out(summary.model.hzone.re$fixed)
format.inla.out(summary.model.hzone.re$hyperpar[, c(1, 2)])
# Posterior sample
## inla posterior samples for boxplot of alphas
post.sample.hzone.re <-
  inla.posterior.sample(n = 500, model.hzone.re)
model.hzone.re$misc$configs$contents
# Code for Figure 9.10
alpha <- list()
for (k in 1:length(post.sample.hzone.re)) {
  alpha[[k]] <- post.sample.hzone.re[[k]]$latent[grep("id.alpha",
                                                      rownames(post.sample.hzone.re[[k]]$latent))]
  
}
alpha.mat <- bind_cols(alpha)
boxplot(
  t(alpha.mat),
  outline = F,
  xlab = "Zone",
  ylab = expression(alpha)
)
# Code for Table 9.1
##### DIC
z1.dic <- mean(model.zone.Z1.result$dic.zone)
z2.dic <- mean(model.zone.Z2.result$dic.zone)
z3.dic <- mean(model.zone.Z3.result$dic.zone)
z4a.dic <- model.hzone.fe$dic$dic
z4b.dic <- model.hzone.re$dic$dic
dic.models <- c(z1.dic, z2.dic, z3.dic, z4a.dic, z4b.dic)
###### WAIC
z1.waic <- mean(model.zone.Z1.result$waic.zone)
z2.waic <- mean(model.zone.Z2.result$waic.zone)
z3.waic <- mean(model.zone.Z3.result$waic.zone)
z4a.waic <- model.hzone.fe$waic$waic
z4b.waic <- model.hzone.re$waic$waic
waic.models <- c(z1.waic, z2.waic, z3.waic, z4a.waic, z4b.waic)
####### MAE (In sample)
z1.mae <- mean(model.zone.Z1.result$mae.zone)
z2.mae <- mean(model.zone.Z2.result$mae.zone)
z3.mae <- mean(model.zone.Z3.result$mae.zone)
fit.model.hzone.fe <- model.hzone.fe$summary.fitted.values$mean
z4a.mae <- mae(ride.hzone.data.fe$tnc, fit.model.hzone.fe) %>%
  str_remove("MAE is ") %>% as.numeric
fit.model.hzone.re <- model.hzone.re$summary.fitted.values$mean
z4b.mae <- mae(ride.hzone.data.re$tnc, fit.model.hzone.re) %>%
  str_remove("MAE is ") %>% as.numeric
mae.models <- c(z1.mae, z2.mae, z3.mae, z4a.mae, z4b.mae)
model.sel.tnc.daily <-
  cbind.data.frame(
    models = c("Model Z1", "Model Z2",
               "Model Z3", "Model Z4a", "Model Z4b"),
    dic.models,
    waic.models,
    mae.models
  )
model.sel.tnc.daily %>%
  kable(
    caption = "In-sample model comparisons.",
    col.names = c("Model", "DIC", "WAIC", "Average in-sample MAE"),
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
