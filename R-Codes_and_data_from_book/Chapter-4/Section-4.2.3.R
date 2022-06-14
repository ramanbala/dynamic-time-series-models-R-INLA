# Section 4.2.3: Model 3 - AR(1) with trend plus noise model
# Data: Musa data
# Inputs: MusaSystem40Data.xlsx
source("Chapter-4/Section-4.2.R")
# Create index
id.x <- trend <- 1:length(y.append)
musa.ar1trend.dat <- cbind.data.frame(id.x, y.append, trend)
formula.ar1trend <- y.append ~ f(id.x, model = "ar1") + trend - 1
model.ar1trend <- inla(
  formula.ar1trend,
  family = "gaussian",
  data = musa.ar1trend.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.ar1trend)
model.ar1trend$summary.hyperpar
# Filter estimates from custom functions
filt.result <-
  filt.inla(
    data.series = y,
    model = "ar",
    ar.order = 1,
    trend = "yes",
    alpha = "no"
  )
filt.all <- filt.result$filt.all.bind
filt.est <- c(NA, filt.all$mean)
smooth.est <- model.ar1trend$summary.random$id$mean
fit.ar1trend <- model.ar1trend$summary.fitted.values$mean[1:n.train]
# Code for Figure 4.5
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x[1:n.train],
    y = train.data$log.tbf,
    fit = fit.ar1trend,
    smooth.state = smooth.est[1:n.train]
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "",
  line.type = "solid",
  line.size = 0.7,
  line.color = c("red", "blue", "green")
)

# Code for Figure 4.6
plot(
  inla.smarginal(model.ar1trend$marginals.fixed$trend),
  type = "l",
  main = "",
  xlab = expression(beta),
  ylab = "density"
)

# Move below codes to another script
# Code for Figure 4.9: ACF plots
resid.ar1trend <- y - fit.ar1trend
par(mfrow = c(2, 2))
ts.plot(resid.ar1trend)
acf(resid.ar1trend)
pacf(resid.ar1trend)
# Code for Figure 4.10
fore <-
  c(rep(NA, n.train),
    tail(model.ar1trend$summary.fitted.values$mean, n.hold))
fore.low <- c(
  rep(NA, n.train),
  tail(model.ar1trend$summary.fitted.values$mean, n.hold) -
    2 * tail(model.ar1trend$summary.fitted.values$sd, n.hold)
)
fore.up <- c(
  rep(NA, n.train),
  tail(model.ar1trend$summary.fitted.values$mean, n.hold) +
    2 * tail(model.ar1trend$summary.fitted.values$sd, n.hold)
)

df3 <-
  cbind.data.frame(time = 1:n,
                   obs = musa$log.tbf,
                   fore,
                   fore.low,
                   fore.up)
p3 <- df3 %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = obs), color = "#D55E00") +
  geom_line(aes(y = fore), color = "#0072B2") +
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) + geom_vline(xintercept = (n.train + 1), size = 0.2) +
  ggtitle("AR(1) with trend plus noise model") +
  labs(y = "log inter-failure time")

# Forecast state variable
tail(model.ar1trend$summary.random$id)
# Code for values in Table 4.1
# DIC & WAIC
model.ar1trend$dic$dic
model.ar1trend$waic$waic
# CPO & PSBF
cpo.ar1trend <- model.ar1trend$cpo$cpo
(psBF.ar1trend <- sum(log(cpo.ar1trend[1:nrow(train.data)])))
# PIT
pit.ar1trend <- model.ar1trend$cpo$pit
hist(pit.ar1trend)

# DIC, WAIC, PSBF using book's custom function
model.selection.criteria(model.ar1trend,
                         plot.PIT = FALSE,
                         n.train = nrow(train.data))
# Marginal likelihood
model.ar1trend$mlik

# MAPE & MAE
yfore.ar1trend <- tail(model.ar1trend$summary.fitted.values)$mean
yhold <- test.data$log.tbf
efore.ar1trend <- yhold - yfore.ar1trend  # forecast errors
# MAPE
mape(yhold,
     yfore.ar1trend,
     "MAPE from AR(1) with trend plus noise model is")
# MAE
mae(yhold,
    yfore.ar1trend,
    "MAE from AR(1) with trend plus noise model is")
