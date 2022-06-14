# Section 4.2.1: Model 1 - AR(1) with level plus noise model
# Data: Musa data
# Inputs: MusaSystem40Data.xlsx

source("Chapter-4/Section-4.2.R")
# Create index
id.x <- 1:length(y.append)
musa.ar1.dat <- cbind.data.frame(id.x, y.append)
# INLA formula 
formula.ar1 <- y.append ~ 1 + f(id.x, model = "ar1")
# INLA model 
model.ar1 <- inla(
  formula.ar1,
  family = "gaussian",
  data = musa.ar1.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.ar1)
# Marginal posterior mean of sigma2.w 
sigma2.w.post.mean = inla.emarginal(
  fun = function(x)
    exp(-x),
  marginal = model.ar1$internal.marginals.hyperpar$`Log precision for id.x`
) * (1 - model.ar1$summary.hyperpar["Rho for id.x", "mean"] ^ 2)
print(sigma2.w.post.mean)
# Filter estimates from custom function filt.inla()
filt.result.ar1 <-
  filt.inla(
    data.series = y,
    model = "ar",
    ar.order = 1,
    trend = "no",
    alpha = "yes"
  )
filt.all <- filt.result.ar1$filt.all.bind
filt.est <- c(NA, filt.all$mean)
# Smoothed estimates 
smooth.est <- model.ar1$summary.random$id$mean
# Fitted values
fit.ar1 <- model.ar1$summary.fitted.values$mean[1:n.train]
# Code for Figure 4.2: plot data, filter and smoothed estimates
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x[1:n.train],
    y = train.data$log.tbf,
    filter = filt.est,
    smooth = smooth.est[1:n.train]
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "",
  line.size = 0.8
)
# Code for Figure 4.3: marginal posterior density of phi
plot(
  inla.smarginal(model.ar1$marginals.hyperpar$`Rho for id.x`),
  main = "",
  xlab = expression(phi),
  ylab = "density",
  type = "l"
)
# Code for Figure 4.9: ACF plots
resid.ar1 <- y - fit.ar1
acf.ar1 <- acf(resid.ar1,plot = FALSE)
# Move to another script
par(mfrow = c(2, 2))
ts.plot(resid.ar1)
acf(resid.ar1)
pacf(resid.ar1)
# Forecast state variable (Section 4.3)
tail(model.ar1$summary.random$id)
# Code for values in Table 4.1
# DIC and WAIC
model.ar1$dic$dic
model.ar1$waic$waic
# CPO and PsBF
cpo.ar1 <- model.ar1$cpo$cpo
psBF.ar1 <- sum(log(cpo.ar1[1:n.train]))
# PIT
pit.ar1 <- model.ar1$cpo$pit
hist(pit.ar1)
# DIC, WAIC, PSBF using book's sourced functions_custom
model.selection.criteria(model.ar1,
                         plot.PIT = FALSE,
                         n.train = n.train)
# Marginal likelihood
model.ar1$mlik
# MAPE & MAE
yfore.ar1 <- tail(model.ar1$summary.fitted.values)$mean
yhold <- test.data$log.tbf
efore.ar1 <- yhold - yfore.ar1  # forecast errors
# MAPE
mape(yhold, yfore.ar1, "MAPE from AR(1) plus noise model is")
# MAE
mae(yhold, yfore.ar1, "MAE from AR(1) plus noise model is")