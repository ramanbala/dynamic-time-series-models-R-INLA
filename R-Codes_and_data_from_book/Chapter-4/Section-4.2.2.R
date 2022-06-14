# Section 4.2.2: Model 2 - Random walk plus noise model
# Data: Musa data
# Inputs: MusaSystem40Data.xlsx
source("Chapter-4/Section-4.2.R")
# Create index
id.w <- 1:length(y.append)
musa.rw1.dat <- cbind.data.frame(y.append, id.w)
formula.rw1 <- y.append ~ f(id.w, model = "rw1",
                            constr = FALSE) - 1
model.rw1 <- inla(
  formula.rw1,
  family = "gaussian",
  data = musa.rw1.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE
  )
)
summary.rw1 <- summary(model.rw1)
model.rw1$summary.hyperpar
# Marginal distribution of sigma2.w
sigma2.w.post.mean.rw1 <- inla.emarginal(
  fun = function(x)
    exp(-x),
  marginal = model.rw1$internal.marginals.hyperpar$`Log precision for id.w`
)
# Filter estimates from custom functions
filt.result <-
  filt.inla(
    data.series = y,
    model = "rw1",
    ar.order = 0,
    trend = "no",
    alpha = "no"
  )
filt.all <- filt.result$filt.all.bind
filt.est <- c(NA, filt.all$mean)
smooth.est <- model.rw1$summary.random$id$mean
fit.rw1 <- model.rw1$summary.fitted.values$mean[1:n.train]
# Code for Figure 4.4
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.w[1:n.train],
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
# Move to another section
# Code for Figure 4.9: ACF plots
resid.rw1 <- y - fit.rw1
par(mfrow = c(2, 2))
ts.plot(resid.rw1)
acf(resid.rw1)
pacf(resid.rw1)
# Code for Figure 4.10
fore <-
  c(rep(NA, n.train),
    tail(model.rw1$summary.fitted.values$mean, n.hold))
fore.low <- c(
  rep(NA, n.train),
  tail(model.rw1$summary.fitted.values$mean, n.hold) -
    2 * tail(model.rw1$summary.fitted.values$sd, n.hold)
)
fore.up <- c(
  rep(NA, n.train),
  tail(model.rw1$summary.fitted.values$mean, n.hold) +
    2 * tail(model.rw1$summary.fitted.values$sd, n.hold)
)

df2 <-
  cbind.data.frame(time = 1:n,
                   obs = musa$log.tbf,
                   fore,
                   fore.low,
                   fore.up)
p2 <- df2 %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = obs), color = "#D55E00") +
  geom_line(aes(y = fore), color = "#0072B2") +
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) + geom_vline(xintercept = (n.train + 1), size = 0.2) +
  ggtitle("Random walk plus noise model") +
  labs(y = "log inter-failure time")

# Forecast state variable
tail(model.rw1$summary.random$id)
# Code for values in Table 4.1
# DIC & WAIC
model.rw1$dic$dic
model.rw1$waic$waic
# CPO & PSBF
cpo.rw1 <- model.rw1$cpo$cpo
(psBF.rw1 <- sum(log(cpo.rw1[1:nrow(train.data)])))
# PIT
pit.rw1 <- model.rw1$cpo$pit
hist(pit.rw1)
#DIC, WAIC, PSBF using book's custom function
model.selection.criteria(model.rw1,
                         plot.PIT = FALSE,
                         n.train = nrow(train.data))
# Marginal likelihood
model.rw1$mlik
# MAPE & MAE
yfore.rw1 <- tail(model.rw1$summary.fitted.values)$mean
yhold <- test.data$log.tbf
efore.rw1 <- yhold - yfore.rw1  # forecast errors
# MAPE
mape(yhold, yfore.rw1, "MAPE from RW plus noise model is")
# MAE
mae(yhold, yfore.rw1, "MAE from RW plus noise model is")
