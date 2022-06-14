# Section 4.2.4: Model 4 - AR(2) with level plus noise
# Data: Musa data
# Inputs: MusaSystem40Data.xlsx
source("Chapter-4/Section-4.2.R")
# Create index
id.x <- 1:length(y.append)
musa.ar2.dat <- cbind.data.frame(id.x, y.append)
formula.ar2 <- y.append ~ 1 + f(id.x,
                                model = "ar",
                                order = 2,
                                constr = FALSE)
model.ar2 <- inla(
  formula.ar2,
  family = "gaussian",
  data = musa.ar2.dat,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.ar2)
model.ar2$summary.fixed
model.ar2$summary.hyperpar
# Filter estimates from custom functions
filt.result.ar2 <- filt.inla(data.series = y, model = "ar", ar.order = 2,
                             trend = "no", alpha = "yes")
filt.all <- filt.result.ar2$filt.all.bind
filt.est <- c(NA, filt.all$mean)
smooth.est <- model.ar2$summary.random$id$mean
ar.p <- 2
fit.ar2 <- model.ar2$summary.fitted.values$mean[(ar.p + 1):n.train]
# Code for Figure 4.7
plot.df <- as_tibble(
  cbind.data.frame(
    time = id.x[(ar.p + 1):n.train],
    y = train.data$log.tbf[(ar.p + 1):n.train],
    filter.state = filt.est,
    smooth.state = smooth.est[(ar.p + 1):n.train]
  )
)
multiline.plot(
  plot.df,
  title = "",
  xlab = "t",
  ylab = "",
  line.type = "solid",
  line.size = 0.7,
  line.color = c("red", "yellow", "green")
)
# Code for Figure 4.8
n.samples <- 10000
pacfs <- inla.hyperpar.sample(n.samples, model.ar2)[, 3:4]
phis <- apply(pacfs, 1L, inla.ar.pacf2phi)
par(mfrow = c(1, 2))
plot(
  density(phis[1, ]),
  type = "l",
  main = "",
  xlab = expression(phi[1]),
  ylab = "density"
)
plot(
  density(phis[2, ]),
  type = "l",
  main = "",
  xlab = expression(phi[2]),
  ylab = "density"
)
# Move below codes to another script

# Code for Figure 4.9: ACF plots
resid.ar2 <- y[(ar.p + 1):n.train] - fit.ar2
acf.ar2 <- acf(resid.ar2, plot = FALSE)
plot(acf.ar2, main = "Model 4: AR(2) with level plus noise", cex.main = 0.6)
# Code for Figure 4.10
fore <- c(rep(NA, n.train), tail(model.ar2$summary.fitted.values$mean, n.hold))
fore.low <- c(rep(NA, n.train),
              tail(model.ar2$summary.fitted.values$mean,n.hold)-
                2*tail(model.ar2$summary.fitted.values$sd,n.hold))
fore.up <- c(rep(NA, n.train),
             tail(model.ar2$summary.fitted.values$mean,n.hold)+
               2* tail(model.ar2$summary.fitted.values$sd,n.hold))

df4 <- cbind.data.frame(time = 1:n,obs = musa$log.tbf, fore, fore.low, fore.up)
p4 <- df4 %>% 
  ggplot(aes(x=time)) +
  geom_line(aes(y = obs), color = "#D55E00")+
  geom_line(aes(y = fore), color = "#0072B2")+
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) +geom_vline(xintercept = (n.train+1), size=0.2) +
  ggtitle("AR(2) plus noise model") +
  labs(y = "log inter-failure time") 

# Forecast state variable
tail(model.ar2$summary.random$id)
# Code for values in Table 4.1
# DIC & WAIC
model.ar2$dic$dic
model.ar2$waic$waic
# CPO & PSBF
cpo.ar2 <- model.ar2$cpo$cpo
(psBF.ar2 <- sum(log(cpo.ar2[1:nrow(train.data)])))
# PIT
pit.ar2 <- model.ar2$cpo$pit
hist(pit.ar2)

# DIC, WAIC, PSBF using book's custom function
model.selection.criteria(model.ar2,
                         plot.PIT = FALSE,
                         n.train = nrow(train.data))
# Marginal likelihood
model.ar2$mlik

# MAPE & MAE
yfore.ar2 <- tail(model.ar2$summary.fitted.values)$mean
yhold <- test.data$log.tbf
efore.ar2 <- yhold - yfore.ar2  # forecast errors
# MAPE
mape(yhold,
     yfore.ar2,
     "MAPE from AR(2) plus noise model is")
# MAE
mae(yhold,
    yfore.ar2,
    "MAE from AR(2) plus noise model is")

