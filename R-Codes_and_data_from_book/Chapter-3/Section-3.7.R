# Section 3.7 - Forecasting states and observations
source("Chapter-3/Section-3.3.R")
# source("Section-3.3.R")
# 10 step future forecast of states
format.inla.out(tail(model.ar1.level$summary.random$id.x[, c(1:3)], 10))
# 10 step ahead forecast of y
format.inla.out(tail(model.ar1.level$summary.fitted.values[, c(1:3)], 10))
# Code for Figure 3.13
obs <- c(y.ar1.level)
fore <-
  c(rep(NA, 490),
    tail(model.ar1.level$summary.fitted.values$mean, 10))
fore.low <- c(
  rep(NA, 490),
  tail(model.ar1.level$summary.fitted.values$mean, 10) -
    2 * tail(model.ar1.level$summary.fitted.values$sd, 10)
)
fore.up <- c(
  rep(NA, 490),
  tail(model.ar1.level$summary.fitted.values$mean, 10) +
    2 * tail(model.ar1.level$summary.fitted.values$sd, 10)
)

plot.df <-
  cbind.data.frame(time = 1:500, obs, fore, fore.low, fore.up)
plot.df %>%
  tail(100) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = obs), color = "#D55E00") +
  geom_line(aes(y = fore), color = "#0072B2") +
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) +
  geom_vline(xintercept = 491,
             ylab = "",
             size = 0.2) +
  xlab(label = "t") +
  ylab(label = expression(y[t]))
