# Section 4.3: Forecasting future states and responses
# Packages used
library(gridExtra)
# Source codes from Sections 4.2.1 to 4.2.4
# Warning - These scripts include codes for filter estimates and hence takes a lot of time to run
source("Chapter-4/Section-4.2.1.R")
source("Chapter-4/Section-4.2.2.R")
source("Chapter-4/Section-4.2.3.R")
source("Chapter-4/Section-4.2.4.R")
# Forecast of future states
format.inla.out(tail(model.ar1$summary.random$id[, c(1:6)]))
# Forecast of y
format.inla.out(tail(model.ar1$summary.fitted.values[, c(1:5)]))
# Code for Figure 4.10
# model-1
fore <-
  c(rep(NA, n.train),
    tail(model.ar1$summary.fitted.values$mean, n.hold))
fore.low <- c(
  rep(NA, n.train),
  tail(model.ar1$summary.fitted.values$mean, n.hold) -
    2 * tail(model.ar1$summary.fitted.values$sd, n.hold)
)
fore.up <- c(
  rep(NA, n.train),
  tail(model.ar1$summary.fitted.values$mean, n.hold) +
    2 * tail(model.ar1$summary.fitted.values$sd, n.hold)
)

df1 <-
  cbind.data.frame(time = 1:n,
                   obs = musa$log.tbf,
                   fore,
                   fore.low,
                   fore.up)
p1 <- df1 %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = obs), color = "#D55E00") +
  labs(y = "log inter-failure time") +
  geom_line(aes(y = fore), color = "#0072B2") +
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) + geom_vline(xintercept = (n.train + 1), size = 0.2) +
  ggtitle("AR(1) with level plus noise model") +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7)
  )

# model-2
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
  labs(y = "log inter-failure time") +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7)
  )

# model 3
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
  labs(y = "log inter-failure time") +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7)
  )

# model 4
fore <-
  c(rep(NA, n.train),
    tail(model.ar2$summary.fitted.values$mean, n.hold))
fore.low <- c(
  rep(NA, n.train),
  tail(model.ar2$summary.fitted.values$mean, n.hold) -
    2 * tail(model.ar2$summary.fitted.values$sd, n.hold)
)
fore.up <- c(
  rep(NA, n.train),
  tail(model.ar2$summary.fitted.values$mean, n.hold) +
    2 * tail(model.ar2$summary.fitted.values$sd, n.hold)
)

df4 <-
  cbind.data.frame(time = 1:n,
                   obs = musa$log.tbf,
                   fore,
                   fore.low,
                   fore.up)
p4 <- df4 %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = obs), color = "#D55E00") +
  geom_line(aes(y = fore), color = "#0072B2") +
  geom_ribbon(aes(ymin = fore.low, ymax = fore.up), alpha =
                .2) + geom_vline(xintercept = (n.train + 1), size = 0.2) +
  ggtitle("AR(2) plus noise model") +
  labs(y = "log inter-failure time") +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7)
  )

grid.arrange(p1, p2, p3, p4)
