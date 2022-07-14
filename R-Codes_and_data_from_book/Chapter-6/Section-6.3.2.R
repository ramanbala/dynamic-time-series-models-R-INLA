# Section 6.3.2: Model H2. Dynamic intercept and Taxi usage
# Data: Weekly ridesourcing usage data
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# Source code with ridesourcing data from Section 6.3
source("Chapter-6/Section-6.3.R")
# Index
id.b0 <- id.subway <- id.taxi <- rep(1:n, g)
id.holiday <- id.precip <- rep(1:n, g)
re.b0 <- re.subway <- re.taxi <- rep(1:g, each = n)
re.holiday <- re.precip <- rep(1, (n * g))
re.age <- re.earn <- re.pop <- re.emp <- rep(1:g, each = n)
ride.data <-
  cbind.data.frame(
    ride,
    id.b0,
    id.subway,
    id.taxi,
    id.holiday,
    id.precip,
    re.b0,
    re.subway,
    re.taxi,
    re.holiday,
    re.precip,
    re.age,
    re.earn,
    re.pop,
    re.emp
  )
# Model formula
formula.tnc.H2 <- tnc ~ 1 + subway + holidays + precip +
  median.age + median.earnings + scaled.population + scaled.employment +
  f(id.b0,
    model = "rw1",
    constr = FALSE,
    replicate = re.b0) +
  f(id.taxi,
    taxi,
    model = "rw1",
    constr = FALSE,
    replicate = re.taxi)
# Model fit
model.tnc.H2 <- inla(
  formula.tnc.H2,
  data = ride.data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
model.tnc.H2$summary.fixed
model.tnc.H2$summary.hyperpar
# code for Figure 6.3
fit.model.H2 <- model.tnc.H2$summary.fitted.values
fit.model.H2$zoneid <- ride$zoneid
obs.fit.tnc.model.H2 <-
  cbind.data.frame(ride[, c(1, 4, 5)], fit = fit.model.H2[, c(1)])
fit.plot.H2 <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- obs.fit.tnc.model.H2 %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, obs = tnc, fit = fit)
  fit.plot.H2[[i]] <-
    multiline.plot(
      plot.data[1:n.train, ],
      title = paste(req.name[i], req.borough[i], sep = ", "),
      xlab = "week",
      ylab = "",
      line.type = "dotdash",
      line.size = 0.8
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 7)
    )
  legend <-
    get_legend(multiline.plot(
      plot.data,
      title = req.name[8],
      xlab = "week",
      ylab = "",
      line.size = 0.8
    ))
}
grid.arrange(
  fit.plot.H2[[1]],
  fit.plot.H2[[2]],
  fit.plot.H2[[3]],
  fit.plot.H2[[4]],
  fit.plot.H2[[5]],
  fit.plot.H2[[6]],
  fit.plot.H2[[7]],
  fit.plot.H2[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)
# Code for Model H2b
id.alpha <- rep(1:g, each = n)
ride.data <- cbind.data.frame(ride.data, id.alpha)
formula.tnc.H2b <- tnc ~ 1 + subway + holidays + precip +
  median.age + median.earnings + scaled.population + scaled.employment +
  f(id.alpha, model = "iid") +
  f(id.taxi,
    taxi,
    model = "rw1",
    constr = FALSE,
    replicate = re.taxi)

# Model fit
model.tnc.H2b <- inla(
  formula.tnc.H2b,
  data = ride.data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary(model.tnc.H2b)

# Code for Figure 6.4
fit.model.H2b <- model.tnc.H2b$summary.fitted.values
fit.model.H2b$zoneid <- ride$zoneid
obs.fit.tnc.model.H2b <-
  cbind.data.frame(ride[, c(1, 4, 5)], fit = fit.model.H2b[, c(1)])
fit.plot.H2b <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- obs.fit.tnc.model.H2b %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, obs = tnc, fit = fit)
  fit.plot.H2b[[i]] <-
    multiline.plot(
      plot.data[1:n.train,],
      title = paste(req.name[i], req.borough[i], sep = ", "),
      xlab = "week",
      ylab = "",
      line.type = "dotdash",
      line.size = 0.8
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 7)
    )
  legend <-
    get_legend(multiline.plot(
      plot.data,
      title = req.name[8],
      xlab = "week",
      ylab = "",
      line.size = 0.8
    ))
}
grid.arrange(
  fit.plot.H2b[[1]],
  fit.plot.H2b[[2]],
  fit.plot.H2b[[3]],
  fit.plot.H2b[[4]],
  fit.plot.H2b[[5]],
  fit.plot.H2b[[6]],
  fit.plot.H2b[[7]],
  fit.plot.H2b[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)
# Values for Tables 6.1 and 6.2
## obtain DIC, WAIC and PsBF
# Model H2
model.sel.model.H2 <-
  model.selection.criteria(model.tnc.H2.rw1, n.train = n.train)
## MAE
data.mae <- obs.fit.tnc.model.H2 %>%
  group_split(zoneid) %>%
  lapply(function(x)
    tail(x, n.hold))
mae.tnc.model.H2 <- data.mae %>%
  sapply(function(x)
    mae(x$tnc, x$fit))
zone.mae.tnc.model.H2 <-
  str_remove_all(mae.tnc.model.H2, "MAE is ") %>%
  as.numeric
mean.all.zone.mae.tnc.model.H2 <- mean(zone.mae.tnc.model.H2)
## MAPE
mape.tnc.model.H2 <- data.mae %>%
  sapply(function(x)
    mape(x$tnc, x$fit))
zone.mape.tnc.model.H2 <-
  str_remove_all(mape.tnc.model.H2, "MAPE is ") %>%
  as.numeric
mean.all.zone.mape.tnc.model.H2 <- mean(zone.mape.tnc.model.H2)
# Model H2b
## obtain DIC, WAIC and PsBF
model.sel.model.H2b <-
  model.selection.criteria(model.tnc.H2b, n.train = n.train)
## MAE
data.mae <- obs.fit.tnc.model.H2b %>%
  group_split(zoneid) %>%
  lapply(function(x)
    tail(x, n.hold))
mae.tnc.model.H2b <- data.mae %>%
  sapply(function(x)
    mae(x$tnc, x$fit))
zone.mae.tnc.model.H2b <-
  str_remove_all(mae.tnc.model.H2b, "MAE is ") %>%
  as.numeric
mean.all.zone.mae.tnc.model.H2b <- mean(zone.mae.tnc.model.H2b)
## MAPE
mape.tnc.model.H2b <- data.mae %>%
  sapply(function(x)
    mape(x$tnc, x$fit))
zone.mape.tnc.model.H2b <-
  str_remove_all(mape.tnc.model.H2b, "MAPE is ") %>%
  as.numeric
mean.all.zone.mape.tnc.model.H2b <- mean(zone.mape.tnc.model.H2b)
