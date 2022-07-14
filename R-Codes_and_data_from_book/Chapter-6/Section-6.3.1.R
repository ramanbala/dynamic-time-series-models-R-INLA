# Section 6.3.1: Model H1. Dynamic Intercept and exogenous predictors
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
formula.tnc.H1 <- tnc ~ 1 +
  f(id.b0, model = "ar1", replicate = re.b0) +
  f(id.subway, subway, model = "ar1", replicate = re.subway) +
  f(id.taxi, taxi, model = "ar1", replicate = re.taxi) +
  f(
    id.holiday,
    holidays,
    model = "ar1",
    replicate = re.holiday,
    hyper = list(theta = list(initial = 20, fixed = TRUE))
  ) +
  f(
    id.precip,
    precip,
    model = "ar1",
    replicate = re.precip,
    hyper = list(theta = list(initial = 20, fixed = TRUE))
  ) +
  f(re.age, median.age, model = "iid") +
  f(re.earn, median.earnings, model = "iid") +
  f(re.pop, scaled.population, model = "iid") +
  f(re.emp, scaled.employment, model = "iid")
# Model fit
model.tnc.H1 <- inla(
  formula.tnc.H1,
  data = ride.data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
summary.model.tnc.H1 <- summary(model.tnc.H1)
summary.model.tnc.H1$fixed
model.tnc.H1$summary.hyperpar[, c(1:2)]

# code for Figure 6.2
fit.model.H1 <- model.tnc.H1$summary.fitted.values
fit.model.H1$zoneid <- ride$zoneid
obs.fit.tnc.model.H1 <-
  cbind.data.frame(ride[, c(1, 4, 5)], fit = fit.model.H1[, c(1)])

fit.plot.H1 <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- obs.fit.tnc.model.H1 %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, obs = tnc, fit = fit)
  fit.plot.H1[[i]] <-
    multiline.plot(
      plot.data[1:n.train, ],
      title = paste(req.name[i], req.borough[i], sep = ", "),
      xlab = "week",
      ylab = "",
      line.type = "dashed",
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
  fit.plot.H1[[1]],
  fit.plot.H1[[2]],
  fit.plot.H1[[3]],
  fit.plot.H1[[4]],
  fit.plot.H1[[5]],
  fit.plot.H1[[6]],
  fit.plot.H1[[7]],
  fit.plot.H1[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)

# Values for Table 6.1 and 6.2
## DIC, WAIC and PsBF
model.sel.model.H1 <- model.selection.criteria(summary.model.tnc.h1, n.train = n.train)
## MAE
data.mae <- obs.fit.tnc.model.H1 %>% 
  group_split(zoneid) %>% 
  lapply(function(x) tail(x, n.hold))
mae.tnc.model.H1 <- data.mae %>% 
  sapply(function(x) mae(x$tnc, x$fit))
zone.mae.tnc.model.H1 <- str_remove_all(mae.tnc.model.H1,"MAE is ") %>% 
  as.numeric 
mean.all.zone.mae.tnc.model.H1 <- mean(zone.mae.tnc.model.H1)
## MAPE
mape.tnc.model.H1 <- data.mae %>% 
  sapply(function(x) mape(x$tnc, x$fit))
zone.mape.tnc.model.H1 <- str_remove_all(mape.tnc.model.H1,"MAPE is ") %>% 
  as.numeric 
mean.all.zone.mape.tnc.model.H1 <- mean(zone.mape.tnc.model.H1)