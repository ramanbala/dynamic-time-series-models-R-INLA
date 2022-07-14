# Section 6.3.3: Model H3. Taxi usage varies by time and zone
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
# Replicates
re.b0 <- re.subway <- re.taxi <- rep(1:g, each = n)
re.holiday <- re.precip <- rep(1, (n * g))
re.age <-
  re.earn <- re.pop <- re.emp <- rep(1:g, each = n)
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
formula.tnc.H3 <- tnc ~ subway + holidays + precip +
  median.age + median.earnings + scaled.population + scaled.employment +
  f(id.taxi,
    taxi,
    model = "ar1",
    constr = FALSE,
    replicate = re.taxi)
# Model fit
model.tnc.H3 <- inla(
  formula.tnc.H3,
  data = ride.data,
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  )
)
# Summary of hyperparameters
model.tnc.H3$summary.fixed
model.tnc.H3$summary.hyperpar
# code for Figure 6.5 
fit.model.H3 <- model.tnc.H3$summary.fitted.values
fit.model.H3$zoneid <- ride$zoneid
obs.fit.tnc.model.H3 <-
  cbind.data.frame(ride[, c(1, 4, 5)], fit = fit.model.H3[, c(1)])
fit.plot.H3 <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- obs.fit.tnc.model.H3 %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, obs = tnc, fit = fit)
  fit.plot.H3[[i]] <-
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
  fit.plot.H3[[1]],
  fit.plot.H3[[2]],
  fit.plot.H3[[3]],
  fit.plot.H3[[4]],
  fit.plot.H3[[5]],
  fit.plot.H3[[6]],
  fit.plot.H3[[7]],
  fit.plot.H3[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)
# Values for Tables 6.1 and 6.2
## DIC, WAIC and PsBF
model.sel.model.H3 <-
  model.selection.criteria(summary.model.tnc.H3, n.train = n.train)
## MAE
data.mae <- obs.fit.tnc.model.H3 %>%
  group_split(zoneid) %>%
  lapply(function(x)
    tail(x, n.hold))
mae.tnc.model.H3 <- data.mae %>%
  sapply(function(x)
    mae(x$tnc, x$fit))
zone.mae.tnc.model.H3 <-
  str_remove_all(mae.tnc.model.H3, "MAE is ") %>%
  as.numeric
mean.all.zone.mae.tnc.model.H3 <- mean(zone.mae.tnc.model.H3)
## MAPE
mape.tnc.model.H3 <- data.mae %>%
  sapply(function(x)
    mape(x$tnc, x$fit))
zone.mape.tnc.model.H3 <-
  str_remove_all(mape.tnc.model.H3, "MAPE is ") %>%
  as.numeric
mean.all.zone.mape.tnc.model.H3 <- mean(zone.mape.tnc.model.H3)
