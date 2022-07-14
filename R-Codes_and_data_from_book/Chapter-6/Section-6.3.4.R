# Section 6.3.4: Model H4. Fixed Intercept, Taxi usage varies over time and zones
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
id.b0 <- id.subway <- id.taxi <- rep(1:n, g)
id.holiday <- id.precip <- rep(1:n, g)
# Replicates
re.b0 <- re.subway <- re.taxi <- rep(1:g, each = n)
re.holiday <- re.precip <- rep(1, (n * g))
re.age <-
  re.earn <- re.pop <- re.emp <- rep(1:g, each = n)
# Merge indices with standardized data
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
# Model H4: Formula
formula.tnc.H4 <-
  tnc ~ subway + holidays + precip + median.age +
  median.earnings + scaled.population + scaled.employment +
  f(id.taxi,
    taxi,
    model = "ar1",
    constr = FALSE,
    replicate = re.taxi)
# Model run
model.tnc.H4 <- inla(
  formula.tnc.H4,
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
model.tnc.H4$summary.fixed
model.tnc.H4$summary.hyperpar
# code for Figure 6.6
fit.model.H4 <- model.tnc.H4$summary.fitted.values
fit.model.H4$zoneid <- ride$zoneid
obs.fit.tnc.model.H4 <-
  cbind.data.frame(ride[, c(1, 4, 5)], fit = fit.model.H4[, c(1)])
fit.plot.H4 <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- obs.fit.tnc.model.H4 %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, obs = tnc, fit = fit)
  fit.plot.H4[[i]] <-
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
  fit.plot.H4[[1]],
  fit.plot.H4[[2]],
  fit.plot.H4[[3]],
  fit.plot.H4[[4]],
  fit.plot.H4[[5]],
  fit.plot.H4[[6]],
  fit.plot.H4[[7]],
  fit.plot.H4[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)
# Values for Tables 6.1 and 6.2
## obtain DIC, WAIC and PsBF
model.sel.model.H4 <- model.selection.criteria(summary.model.tnc.H4, n.train = n.train)
## MAE
data.mae <- obs.fit.tnc.model.H4 %>% 
  group_split(zoneid) %>% 
  lapply(function(x) tail(x, n.hold))
mae.tnc.model.H4 <- data.mae %>% 
  sapply(function(x) mae(x$tnc, x$fit))
zone.mae.tnc.model.H4 <- str_remove_all(mae.tnc.model.H4,"MAE is ") %>% 
  as.numeric 
mean.all.zone.mae.tnc.model.H4 <- mean(zone.mae.tnc.model.H4)
## MAPE
mape.tnc.model.H4 <- data.mae %>% 
  sapply(function(x) mape(x$tnc, x$fit))
zone.mape.tnc.model.H4 <- str_remove_all(mape.tnc.model.H4,"MAPE is ") %>% 
  as.numeric 
mean.all.zone.mape.tnc.model.H4 <- mean(zone.mape.tnc.model.H4)
