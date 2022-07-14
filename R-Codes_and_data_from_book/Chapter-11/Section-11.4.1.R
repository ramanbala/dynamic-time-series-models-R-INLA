# Section 11.4.1: Model 1. Knorr-Held additive effects model
# Data: monthly TNC usage
# Inputs: "tnc_monthly_data_for_spatiotemporal.csv"
# Source codes from Section 11.4
source("Chapter-11/Section-11.4.R")
# Model Formula and fit
formula.add <-
  tnc.month ~ 1 + f(id.zone, model = "bym", graph = nyc.adj) +
  f(id.month, model = "rw1") +
  f(id.monthu, model = "iid")
tnc.add <- inla(
  formula.add,
  data = data.kh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  family = "poisson"
)
summary(tnc.add)
