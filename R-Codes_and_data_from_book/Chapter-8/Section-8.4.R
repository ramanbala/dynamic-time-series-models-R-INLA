# Section 8.4: Multinomial time series
# Data: Simulated
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
library(kableExtra)
# Source custom functions
source("functions_custom.R")
# Model D1: Dynamic intercept in a single category
beta <- -0.3 # coefficient for C[j,t]
deltas <- c(1, 4, 3) # category specific coefficients for U[j,t]
param <- c(beta, deltas)
n <- 1000
set.seed(123457)
# category specific predictor with constant coefficient beta
C.1 <- rnorm(n, mean = 30, sd = 2.5)
C.2 <- rnorm(n, mean = 35, sd = 3.5)
C.3 <- rnorm(n, mean = 40, sd = 1)
# category specific predictor with category-specific coefficient delta_j
U.1 <- rnorm(n, mean = 2, sd = 0.5)
U.2 <- rnorm(n, mean = 2, sd = 0.5)
U.3 <- rnorm(n, mean = 2, sd = 0.5)
Multinom.sample.rand <- function(N, random.int) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  for (t in 1:n) {
    eta.1 <- beta * C.1[t] + deltas[1] * U.1[t] +
      random.int[t]
    eta.2 <- beta * C.2[t] + deltas[2] * U.2[t]
    eta.3 <- beta * C.3[t] + deltas[3] * U.3[t]
    probs <- c(eta.1, eta.2, eta.3)
    probs <- exp(probs) / sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[t, ] <- as.vector(samp)
  }
  colnames(Y) <- c("Y.1", "Y.2", "Y.3")
  return(Y)
}
# Generate beta_{1,t,0}
rw1.int <- rep(NA, n)
rw1.int[1] <- 0
for (t in 2:n) {
  rw1.int[t] <- rnorm(1, mean = rw1.int[t - 1], sd = 0.1)
}
# Generate series
N <- 100
Y.rw1 <- Multinom.sample.rand(N, rw1.int)
# Code for Figure 8.4
ts1 <-
  tsline(
    Y.rw1[, 1],
    xlab = "t",
    ylab = "Y.1",
    line.size = 0.5,
    line.color = "red"
  )  + ylim(0, 100)
ts2 <-
  tsline(
    Y.rw1[, 2],
    xlab = "t",
    ylab = "Y.2",
    line.size = 0.5,
    line.color = "red"
  )  + ylim(0, 100)
ts3 <-
  tsline(
    Y.rw1[, 3],
    xlab = "t",
    ylab = "Y.3",
    line.size = 0.5,
    line.color = "red"
  )  + ylim(0, 100)
grid.arrange(ts1, ts2, ts3)
# Indexes
Y <- c(Y.rw1[, 1], Y.rw1[, 2], Y.rw1[, 3])
C <- c(C.1, C.2, C.3)
U.1 <- c(U.1, rep(NA, 2 * n))
U.2 <- c(rep(NA, n), U.2, rep(NA, n))
U.3 <- c(rep(NA, 2 * n), U.3)
rw1.idx <- c(1:n, rep(NA, 2 * n))
phi <- rep(1:n, 3)
inla.dat.D1 <- cbind.data.frame(Y, C, U.1, U.2, U.3, rw1.idx, phi)
# Model formula and fit
formula.D1 <- Y ~ -1 + C + U.1 + U.2 + U.3 +
  f(phi, model = "iid",
    hyper = list(prec = list(initial = -10, fixed = TRUE))) +
  f(rw1.idx, model = "rw1", constr = FALSE)
model.D1 <- inla(
  formula.D1,
  family = "poisson",
  data = inla.dat.D1,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)
summary(model.D1)
# Code for Figure 8.5
multiline.plot(
  plot.data = cbind.data.frame(
    time = 1:n,
    obs = rw1.int,
    fit = model.D1$summary.random$rw1.idx[, 2]
  ),
  ylab = "rw1.int",
  xlab = "t",
  line.size = 0.6,
  line.color = c("red", "blue")
) +
  theme(legend.position = "none")
# Code for Figure 8.6
of1 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 1],
    fit = model.D1$summary.fitted.values[1:n, 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.1",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
of2 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 2],
    fit = model.D1$summary.fitted.values[(n + 1):(2 * n), 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.2",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
of3 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 3],
    fit = model.D1$summary.fitted.values[(2 * n + 1):(3 * n), 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.3",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
grid.arrange(of1, of2, of3)
# MAE
mae.D1.Y1 <-
  mae(Y.rw1[, 1], model.D1$summary.fitted.values[1:n, 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D1.Y2 <-
  mae(Y.rw1[, 2], model.D1$summary.fitted.values[(n + 1):(2 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D1.Y3 <-
  mae(Y.rw1[, 3], model.D1$summary.fitted.values[(2 * n + 1):(3 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
# Model D2: dynamic intercept in category 2
beta <-  -0.3  # coefficient for C[j,t]
deltas <-  c(1, 4, 3) # category specific coefficients for U[j,t]
param <-  c(beta, deltas)
n <-  1000
set.seed(123457)
# category specific predictor with constant coefficient beta
C.1 <-  rnorm(n, mean = 30, sd = 2.5)
C.2 <- rnorm(n, mean = 35, sd = 3.5)
C.3 <- rnorm(n, mean = 40, sd = 1)
# category specific predictor with category-specific coefficient delta_j
U.1 <- rnorm(n, mean = 2, sd = 0.5)
U.2 <- rnorm(n, mean = 2, sd = 0.5)
U.3 <- rnorm(n, mean = 2, sd = 0.5)
# Source this function
Multinom.sample.rand <- function(N, random.int) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  for (t in 1:n) {
    eta.1 <- beta * C.1[t] + deltas[1] * U.1[t]
    eta.2 <- beta * C.2[t] + deltas[2] * U.2[t] +
      random.int[t]
    eta.3 <- beta * C.3[t] + deltas[3] * U.3[t]
    probs <- c(eta.1, eta.2, eta.3)
    probs <- exp(probs) / sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[t, ] <- as.vector(samp)
  }
  colnames(Y) <- c("Y.1", "Y.2", "Y.3")
  return(Y)
}
# Random walk
rw1.int <- rep(NA, n)
rw1.int[1] <- 0
for (t in 2:n) {
  rw1.int[t] <- rnorm(1, mean = rw1.int[t - 1], sd = 0.1)
}

N <- 100
Y.rw1 <- Multinom.sample.rand(N, rw1.int)

Y <- c(Y.rw1[, 1], Y.rw1[, 2], Y.rw1[, 3])
C <- c(C.1, C.2, C.3)
U.1 <- c(U.1, rep(NA, 2 * n))
U.2 <- c(rep(NA, n), U.2, rep(NA, n))
U.3 <- c(rep(NA, 2 * n), U.3)
# rw1 index for category 2
rw1.idx <- c(rep(NA, n), 1:n, rep(NA, n))
phi <- rep(1:n, 3)
inla.dat.D2 <- cbind.data.frame(Y, C, U.1, U.2, U.3, rw1.idx, phi)
# Formula
formula.D2 <- Y ~ -1 + C + U.1 + U.2 + U.3 +
  f(phi, model = "iid", hyper = list(prec = list(initial = -10, fixed = TRUE))) +
  f(rw1.idx, model = "rw1", constr = FALSE)

model.D2 <- inla(
  formula.D2,
  family = "poisson",
  data = inla.dat.D2,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)
summary(model.D2)
# MAE
mae.D2.Y1 <-
  mae(Y.rw1[, 1], model.D2$summary.fitted.values[1:n, 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D2.Y2 <-
  mae(Y.rw1[, 2], model.D2$summary.fitted.values[(n + 1):(2 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D2.Y3 <-
  mae(Y.rw1[, 3], model.D2$summary.fitted.values[(2 * n + 1):(3 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
# Model D3: dynamic intercept in category 3
beta <-  -0.3  # coefficient for C[j,t]
deltas <-  c(1, 4, 3) # category specific coefficients for U[j,t]
param <-  c(beta, deltas)
n <-  1000
set.seed(123457)
# category specific predictor with constant coefficient beta
C.1 <-  rnorm(n, mean = 30, sd = 2.5)
C.2 <- rnorm(n, mean = 35, sd = 3.5)
C.3 <- rnorm(n, mean = 40, sd = 1)
# category specific predictor with category-specific coefficient delta_j
U.1 <- rnorm(n, mean = 2, sd = 0.5)
U.2 <- rnorm(n, mean = 2, sd = 0.5)
U.3 <- rnorm(n, mean = 2, sd = 0.5)
# Source this function
Multinom.sample.rand <- function(N, random.int) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  for (t in 1:n) {
    eta.1 <- beta * C.1[t] + deltas[1] * U.1[t]
    eta.2 <- beta * C.2[t] + deltas[2] * U.2[t]
    eta.3 <- beta * C.3[t] + deltas[3] * U.3[t] +
      random.int[t]
    probs <- c(eta.1, eta.2, eta.3)
    probs <- exp(probs) / sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[t, ] <- as.vector(samp)
  }
  colnames(Y) <- c("Y.1", "Y.2", "Y.3")
  return(Y)
}
# Random walk
rw1.int <- rep(NA, n)
rw1.int[1] <- 0
for (t in 2:n) {
  rw1.int[t] <- rnorm(1, mean = rw1.int[t - 1], sd = 0.1)
}

N <- 100
Y.rw1 <- Multinom.sample.rand(N, rw1.int)

Y <- c(Y.rw1[, 1], Y.rw1[, 2], Y.rw1[, 3])
C <- c(C.1, C.2, C.3)
U.1 <- c(U.1, rep(NA, 2 * n))
U.2 <- c(rep(NA, n), U.2, rep(NA, n))
U.3 <- c(rep(NA, 2 * n), U.3)
# rw1 index for category 3
rw1.idx <- c(rep(NA, 2 * n), 1:n)
phi <- rep(1:n, 3)
inla.dat.D3 <- cbind.data.frame(Y, C, U.1, U.2, U.3, rw1.idx, phi)
# Formula
formula.D3 <- Y ~ -1 + C + U.1 + U.2 + U.3 +
  f(phi, model = "iid", hyper = list(prec = list(initial = -10, fixed = TRUE))) +
  f(rw1.idx, model = "rw1", constr = FALSE)

model.D3 <- inla(
  formula.D3,
  family = "poisson",
  data = inla.dat.D3,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)
summary(model.D3)
# MAE
mae.D3.Y1 <-
  mae(Y.rw1[, 1], model.D3$summary.fitted.values[1:n, 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D3.Y2 <-
  mae(Y.rw1[, 2], model.D3$summary.fitted.values[(n + 1):(2 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D3.Y3 <-
  mae(Y.rw1[, 3], model.D3$summary.fitted.values[(2 * n + 1):(3 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
#Model D4: Dynamic coefficient of C_{j,t}
deltas <-  c(1, 4, 3) # category specific coefficients for U[j,t]
param <-  c(deltas)
n <-  1000 # length of categorical time series
set.seed(123457)
# category specific predictor with dynamic coefficient beta
C.1 <-  rnorm(n, mean = 30, sd = 2.5)
C.2 <- rnorm(n, mean = 35, sd = 3.5)
C.3 <- rnorm(n, mean = 40, sd = 1)
U.1 <- rnorm(n, mean = 2, sd = 0.5)
U.2 <- rnorm(n, mean = 2, sd = 0.5)
U.3 <- rnorm(n, mean = 2, sd = 0.5)
Multinom.sample.rand <- function(N, rw1.beta) {
  Y <- matrix(NA, ncol = 3, nrow = n)
  eta.1 <- eta.2 <- eta.3 <- rep(NA, n)
  for (t in 1:n) {
    eta.1[t] <- rw1.beta[t] * C.1[t] + deltas[1] * U.1[t]
    eta.2[t] <- rw1.beta[t] * C.2[t] + deltas[2] * U.2[t]
    eta.3[t] <- rw1.beta[t] * C.3[t] + deltas[3] * U.3[t]
    probs <- c(eta.1[t], eta.2[t], eta.3[t])
    probs <- exp(probs) / sum(exp(probs))
    samp <- rmultinom(1, N, prob = probs)
    Y[t, ] <- as.vector(samp)
  }
  colnames(Y) <- c("Y.1", "Y.2", "Y.3")
  return(Y)
}
rw1.beta <- rep(NA, n)
rw1.beta[1] <- 0
for (t in 2:n) {
  rw1.beta[t] <- rnorm(1, mean = rw1.beta[t - 1], sd = 0.1)
}
N <- 100
Y.rw1 <- Multinom.sample.rand(N, rw1.beta)
df.rw1 <- cbind.data.frame(Y.rw1, C.1, C.2, C.3, U.1, U.2, U.3)
Y.new <- c(Y.rw1[, 1], Y.rw1[, 2], Y.rw1[, 3])
C.new <- c(df.rw1$C.1, df.rw1$C.2, df.rw1$C.3)
C.1.new <- c(C.1, rep(NA, 2 * n))
C.2.new <- c(rep(NA, n), C.2, rep(NA, n))
C.3.new <- c(rep(NA, 2 * n), C.3)
C.new <- c(C.1, C.2, C.3)
U.1.new <- c(U.1, rep(NA, 2 * n))
U.2.new <- c(rep(NA, n), U.2, rep(NA, n))
U.3.new <- c(rep(NA, 2 * n), U.3)
id.rw <- phi.new <- c(rep(1:n, 3))
rw1.idx <- c(1:n, rep(NA, 2 * n))
rw2.idx <- c(rep(NA, n), 1:n, rep(NA, n))
rw3.idx <- c(rep(NA, 2 * n), 1:n)
re.idx <- rep(1, (3 * n))
id.re <- rep(1:n, 3)
inla.dat.D4 <- data.frame(
  Y.new,
  C.new,
  C.1.new,
  C.2.new,
  C.3.new,
  U.1.new,
  U.2.new,
  U.3.new,
  phi.new,
  id.rw,
  re.idx,
  id.re,
  rw1.idx,
  rw2.idx,
  rw3.idx
)
# Model formula and fit
formula.D4 <- Y.new ~ -1  + U.1.new + U.2.new + U.3.new +
  f(phi.new, model = "iid",
    hyper = list(prec = list(initial = -10, fixed = TRUE))) +
  f(id.rw, C.new, model = "rw1", constr = FALSE) +
  f(
    id.re,
    model = "iid",
    replicate = re.idx,
    hyper = list(theta = list(initial = 20, fixed = T))
  )
model.D4 <- inla(
  formula.D4,
  family = "poisson",
  data = inla.dat.D4,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)
summary(model.D4)
# Code for Figure 8.7
multiline.plot(
  plot.data = cbind.data.frame(
    time = 1:n,
    obs = rw1.beta,
    fit = model.D4$summary.random$id.rw$`0.5quant`
  ),
  ylab = "rw1.beta",
  xlab = "t",
  line.size = 0.7,
  line.color = c("red", "blue")
) +
  theme(legend.position = "none")
# Code for Figure 8.8
of1 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 1],
    fit = model.D4$summary.fitted.values[1:n, 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.1",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
of2 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 2],
    fit = model.D4$summary.fitted.values[(n + 1):(2 * n), 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.2",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
of3 <-
  cbind.data.frame(
    time = 1:n,
    obs = Y.rw1[, 3],
    fit = model.D4$summary.fitted.values[(2 * n + 1):(3 * n), 1]
  ) %>%
  multiline.plot(
    xlab = "t",
    ylab = "Y.3",
    line.size = 0.5,
    line.color = c("red", "blue")
  ) +
  theme(legend.position = "none") +
  ylim(0, 100)
grid.arrange(of1, of2, of3)
# MAE
mae.D4.Y1 <-
  mae(Y.rw1[, 1], model.D4$summary.fitted.values[1:n, 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D4.Y2 <-
  mae(Y.rw1[, 2], model.D4$summary.fitted.values[(n + 1):(2 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
mae.D4.Y3 <-
  mae(Y.rw1[, 3], model.D4$summary.fitted.values[(2 * n + 1):(3 * n), 1]) %>%
  str_remove_all("MAE is ") %>% as.numeric()
# Code for Table 8.1
insample.mae <-
  cbind.data.frame(
    Model = c("D1", "D2", "D3", "D4"),
    MAE.Y1 = c(mae.D1.Y1, mae.D2.Y1, mae.D3.Y1, mae.D4.Y1),
    MAE.Y2 = c(mae.D1.Y2, mae.D2.Y2, mae.D3.Y2, mae.D4.Y2),
    MAE.Y3 = c(mae.D1.Y3, mae.D2.Y3, mae.D3.Y3, mae.D4.Y3)
  )
insample.mae %>%
  kable(
    caption = "In-sample MAE for model comparison.",
    col.names = c("Model", "MAE(Y.1)", "MAE(Y.2)", "MAE(Y.3)"),
    escape = FALSE,
    booktabs = TRUE,
    digits = 3
  ) %>%
  kable_styling(
    bootstrap_options = "striped",
    position = "center",
    fixed_thead = TRUE,
    full_width = FALSE
  ) 