# Section 12.4.1: Example: Simulated bivariate VAR(1) series
# Data: Simulated
rm(list = ls())
# Required packages
library(matrixcalc)
library(Matrix)
library(MASS)
library(vars)
library(INLA)
# Simulation
set.seed(123456)
p <- 1
k <- 2
n <- 300
phi.mat <- matrix(c(0.7, 0.3, 0.1, 0.2), nrow = 2, byrow = TRUE)
# Construct W matrix:
init.val <- matrix(c(0, 0))
rho.w1w2 = 0
sigma2.w1 <- 1 / 2
sigma2.w2 <- 1 / 2
cov.w1w2 <- rho.w1w2 * sqrt(sigma2.w1 * sigma2.w2)
sigma.mat.w <-
  matrix(c(sigma2.w1, cov.w1w2, cov.w1w2, sigma2.w2),
         nrow = 2,
         byrow = T)
# Simulation of VAR(1)
sample.list <- list()
sample.list[[1]] <- init.val
for (i in 2:n) {
  sample.list[[i]] <-
    phi.mat %*% sample.list[[i - 1]] + matrix(MASS::mvrnorm(
      n = 1,
      mu = c(0, 0),
      Sigma = sigma.mat.w
    ))
}
simulated_var <- do.call(cbind, sample.list)
var.ts <- t(simulated_var)
colnames(var.ts) <- c("y1", "y2")
x.t <- vec(var.ts)
# Construct V matrix
rho.v1v2 = 0.55
sigma2.v1 <- 1 / 2
sigma2.v2 <- 1 / 3
cov.v1v2 <- rho.v1v2 * sqrt(sigma2.v1 * sigma2.v2)
sigma.mat.v <-
  matrix(c(sigma2.v1, cov.v1v2, cov.v1v2, sigma2.v2),
         nrow = 2,
         byrow = T)
v.t <- vec(mvrnorm(n, mu = c(0, 0), Sigma = sigma.mat.v))
y.t <- x.t + v.t
# Functions to use within rgeneric()
interpret.theta <- function(theta) {
  n.phi <- k * k * p
  n.prec <- k
  n.tot <- n.phi + n.prec
  phi.VAR <- sapply(theta[as.integer(1:n.phi)], function(x) {
    x
  })
  # W matrix  precisions
  wprec <-
    sapply(theta[as.integer((n.phi + 1):(n.phi + n.prec))], function(x) {
      exp(x)
    })
  param <- c(phi.VAR, wprec)
  W <- diag(1, n.prec)
  st.dev <- 1 / sqrt(wprec)
  st.dev.mat <-
    matrix(st.dev, ncol = 1) %*% matrix(st.dev, nrow = 1)
  W <- W * st.dev.mat
  PREC <- solve(W)
  return(list(
    param = param,
    VACOV = W,
    PREC = PREC,
    phi.vec = c(phi.VAR)
    ,
    n.phi = n.phi,
    n.prec = n.prec
  ))
}
inla.rgeneric.VAR_1 <-
  function(cmd = c("graph",
                   "Q",
                   "mu",
                   "initial",
                   "log.norm.const",
                   "log.prior",
                   "quit"),
           theta = NULL)
  {
    interpret.theta <- function() {
      n.phi <- k * k * p
      n.prec <- k
      n.tot <- n.phi + n.prec
      phi.VAR <-
        sapply(theta[as.integer(1:n.phi)], function(x) {
          x
        })
      # W matrix  precisions
      wprec <-
        sapply(theta[as.integer((n.phi + 1):(n.phi + n.prec))], function(x) {
          exp(x)
        })
      param <- c(phi.VAR, wprec)
      W <- diag(1, n.prec)
      st.dev <- 1 / sqrt(wprec)
      st.dev.mat <-
        matrix(st.dev, ncol = 1) %*% matrix(st.dev, nrow = 1)
      W <- W * st.dev.mat
      PREC <- solve(W)
      return(list(
        param = param,
        VACOV = W,
        PREC = PREC,
        phi.vec = c(phi.VAR)
        ,
        n.phi = n.phi,
        n.prec = n.prec
      ))
    }
    #Precision matrix
    Q <- function() {
      param <- interpret.theta()
      phi.mat <- matrix(param$phi.vec, nrow = k)
      sigma.w.inv <- param$PREC
      A <- t(phi.mat) %*% sigma.w.inv %*% phi.mat
      B <- -t(phi.mat) %*% sigma.w.inv
      C <- sigma.w.inv
      # Construct mid-block:
      zero.mat <- matrix(0, nrow = 2, ncol = 2 * n)
      # Define the matrix block:
      mat <- cbind(t(B), A + C, B)
      # Initializing column id and matrix list:
      col.id <- list()
      mat.list <- list()
      col.id[[1]] <- 1:(3 * k)
      mat.list[[1]] <- zero.mat
      mat.list[[1]][, col.id[[1]]] <- mat
      for (id in 2:(n - 2)) {
        start.id <- col.id[[id - 1]][1] + k
        end.d <-  start.id + (3 * k - 1)
        col.id[[id]] <- start.id:end.d
        mat.list[[id]] <- zero.mat
        mat.list[[id]][, col.id[[id]]] <- mat
      }
      mid.block <- do.call(rbind, mat.list)
      tau.val <- 0.1
      diffuse.prec <- tau.val * diag(1, k)
      # Construct first and last row blocks and then join with mid block:
      first.row.block <-
        cbind(A + diffuse.prec, B, matrix(0, nrow = k, ncol = (k * n - k ^ 2)))
      last.row.block <- cbind(matrix(0, nrow = k, ncol = k * n - k ^ 2), t(B), C)
      toep.block.mat <-
        rbind(first.row.block, mid.block, last.row.block)
      # Changing to a sparse Matrix:
      prec.mat <- Matrix(toep.block.mat, sparse = TRUE)
      return(prec.mat)
    }
    # Graph function: Essentially Q matrix
    graph = function() {
      return (inla.as.sparse(Q()))
    }
    #Mean of model
    mu <- function() {
      return(numeric(0))
    }
    # Log normal constant:
    log.norm.const <- function() {
      Q <- Q()
      log.det.val <-
        Matrix::determinant(Q, logarithm = TRUE)$modulus
      val <- (-k * n / 2) * log(2 * pi) + 0.5 * log.det.val
      return (val)
    }
    log.prior <- function() {
      param <- interpret.theta()
      pars <- param$param
      k <- k
      total.par <- param$n.phi + param$n.prec
      # Normal prior for phi's:
      theta.phi <- theta[1:param$n.phi]
      phi.prior <-
        sum(sapply(theta.phi, function(x)
          dnorm(
            x,
            mean = 0,
            sd = 1,
            log = TRUE
          )))
      theta.prec <-
        theta[(param$n.phi + 1):(param$n.phi + param$n.prec)]
      prec.prior <-
        sum(sapply(theta.prec, function(x)
          dgamma(
            x,
            shape = 1,
            scale = 1,
            log = TRUE
          )))
      prec.jacob <- sum(theta.prec) # This is for precision terms
      prior.val <- phi.prior + prec.prior + prec.jacob
      return (prior.val)
    }
    initial = function() {
      phi.init <- c(0.1, 0.1, 0.1, 0.1)
      prec.init <- rep(1, k)
      init <- c(phi.init, prec.init)
      return (init)
    }
    if (as.integer(R.version$major) > 3) {
      if (!length(theta))
        theta <- initial()
    } else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return (val)
  }

inla.dat <- data.frame(y = y.t, iid2d.id = 1:length(y.t))
odd.id <- seq(1, nrow(inla.dat), by = 2)
even.id <- seq(2, nrow(inla.dat), by = 2)
var.id <- c(odd.id, even.id)
inla.dat$var.id <- var.id
# Model formula and result
model.define <-
  inla.rgeneric.define(inla.rgeneric.VAR_1,
                       p = p,
                       k = k,
                       n = n)
# Model:
formula.rgeneric <-  y ~ -1 +
  f(var.id, model = model.define) + f(iid2d.id, model = "iid2d", n = 2 *
                                        n)
result.rgeneric <- inla(
  formula.rgeneric,
  family = c("gaussian"),
  data = inla.dat,
  control.compute = list(dic = TRUE, config = TRUE),
  control.predictor = list(compute = TRUE),
  control.mode = list(restart = TRUE),
  control.family = list(hyper = list(prec = list(
    initial = 15
    , fixed =
      T
  )))
  # verbose = TRUE
)
summary(result.rgeneric)
# Comparison Table: (Actual vs INLA estimated)
var.parm <-
  interpret.theta(result.rgeneric$summary.hyperpar$mean[1:6])$param
iid2d.param <- result.rgeneric$summary.hyperpar$mean[7:9]
inla_estimated <- c(var.parm, iid2d.param)
actual <-
  c(vec(phi.mat),
    1 / sigma2.w1,
    1 / sigma2.w2,
    1 / sigma2.v1,
    1 / sigma2.v2,
    rho.v1v2)
tab <- data.frame(actual, inla_estimated)
rownames(tab) <- c(
  "phi11",
  "phi21",
  "phi12",
  "phi22",
  "prec.w1"
  ,
  "prec.w2",
  "prec.v1",
  "prec.v2",
  "rho.v1v2"
)
tab
