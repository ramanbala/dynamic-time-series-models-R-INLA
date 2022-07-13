# Section 3.11 - Posterior predictive samples of unknown observations
source("Chapter-3/Section-3.10.R")
fun.pred <- function(idx.pred) {
  m <- length(idx.pred)
  return (Predictor[idx.pred] + rnorm(m, mean = 0,
                                      sd = sqrt(1 / theta[1])))
}
pred.post <-
  inla.posterior.sample.eval(fun.pred, ar1.level.latent_hyper.samples,
                             idx.pred = which(is.na(y.append)))
# Code for Figure 3.18
y.append.500 <- c(rep(NA, length(y)), pred.post[, 500])
y.append.1000 <- c(rep(NA, length(y)), pred.post[, 1000])
y.append.1500 <- c(rep(NA, length(y)), pred.post[, 1500])
y.append.2000 <- c(rep(NA, length(y)), pred.post[, 2000])
p.500 <-
  multiline.plot(
    cbind.data.frame(
      time = 1:length(y.ar1.level),
      obs = y.ar1.level,
      fit = y.append.500
    ),
    line.color = c("red", "blue"),
    title = expression(500^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = "y.ar1.level"
  ) +
  geom_vline(xintercept = 495,size = 0.2) +
  theme(legend.position = "none", plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p.1000 <-
  multiline.plot(
    cbind.data.frame(
      time = 1:length(y.ar1.level),
      obs = y.ar1.level,
      fit = y.append.1000
    ),
    line.color = c("red", "blue"),
    title = expression(1000^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = "y.ar1.level"
  ) +
  geom_vline(xintercept = 495, size = 0.2) +
  theme(legend.position = "none", plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p.1500 <-
  multiline.plot(
    cbind.data.frame(
      time = 1:length(y.ar1.level),
      obs = y.ar1.level,
      fit = y.append.1500
    ),
    line.color = c("red", "blue"),
    title = expression(1500^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = "y.ar1.level"
  ) +
  geom_vline(xintercept = 495, size = 0.2) +
  theme(legend.position = "none", plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10))
p.2000 <-
  multiline.plot(
    cbind.data.frame(
      time = 1:length(y.ar1.level),
      obs = y.ar1.level,
      fit = y.append.2000
    ),
    line.color = c("red", "blue"),
    title = expression(2000^th~sample),
    line.size = 0.5,
    xlab = "t",
    ylab = "y.ar1.level"
  ) +
  geom_vline(xintercept = 495, size = 0.2) +
  theme(legend.position = "none", plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10))
grid.arrange(p.500, p.1000, p.1500, p.2000)
# Code for Figure 3.19
par(mfrow = c(1, 2))
hist(pred.post[1, ],
     xlab = expression(x[496]),
     ylab = "frequency",
     main = "")
hist(pred.post[5, ],
     xlab = expression(x[500]),
     ylab = "frequency",
     main = "")