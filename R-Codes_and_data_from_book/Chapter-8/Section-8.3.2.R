# Section 8.3.2: Example: Weekly shopping trips for multiple households
# Data: Number of weekly shopping trips
# Inputs: Weekly_Shopping_Trips.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")
# Read data
trips <-
  read.csv("Chapter-8/Weekly_Shopping_Trips.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
# Convert data from wide format to long format
g <- nrow(trips)
trips.long <- trips %>%
  pivot_longer(cols = W614:W717, names_to = "week") %>%
  rename(n.trips = value)
n <- n_distinct(trips.long$week)
n.hold <- 6
n.train <- n - n.hold
trips.house <- trips.long %>%
  group_split(panelist)
train.df <- trips.house %>%
  lapply(function(x)
    x[1:n.train, ]) %>%
  bind_rows()
test.df <- trips.house %>%
  lapply(function(x)
    tail(x, n.hold)) %>%
  bind_rows()
# Indexes and replicates
bin.size <- train.df %>%
  group_split(panelist) %>%
  sapply(function(x)
    max(x$n.trips))
trips.allh <- trips.house %>%
  lapply(function(x)
    mutate(x, n.trips = replace(
      n.trips, list = (n.train + 1):n, values = NA
    )))
id.b0 <- id.house <- rep(1:n, g)
re <- re.house <- rep(1:g, each = n)
trips.allh <-
  cbind.data.frame(bind_rows(trips.allh), id.b0, re, id.house, re.house)
prior.hbin = list(
  prec = list(prior = "loggamma", param = c(0.1, 0.1)),
  rho = list(prior = "normal", param = c(0, 0.15))
)
bin.size.all <- rep(bin.size, each = n)

# Model formula and fit
formula.allh <- n.trips ~ 1 +
  f(id.b0,
    model = "ar1",
    replicate = re,hyper = prior.hbin) +
  f(id.house, model = "iid", replicate = re.house)
model.allh <- inla(
  formula.allh,
  family = "binomial",
  Ntrials = bin.size.all,
  control.family = list(link="logit"),
  data = trips.allh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    cpo = TRUE,
    waic = TRUE,
    config = TRUE,
    cpo = TRUE)
)
summary(model.allh)
# Posterior prediction
set.seed(1234579)
post.samp <- inla.posterior.sample(n = 1000, model.allh)
model.allh$misc$configs$contents
fit.post.samp <- vector("list", 1000)
fit.house <-
  matrix(0,
         nrow = n,
         ncol = g,
         dimnames = list(NULL, paste("H", 1:g, sep = ".")))
for (j in 1:1000) {
  p <- post.samp[[j]]$latent[1:(n * g)] %>%
    matrix(nrow = n,
           ncol = g,
           byrow = F)
  for (i in 1:g) {
    fit.house[, i] <-
      sapply(p[, i], function(x)
        rbinom(1, bin.size[g], exp(x) / (1 + exp(x))))
  }
  fit.post.samp[[j]] <- fit.house
}
fit.post.samp.all <- fit.post.samp %>%
  lapply(as_tibble) %>%
  bind_rows()
fit.per.house <- vector("list", g)
length(fit.per.house)
str(fit.post.samp.all[, 1])
for (k in 1:g) {
  fit.per.house[[k]] <-
    matrix(c(fit.post.samp.all[[k]]), nrow = n, ncol = 1000)
}
# Code for Figure 8.3
mean.forecast <- fit.per.house %>% 
  lapply(function(x)rowMeans(x)) %>% 
  bind_cols()
six.house <- c(3, 166, 242, 501)
eda.plot <- list()
for (i in 1:length(six.house)) {
  house.data <- trips.house[[six.house[i]]]
  house.id <- house.data$panelist[1]
  house.forecast <- unname(unlist(mean.forecast[,six.house[i]]))
  plot.data <-
    cbind.data.frame(time = 1:n,
                     observed = house.data$n.trips,
                     forecast= house.forecast)
  eda.plot[[i]] <-
    multiline.plot(
      plot.data,
      title = paste("Household id:", house.id, sep = " "),
      xlab = "week",
      ylab = "",
      line.size = 0.6,
      line.color = c("red", "blue")
    ) +
    geom_vline(xintercept = (n.train), size = 0.2)+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 7)
    )
  legend <-
    get_legend(
      multiline.plot(
        plot.data,
        title = "",
        xlab = "week",
        ylab = "",
        line.size = 0.6,
        line.color = c("red", "blue")
      )
    )
}
grid.arrange(
  eda.plot[[1]],
  eda.plot[[2]],
  eda.plot[[3]],
  eda.plot[[4]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, 5)),
  widths = c(5, 5, 2)
)
