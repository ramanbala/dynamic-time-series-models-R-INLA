# Section 6.4: Model comparisons
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
# Source codes from Section 6.3.1 to 6.3.4
source("Chapter-6/Section-6.3.1.R")
source("Chapter-6/Section-6.3.2.R")
source("Chapter-6/Section-6.3.3.R")
source("Chapter-6/Section-6.3.4.R")
# Code for Figure 6.7
p.id <- req.zoneid[3]
dat.1 <- obs.fit.tnc.model.H1 %>%
  filter(zoneid == p.id) %>%
  mutate(time = 1:n) %>%
  select(time, obs = tnc, fit = fit)
# Compute In-sample MAPE
mape.insample.H1 <-
  mape(dat.1$obs[1:n.train], dat.1$fit[1:n.train]) %>%
  str_remove("MAPE is ") %>%
  as.numeric

fit.h1 <-
  multiline.plot(
    dat.1[1:n.train,],
    title = paste("Model H1: In-sample MAPE is", mape.insample.H1),
    xlab = "week",
    ylab = "tnc",
    line.type = "dotdash",
    line.size = 0.8
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )
mape.insample.H1 <- mape(dat.1$obs[1:n.train], dat.1$fit[1:n.train])
dat.2 <- obs.fit.tnc.model.H2 %>%
  filter(zoneid == p.id) %>%
  mutate(time = 1:n) %>%
  select(time, obs = tnc, fit = fit)
# Compute In-sample MAPE
mape.insample.H2 <-
  mape(dat.2$obs[1:n.train], dat.2$fit[1:n.train]) %>%
  str_remove("MAPE is ") %>%
  as.numeric
fit.h2 <-
  multiline.plot(
    dat.2[1:n.train,],
    title = paste("Model H2: In-sample MAPE is", mape.insample.H2),
    xlab = "week",
    ylab = "tnc",
    line.type = "dotdash",
    line.size = 0.8
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )
## model H2b
dat.2b <- obs.fit.tnc.model.H2b %>%
  filter(zoneid == p.id) %>%
  mutate(time = 1:n) %>%
  select(time, obs = tnc, fit = fit)
# Compute In-sample MAPE
mape.insample.H2b <-
  mape(dat.2b$obs[1:n.train], dat.2b$fit[1:n.train]) %>%
  str_remove("MAPE is ") %>%
  as.numeric
fit.h2b <-
  multiline.plot(
    dat.2b[1:n.train,],
    title = paste("Model H2b: In-sample MAPE is", mape.insample.H2b),
    xlab = "week",
    ylab = "tnc",
    line.type = "dotdash",
    line.size = 0.8
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )
dat.3 <- obs.fit.tnc.model.H3 %>%
  filter(zoneid == p.id) %>%
  mutate(time = 1:n) %>%
  select(time, obs = tnc, fit = fit)
# Compute In-sample MAPE
mape.insample.H3 <-
  mape(dat.3$obs[1:n.train], dat.3$fit[1:n.train]) %>%
  str_remove("MAPE is ") %>%
  as.numeric
fit.h3 <-
  multiline.plot(
    dat.3[1:n.train,],
    title = paste("Model H3: In-sample MAPE is", mape.insample.H3),
    xlab = "week",
    ylab = "tnc",
    line.type = "dotdash",
    line.size = 0.8
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )

dat.4 <- obs.fit.tnc.model.H4 %>%
  filter(zoneid == p.id) %>%
  mutate(time = 1:n) %>%
  select(time, obs = tnc, fit = fit)
# Compute In-sample MAPE
mape.insample.H4 <-
  mape(dat.4$obs[1:n.train], dat.4$fit[1:n.train]) %>%
  str_remove("MAPE is ") %>%
  as.numeric
fit.h4 <-
  multiline.plot(
    dat.4[1:n.train,],
    title = paste("Model H4: In-sample MAPE is", mape.insample.H4),
    xlab = "week",
    ylab = "tnc",
    line.type = "dotdash",
    line.size = 0.8
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 7),
    axis.title.x = element_text(size = 7)
  )
legend <-
  get_legend(multiline.plot(
    plot.data,
    title = req.name[8],
    xlab = "week",
    ylab = "tnc",
    line.size = 0.8
  ))

grid.arrange(fit.h1,
             fit.h2,
             fit.h2b,
             fit.h3,
             fit.h4,
             legend,
             layout_matrix = rbind(c(1, 2, 3), c(4, 5, 6)))
