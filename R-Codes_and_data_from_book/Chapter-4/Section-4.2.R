# Section 4.2: An engineering example-Musa data
# Data: Musa data
# Inputs: MusaSystem40Data.xlsx

rm(list = ls())
# R packages
library(tidyverse)
library(readxl)
library(INLA)
library(doParallel)
library(foreach)
# Source custom functions
source("functions_custom.R")
# Load data
musa <- read_xlsx("Chapter-4/MusaSystem40Data.xlsx")
head(musa)
musa <- musa %>%
  mutate(log.tbf = log(`Time Beween Failures`))
# Code for Figure 4.1
ts.plot(ts(musa$log.tbf), ylab = "log inter-failure time", xlab = "t")
# Append training data with NA
n <- nrow(musa)
n.hold <- 6
n.train <- n - n.hold
test.data <- tail(musa, n.hold)
train.data <- musa[1:n.train,]
y <- train.data$log.tbf
y.append <- c(y, rep(NA, n.hold))