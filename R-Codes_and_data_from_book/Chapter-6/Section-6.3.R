# Section 6.3: Example: Ridesourcing in NYC
# Data: Weekly ridesourcing usage data
# Inputs: tnc_weekly_data.csv
rm(list = ls())
# Required packages
library(INLA)
library(tidyverse)
library(gridExtra)
# Source custom functions
source("functions_custom.R")

# Load data
ride <-
  read.csv("Chapter-6/tnc_weekly_data.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
n <- n_distinct(ride$date)
g <- n_distinct(ride$zoneid)
k <- 10000
ride$tnc <- ride$tnc / k
ride$taxi <- ride$taxi / k
ride$subway <- ride$subway / k
# Train and test data
n.hold <- 5
n.train <- n - n.hold
df.zone <- ride %>%
  group_split(zoneid)
train.df <- df.zone %>%
  lapply(function(x)
    x[1:n.train, ]) %>%
  bind_rows()
test.df <- df.zone %>%
  lapply(function(x)
    tail(x, n.hold)) %>%
  bind_rows()

# Code for Figure 6.1
zonename <- sort(unique(ride$zone.name))
req.name <- zonename[c(4, 104, 11, 13, 30, 69, 2, 32)]
m1 <- match(req.name, ride$zone.name)
req.zoneid <- ride$zoneid[m1]
req.borough <- ride$borough[m1]
eda.plot <- list()
for (i in 1:length(req.zoneid)) {
  plot.data <- ride %>%
    filter(zoneid == req.zoneid[i]) %>%
    mutate(time = 1:n) %>%
    select(time, tnc, taxi)
  eda.plot[[i]] <-
    multiline.plot(
      plot.data,
      title = paste(req.name[i], req.borough[i], sep = ", "),
      xlab = "week",
      ylab = "",
      line.size = 0.8,
      line.color = c("red", "black")
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 7)
    )
  legend <-
    get_legend(
      multiline.plot(
        plot.data,
        title = req.name[8],
        xlab = "week",
        ylab = "",
        line.size = 0.8,
        line.color = c("red", "black")
      )
    )
}
grid.arrange(
  eda.plot[[1]],
  eda.plot[[2]],
  eda.plot[[3]],
  eda.plot[[4]],
  eda.plot[[5]],
  eda.plot[[6]],
  eda.plot[[7]],
  eda.plot[[8]],
  legend,
  layout_matrix = rbind(c(1, 2, NA), c(3, 4, NA), c(5, 6, NA), c(7, 8, 9)),
  widths = c(5, 5, 1)
)
