# Section 11.4: Example: Monthly TNC usage in NYC taxi zones
# Data: monthly TNC usage
# Inputs: "tnc_monthly_data_for_spatiotemporal.csv"
rm(list = ls())
# Required packages
library(lubridate)
library(gridExtra)
library(raster)
library(tmap)
library(sf)
library(spdep)
library(mapview)
library(gtools)
library(INLA)
library(tidyverse)
# Source custom functions
source("functions_custom.R")
# Read data
ride.3yr <-
  read.csv(
    "Chapter-11/tnc_monthly_data_for_spatiotemporal.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
ride <- ride.3yr[mixedorder(ride.3yr$zoneid), ] %>%
  arrange(year, month)
ride$month.year <- paste(ride$year, ride$month, sep = "-")
n <- n_distinct(ride$month.year)
g <- n_distinct(ride$zoneid)
# Read shape files
zoneshape <- st_read("Chapter-11/taxi_zones.shp", quiet = TRUE)
zoneshape$zoneid <- paste("ID", zoneshape$LocationID, sep = "")
match.zone <- match(unique(ride$zoneid), zoneshape$zoneid)
zoneshape <- zoneshape[match.zone,] %>%
  arrange(OBJECTID)
# Adjacency matrix
nb.q <- poly2nb(zoneshape, queen = TRUE, row.names = zoneshape$zoneid)
nb2INLA("map.adj", nb.q)
nyc.adj <- inla.read.graph(filename = "map.adj")
# Indexes
zoneid <- factor(ride$zoneid, levels = unique(zoneshape$zoneid))
id.zone <- id.zone.int <- as.numeric(zoneid)
id.month <- id.monthu <- id.month.int <- rep(1:n, each = g)
id.zoneu.monthu <- 1:nrow(ride)
# Check for identical order of zones
identical(unique(ride$zoneid), zoneshape$zoneid)
# Data frame
data.kh <- cbind.data.frame(ride,
                            id.zone,
                            id.zone.int,
                            id.month,
                            id.monthu,
                            id.month.int,
                            id.zoneu.monthu)