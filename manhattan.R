# Identify discrepencies and repetitions across on-market and universe datasets

setwd("/Users/whlu/Documents/Terra Nova/UChicago/Q-line")
library("rgdal")
library("rgeos")
library("sp")
library("tmap")
library(dplyr)

marketpts <- read.csv("QECA_on_market.csv")
marketpts <- marketpts[,c(1,11,10,5)]
head(marketpts)
market.points <- SpatialPointsDataFrame(marketpts[,2:3], marketpts, proj4string = CRS("+init=EPSG:2263"))
plot(market.points)
market_buffers <- gBuffer(market.points, width = 20, byid = TRUE)

universepts <- read.csv("QECA_universe_1_Edited.csv")
universepts <- universepts[,c(1,37,36,15)]
uni.points <- SpatialPointsDataFrame(universepts[,2:3], universepts, proj4string = CRS("+init=EPSG:2263"))

pip <- over(uni.points, market_buffers)
uni.points@data <- cbind(uni.points@data, pip)
pts_data <- uni.points@data
pts_data$diff <- abs(pts_data$FULLVAL - pts_data$Price)
pts_data %>% 
  group_by(UniqueIndex) %>% 
  slice(which.min(diff))


