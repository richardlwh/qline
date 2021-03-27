
# Process noise complaints dataset for ArcGIS analysis

library(rgeos)
library(sp)
library("tmap")
library(dplyr)


noise <- read.csv("311_Service_Requests_for_2009.csv")
noise_mh <- noise[,c(1,2,6,7,8,10,24,51,50)]
noise_mh <- filter(noise_mh, Borough == "MANHATTAN")
noise_fil <- noise_mh[grep("Noise",noise_mh[,c(3)]),]
noise_fil <- na.omit(noise_fil, cols=c("Latitude", "Longitude"))

write.csv(noise_fil, "/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/on_market_noise_2009.csv")
