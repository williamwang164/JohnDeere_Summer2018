# title: convertTiff2PngV1.0.R
# purpose: convert GeoTiff files to png files
# author: Robert A. Stevens
# date: July 21, 2017
# input:  TIFF files from Vista
# output: PNG files for others to view and use

# changes
# 1. Add title with mean and median values for yield and moisture

# load libraries
library(rgdal)
library(raster)
library(rasterVis)
library(ggplot2)
library(colorRamps)
library(colorspace)
#getwd()

# define location on IC server
#setwd("~/COPBR_Test/")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2016_Maps/Neubrandenburg/Yield_BARLEY_delivered_08July16/Yield_BARLEY")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2016_Maps/Sugar/Yield_Sugarcane_29037/FractionMasks_Sugarcane")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2016_Maps/Sugar/Yield_Sugarcane_29037/Yield_Sugarcane")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2016_Maps/Sugar/17Nov16/MOGmoisture_Wheat")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2016_Maps/Sugar/17Nov16/FractionBrown_Sugarcane_draft")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/Neubrandenburg")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/Neubrandenburg/Wheat")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/R2_Lead_Farms/Borchert/From_Vista/13Oct17/YIELD_Barley")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/R2_Lead_Farms/Borchert/From_Vista/13Oct17/YIELD_Canola")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/R2_Lead_Farms/Borchert/From_Vista/13Oct17/YIELD_Wheat")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/R2_Lead_Farms/Borchert/From_Vista/YIELD_delivery_REVISED_08Feb18")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/Neubrandenburg/YIELD_delivery_REVISED_09Feb18")
#setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/Neubrandenburg/07Mar18_YIELD_delivery_REVISED2")
setwd("//iso19prod/IntelligentCombine/Predictive_Maps/2017_Maps/R2_Lead_Farms/Borchert/From_Vista/07Mar18_YIELD_delivery_REVISED2")

fileList <- list.files(pattern = "*.TIF")
fileList # visual check
plotList <- list()
for(i in seq_along(fileList)) {
  field <- raster(fileList[i])
  field[field <= 0] <- NA
  field <- trim(field)
  meanVal <- round(cellStats(field, "mean"), 1)
  medVal  <- round(median(field, na.rm = TRUE), 1)
  minY <- quantile(field, probs = c(0.025), type = 7, names = FALSE)
  maxY <- quantile(field, probs = c(0.975), type = 7, names = FALSE)
  field[field < minY] <- minY
  field[field > maxY] <- maxY
  
  plotList <- gplot(field) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(colours = rev(terrain_hcl(length(unique(na.omit(values(field)))))) ) +
    coord_equal() +
    ggtitle(paste(fileList[1], "Mean = ", meanVal, "Median = ", medVal)) +
    theme(axis.ticks = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank()
    ) 
}
for(plot in plotList){
  print(plot)
}

# for(i in seq_along(fileList)) {
#   field <- raster(fileList[i])
#   field[field <= 0] <- NA
#   field <- trim(field)
#   meanVal <- round(cellStats(field, "mean"), 1)
#   medVal  <- round(median(field, na.rm = TRUE), 1)
#   minY <- quantile(field, probs = c(0.025), type = 7, names = FALSE)
#   maxY <- quantile(field, probs = c(0.975), type = 7, names = FALSE)
#   field[field < minY] <- minY
#   field[field > maxY] <- maxY
#   plot(field, axes = FALSE, box = FALSE, main = paste(fileList[i], "Mean = ", meanVal, "Median = ", medVal))
#   dev.off()
# }

# # for development/testing
# field <- raster(fileList[1])
# meanVal <- round(cellStats(field, "mean"), 1)
# medVal  <- round(median(field, na.rm = TRUE), 1)
# plot(field, axes = FALSE, box = FALSE, main = paste(fileList[i], "Mean = ", meanVal, "Median = ", medVal))
# field[field < 0] <- NA
# field <- trim(field)
# plot(field, axes = FALSE, box = FALSE)
