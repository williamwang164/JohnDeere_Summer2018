# Author: William Wang
# 
# This source file contains functions which pertain to:
#   Scaling limits for map plots
#   Scaling Limits for density plots
#   Theme for map plots
#   Theme for density plots
#   Theme for one page plots
#   Plotting functions
#======================================================


#======================================================
# Utility functions for Plots
#======================================================


limitVar <- function(df, item, pc){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Normalizes data by eliminating extreme values falling outside of the percent
# Inputs:
#        df:    A binded data frame containing all variables from files categorized by Source
#               eg. Source == "As Harvested" || Source == "Simulated"
#        item:  A name for a column in df, eg. "gpssp"
#        pc:    The percent confidence in decimal form, eg. 0.8
# Output:
#        data:  A data frame containing lon, lat, minZ, maxZ, var2 and Source
#               - lon, lat:   used for ploting maps as x,y axis
#               - minZ, maxZ: used as limits
#               - var2:       normalized data from "item" column 
#               - Source:     used for facet wrapping
#---------------------------------------------------------------------------------------------               

  data <- data.frame(lon = df$lon, lat = df$lat, var = df[[item]], Source = df$Source)
  
  data$minZ <- with(data, quantile(var, 1 - pc, na.rm = TRUE))
  data$maxZ <- with(data, quantile(var, pc, na.rm = TRUE))
  
  data$var2 <- with(data, ifelse(var < minZ, minZ, var))
  data$var2 <- with(data, ifelse(var2 > maxZ, maxZ, var2))
  
  return(data)
}



densLimit <- function(df, item, pc){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Calculates the limits to within the percent confidence to scale density plots
# Inputs:
#        df:    A binded data frame containing all variables from files categorized by Source
#               eg. Source == "As Harvested" || Source == "Simulated"
#        item:  A name for a column in df, eg. "gpssp"
#        pc:    The percent confidence in decimal form, eg. 0.8
# Output:
#        data:  A data frame containing var, Source, minZ, maxZ
#               -var:       the column/variable of interest
#               -Source:    used to identifty data source
#               -minZ,maxZ: used to limit the density plot data within percent confidence
#--------------------------------------------------------------------------------------------- 

  data <- data.frame(var = df[[item]], Source = df$Source)
  data$minZ <- with(data, quantile(var, 1 - pc, na.rm = TRUE))
  data$maxZ <- with(data, quantile(var, pc, na.rm = TRUE))
  return(data)
}

densTheme <- function(base_size = 20){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Provides theme for density plots
# Inputs:
#        base_size: max size for text, default is 20pt 
#--------------------------------------------------------------------------------------------- 
  
  theme(
    axis.line =         element_blank(),
    axis.text.x =       element_text(size = base_size, lineheight = 0.9, vjust = 1),
    axis.text.y =       element_text(size = base_size, lineheight = 0.9, hjust = 1),
    axis.ticks =        element_line(colour = "black", size = 0.2),
    axis.title.x =      element_text(size = base_size, vjust = 1),
    axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
    
    legend.background = element_rect(colour=NA), 
    legend.key =        element_rect(colour = "grey80"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       element_text(size = base_size),
    legend.title =      element_blank(),
    legend.position =   "right",
    
    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  element_rect(fill = "grey80", colour = "grey50"), 
    strip.text.x =      element_text(size = base_size * 0.8),
    strip.text.y =      element_text(size = base_size * 0.8, angle = -90),
    
    plot.background =   element_rect(colour = NA),
    plot.title =        element_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  )
}



onePlot <- function(){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Provides theme for plots that take up the whole page
#--------------------------------------------------------------------------------------------- 
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20),
    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines") 
  )
}



mapTheme <- function(){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Provides theme for ggmap plots with facet wraps
#--------------------------------------------------------------------------------------------- 
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    strip.text.x = element_text(size = 40, face = "bold")
  )
}

timeTheme <- function(){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Provides theme for time plots in the versatility section
#--------------------------------------------------------------------------------------------- 
  
  theme(
    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines") 
  )
}

#======================================================
# Plotting functions                                  
#======================================================


plotMap <- function(map, df, item, pc, score = FALSE){
#--------------------------------------------------------------------------------------------- 
# Purpose:      plots maps 
# Inputs:
#        map:   map for ggmap to plot
#        mapDF: data frame from limitVar function
#        pc:    The percent confidence in decimal form, eg. 0.8
#        score: If the item to plot will be a score
# Output:       the plot object
#--------------------------------------------------------------------------------------------- 
  
  if(score){
    scoreDF  <- data.frame(lon = df$lon, lat = df$lat, Source = df$Source, var = df[[item]])
    mapPlot <- ggmap(map) +
      geom_point(data = scoreDF,aes(x = lon, y = lat, colour = var)) +
      scale_colour_gradientn(limits = c(0, 100),
                             colours = matlab.like(64),
                             guide = guide_colorbar(barwidth = unit(.45, "npc"))) +
      scale_x_continuous(limits = c(minX, maxX), expand = c(0, 0)) +
      scale_y_continuous(limits = c(minY, maxY), expand = c(0, 0)) +
      facet_wrap(~ Source) +
      mapTheme()
    
  } else{
    
    mapDF <- limitVar(df, item, pc)
    mapPlot <- ggmap(map) +
      geom_point(data = mapDF, aes(x = lon, y = lat, colour = var2)) +
      scale_colour_gradientn(
        limits = c(mapDF$minZ[1]-1, mapDF$maxZ[1]+1),
        colours = matlab.like(64),
        guide = guide_colorbar(barwidth = unit(0.45, "npc"))
      ) +
      scale_x_continuous(limits = c(minX, maxX), expand = c(0, 0)) +
      scale_y_continuous(limits = c(minY, maxY), expand = c(0, 0)) +
      facet_wrap(~ Source) +
      mapTheme()
  }
  
  return(mapPlot)
}


plotDens <- function(df, item, pc, label, score = FALSE){
#--------------------------------------------------------------------------------------------- 
# Purpose:      plots density maps
# Inputs:
#        df:    binded df with data from all sources
#        item:  A name for a column in df, eg. "gpssp"
#        pc:    The percent confidence in decimal form, eg. 0.92
#        label: a string to label the x axis of the density plot
# Output:       the plot object
#--------------------------------------------------------------------------------------------- 
  
  if(score){
    scoreDF  <- data.frame(Source = df$Source, var = df[[item]])
    ggplot(data = scoreDF) +
      geom_density(aes(x = var, color = Source), size = 2) +
      densTheme() +
      labs(x = label) +
      scale_x_continuous(limits = c(0, 100) ) +
      geom_hline(yintercept = 0, size = 2, colour = "gray")
    
  } else{ 
    densDF <- densLimit(df, item, params$dpc)
    ggplot(data = densDF) +
      geom_density(aes(x = var, color = Source), size = 1.5) +
      scale_x_continuous(limits = c(densDF$minZ[1], densDF$maxZ[1]), expand = c(0, 0))+
      densTheme() +
      labs( x = label) +
      geom_hline(yintercept = 0, size = 2, colour = "gray")
  }
}






