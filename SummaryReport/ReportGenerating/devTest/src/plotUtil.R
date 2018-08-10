# This source file contains functions which pertain to:
#   Scaling limits for map plots
#   Scaling Limits for density plots
#   
limitVar <- function(df, item, pc){
  data <- data.frame(lon = df$lon, lat = df$lat, var = df[[item]], serialnumber = df$serialnumber)
  data$minZ <- with(data, quantile(var, 1 - pc, na.rm = TRUE))
  data$maxZ <- with(data, quantile(var, pc, na.rm = TRUE))
  
  data$var2 <- with(data, ifelse(var < minZ, minZ, var))
  data$var2 <- with(data, ifelse(var2 > maxZ, maxZ, var2))
  
  return(data)
}

densLimit <- function(df, item, pc){
  data <- data.frame(lon = df$lon, lat = df$lat, var = df[[item]])
  minZ <- with(data, quantile(var, 1 - pc, na.rm = TRUE))
  maxZ <- with(data, quantile(var, pc, na.rm = TRUE))
  c(minZ, maxZ)
}

densTheme <- function(base_size = 20){
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

rightTheme <- function(){
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    strip.text.x = element_text(size = 40, face = "bold")
  )
}

timeTheme <- function(){
  theme(
    panel.background =  element_rect(fill = "white", colour = NA), 
    panel.border =      element_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  element_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines") 
  )
}