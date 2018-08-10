#-------------------------

outputPath <- "~/GitHub/automatePPTX/slideImages"
paramPath <- "~/GitHub/automatePPTX"
paramFN <- "vistaParams.csv"
tableOut <- data.frame(crop = as.character(),
                       field = as.character(),
                       ha = as.numeric(),
                       harvested = as.character(),
                       actual = as.numeric(),
                       can = as.numeric(),
                       vista = as.numeric()
)  
#-------------------------

#-------------------------
# {Load Libraries}
  suppressPackageStartupMessages(library(ggplot2))    # Used for several of the plots
  suppressPackageStartupMessages(library(maps))       # correct Lon/Lat projections for ggplot2 coord_map()
  suppressPackageStartupMessages(library(plyr))       # sort dataframe to make sure in the correct order
  suppressPackageStartupMessages(library(ggmap))      # Used for Google Earth style plots
  suppressPackageStartupMessages(library(colorRamps)) # for matlab.like(...)
  suppressPackageStartupMessages(library(grid))       
  suppressPackageStartupMessages(library(scales))     # for 'pretty_breaks'
  suppressPackageStartupMessages(library(dplyr))      # for 'summarize'
  suppressPackageStartupMessages(library(gridExtra)) # for 'grid.arrange' (?)
  # load packages for spatial join
  #suppressPackageStartupMessages(library(rgdal))
  suppressPackageStartupMessages(library(raster)) 
  suppressPackageStartupMessages(library(spatialEco)) 
#-------------------------
  
  
#-------------------------  
# {Import the parameter csv}
  setwd(paramPath)
  vistaParams <- read.csv(paramFN, header = TRUE, as.is = TRUE)
#-------------------------
  
#-------------------------
# {Iterate the vistaParams row by row}
  for( i in (1 : dim(vistaParams)[1])){
    parameters <- vistaParams[i, ]
#-------------------------

#-------------------------
# {Vista Script Begins}
    dsr <- (1/3)*as.numeric(parameters$dsN)/as.numeric(parameters$dsD)
#------------------------- 
    
## Import
  # - CAN data - filter out non-harvesting Machine State epoch
  # - GeoTiff image
  # - Spatially join CAN data and GeoTiff file

#-------------------------     
# {Conitune}
    setwd(parameters$path1)
    cat("Data from file:", parameters$fn1, "\n")
    HA0 <- read.csv(parameters$fn1, header = TRUE, as.is = TRUE)
    cat("CAN file contained ", dim(HA0)[2], "variables and", dim(HA0)[1], "epochs\n")
    #HA0 <- subset(HA0, ms %in% c(4, 5))
    HA0 <- subset(HA0, Record01 == "On")
    cat("Harvest only epochs", dim(HA0)[2], "variables and", dim(HA0)[1], "epochs\n")
    
    # read TID file as raster image 
    rasterImage <- raster(paste0(parameters$path2, parameters$fn2)) 
    
    # CRS - Coordinate Reference System 
    # WGS84 - commonly used CRS in U.S. 
    geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
    
    # Transform TIF data to WGS84 projection 
    tifData <- rasterToPolygons(rasterImage) 
    tifData <- spTransform(tifData, CRS(geo.prj))  
    
    # Create a SpatialPointsDataFrame from CSV data 
    xy <- HA0[ , c("lon", "lat")] 
    spdf <- SpatialPointsDataFrame(coords = xy, data = HA0, proj4string = CRS(geo.prj)) 
    
    # Spatially join raster image and CSV data 
    joinedDataSet <- point.in.poly(spdf, tifData) 
    HA <- data.frame(joinedDataSet@data) 
    
    # name for predicted yield values in TIF image is same as tif image file name. 
    # filtering out CAN data that does not overlap with TIF data 
    # filteredDataSet <- joinedDataSet@data[joinedDataSet@data[,"merged"] != 0,]  
#------------------------- 

## Tidy
  # - Change input file variable names to match script
  # - Make sure no negative values are included as a result of converting Vista GeoTiff image to CSV file

#------------------------- 
# {Continue}
    #HA$Longitude <- HA$lon
    #HA$Latitude  <- HA$lat
    
    predVar <- gsub(".TIF", "", parameters$fn2)
    predVar <- gsub("-", ".", predVar) # "-" not allowed in raster data frame?
    HA$YieldVista  <- HA[ , predVar]
    
    HA$YieldCAN0  <- HA$yield
    HA$YieldCAN1  <- HA$yield2
    
    HA$YieldVista[HA$YieldVista < 0] <- NA # in case negative values in Vista GeoTiff image (meaning NA)
    
    # Scale Vista Yield to match payable moisture (no change for barley, but changes for canola and wheat)
    #HA$YieldVista <- (1 - params$vml)*HA$YieldVista # remove assumed Vista Moisture Level (vml)
    #HA$YieldVista <- HA$YieldVista/(1 - params$pay) # add Payable moisture (pay)
#------------------------- 
    
## Explore original data
  # - Transform
  # - Visualize
  # - Model
    
## Transform
  # - Stack the 3 sources to create data frame for plotting
  # - Calculate a spatial differences between the Vista and CAN1    

#------------------------- 
# {Continue}
    CAN0 <- HA[ , c("Longitude", "Latitude", "YieldCAN0")]
    CAN0$Source <- rep("CAN0", dim(CAN0)[1])
    names(CAN0) <- c("Longitude", "Latitude", "Yield", "Source")
    
    CAN1 <- HA[ , c("Longitude", "Latitude", "YieldCAN1")]
    CAN1$Source <- rep("CAN1", dim(CAN1)[1])
    names(CAN1) <- c("Longitude", "Latitude", "Yield", "Source")
    
    Vista <- HA[ , c("Longitude", "Latitude", "YieldVista")]
    Vista$Source <- rep("Vista", dim(Vista)[1])
    names(Vista) <- c("Longitude", "Latitude", "Yield", "Source")
    
    merged <- rbind(CAN0, CAN1, Vista)
    
    merged2 <- merged[merged$Yield > 0, ] # remove 0 values per SB's suggestion
    merged2 <- merged2[!is.na(merged2$Yield), ] # remove missing values - clutter up the plots
    
    HAsub <- HA[HA$YieldCAN1 != 0, ] # remove 0 values per SB's suggestion
    HAsub <- HAsub[!is.na(HAsub$YieldCAN1), ] # remove missing values 
    
    HAsub$YieldDiff <- with(HAsub, YieldVista - YieldCAN1)
#-------------------------     

## Visualize
  # - Map Yield by Source: CAN0, CAN1, and Vista
  # - Plot distribution for each Source
  # - Map differences between Vista and CAN1    

#-------------------------
# {Continue}
    mapLon <- with(HA, (min(Longitude, na.rm = TRUE) + max(Longitude, na.rm = TRUE))/2)
    mapLat <- with(HA, (min(Latitude , na.rm = TRUE) + max(Latitude , na.rm = TRUE))/2)
    
    #Remove revgeocode (lines 164-165)?
    cat("The location is:\n")
    (revgeocode(c(mapLon, mapLat)))
    
    map <- get_googlemap(center = c(lon = mapLon, lat = mapLat), zoom = 14, maptype = 'satellite')
    
    minX <- with(HA, min(Longitude, na.rm = TRUE))
    maxX <- with(HA, max(Longitude, na.rm = TRUE))
    minY <- with(HA, min(Latitude, na.rm = TRUE))
    maxY <- with(HA, max(Latitude, na.rm = TRUE))
    
    minX <- round(minX, 4) - 0.0001
    maxX <- round(maxX, 4) + 0.0001
    minY <- round(minY, 4) - 0.0001
    maxY <- round(maxY, 4) + 0.0001
    
    plotMapC4a <- function(lon, lat, mapColor, scaleName, source) {
      
      df <- data.frame(lon = lon, lat = lat, mapColor = mapColor, source = source)
      
      rng1 <- with(subset(df, source == "Vista"), range(mapColor, na.rm = TRUE))
      
      minZ <- rng1[1]
      maxZ <- rng1[2]
      
      df$mapColor <- with(df, ifelse(mapColor < minZ, minZ, mapColor))
      df$mapColor <- with(df, ifelse(mapColor > maxZ, maxZ, mapColor))
      
      scaling <- scale_colour_gradientn(colours = matlab.like(64), 
                                        breaks = pretty_breaks(n = 5), 
                                        guide = guide_colorbar(barwidth = 30, barheight = 1),
                                        name = NULL)
      
      p <- ggmap(map, extent = "device") +
        geom_point(data = df, aes(x = lon, y = lat, color = mapColor), size = I(1/3), alpha = I(1)) +
        labs(x = NULL, y = NULL, 
             title = "CAN0, CAN1 and Vista Yield (t/ha) - Range limited to min & max of Vista values") +
        scaling +
        theme(legend.position = "bottom",
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0, "lines"),
              plot.margin = unit(c(0, 0, 0, 0), "mm"),
              axis.ticks = element_blank(),
              axis.text = element_text(margin = margin(t = 0, unit = "cm")),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.length = unit(0, "null")) +
        scale_x_continuous(limits = c(minX, maxX), expand = c(0, 0)) +
        scale_y_continuous(limits = c(minY, maxY), expand = c(0, 0)) 
      
      p <- p + facet_wrap(~ source, ncol = 3)
      p
    }
    
    plotMapC4b <- function(lon, lat, mapColor, scaleName, source, plotTitle) {
      
      df <- data.frame(lon = lon, lat = lat, mapColor = mapColor, source = source)
      
      rng1 <- with(subset(df, source == "Vista"), range(mapColor, na.rm = TRUE))
      
      minZ <- rng1[1]
      maxZ <- rng1[2]
      
      df$mapColor <- with(df, ifelse(mapColor < minZ, minZ, mapColor))
      df$mapColor <- with(df, ifelse(mapColor > maxZ, maxZ, mapColor))
      
      scaling <- scale_colour_gradientn(colours = matlab.like(64), 
                                        breaks = pretty_breaks(n = 5), 
                                        guide = guide_colorbar(barwidth = 30, barheight = 1),
                                        name = NULL)
      
      p <- ggmap(map, extent = "device") +
        geom_point(data = df, aes(x = lon, y = lat, color = mapColor), size = dsr, alpha = I(1)) +
        labs(title = plotTitle, x = NULL, y = NULL) +
        scaling +
        theme(legend.position = "bottom",
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.spacing = unit(0, "lines"),
              plot.margin = unit(c(0, 0, 0, 0), "mm"),
              axis.ticks = element_blank(),
              axis.text = element_text(margin = margin(t = 0, unit = "cm")),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.length = unit(0, "null")) +
        scale_x_continuous(limits = c(minX, maxX), expand = c(0, 0)) +
        scale_y_continuous(limits = c(minY, maxY), expand = c(0, 0)) 
      
      p <- p + facet_wrap(~ source, ncol = 3)
      p
    }
    
    with(merged2, plotMapC4a(Longitude, Latitude, Yield, "Yield", Source))
    
    ggplot(merged2, aes(x = Yield, color = Source)) + 
      geom_density(size = I(3/2), adjust = I(1)) + 
      labs(title = "Yield density plot", x = "Yield (t/ha)") +
      geom_hline(yintercept=0, color = "grey", size = I(1)) + 
      scale_x_continuous(expand = c(0, 0)) + # , limits = c(0, yul)
      scale_y_continuous(expand = c(0, 0)) 
    
    plotTitle <- "Vista Predicted Yield - CAN1 Yield"
    ggmap(map) + 
      geom_point(data = HAsub, aes(x = Longitude, y = Latitude, color = YieldDiff), 
                 size = I(1)) +
      labs(title = paste(plotTitle), x = NULL, y = NULL) +
      scale_colour_gradientn(colours = matlab.like(64), #limits = c(-5, 5),
                             guide = guide_colorbar(barwidth = 20, barheight = 1)) +
      scale_x_continuous(limits = c(minX, maxX)) +
      scale_y_continuous(limits = c(minY, maxY)) +
      theme(legend.position = "bottom",
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
#-------------------------  
    
## Model
  # - Summary Statistics    

#------------------------- 
# {Continue}    
    ddply(merged2, c("Source"), summarise,
          Min    = round(min(Yield, na.rm = TRUE), 2),
          Q25    = round(quantile(Yield, p = 0.25, na.rm = TRUE), 2),
          Median = round(median(Yield, na.rm = TRUE), 2),
          Avg    = round(mean(Yield, na.rm = TRUE), 2),
          Q75    = round(quantile(Yield, p = 0.75, na.rm = TRUE), 2),
          Max    = round(max(Yield, na.rm = TRUE), 2), 
          StDev  = round(sd(Yield, na.rm = TRUE), 2))
#-------------------------  
    
## Explore after applying median offset
  # - Transform
  # - Visualize
  # - Model
## Transform
  # - Subtract difference in Median(Vista) & Median(CAN) from Vista value.

#------------------------- 
# {Continue}  
    bias <- median(merged2$Yield[merged2$Source == "Vista"]) - median(merged2$Yield[merged2$Source == "CAN1"])
    
    merged3 <- merged2
    merged3$Yield[merged3$Source == "Vista"] <- merged3$Yield[merged3$Source == "Vista"] - bias
    
    HA$YieldDiff2 <- with(HA, YieldVista - bias - YieldCAN1)    
#------------------------- 
    
## Visualize
  # - See if distributions align on density plot, implying offset/bias in Vista prediction
  # - Map differences 
  # - Map resulting yield for each source   
    
#------------------------- 
# {Continue}     
    #cat("Subtract delta(Median Vista Yield & Median CAN1 Yield) =", round(bias, 2), "from Vista values\n")
    
    #pTitle <- paste("Subtracts", round(bias, 2), "from Vista values - 2630 Dry Yield scaled to Actual Dry Yield\n")
    pTitle <- paste("CAN1 scaled to Actual Dry Yield &", round(bias, 2), "subtracted from Vista\n")
    cat(pTitle)
    
    p3 <- ggplot(merged3, aes(x = Yield, color = Source)) + 
      geom_density(size = I(3/2), adjust = I(1)) + 
      labs(title = NULL, x = "Yield (t/ha)") +
      geom_hline(yintercept=0, color = "grey", size = I(1)) + 
      scale_x_continuous(expand = c(0, 0)) + # , limits = c(0, yul)
      scale_y_continuous(expand = c(0, 0)) 
    print(p3)
    
    plotTitle <- "Vista Predicted Yield - CAN1 Yield"
    ggmap(map) + 
      geom_point(data = HA, aes(x = Longitude, y = Latitude, color = YieldDiff2), size = I(1)) +
      labs(title = paste(plotTitle), x = NULL, y = NULL) +
      scale_colour_gradientn(colours = matlab.like(64), #limits = c(-5, 5),
                             guide = guide_colorbar(barwidth = 20, barheight = 1)) +
      scale_x_continuous(limits = c(minX, maxX)) +
      scale_y_continuous(limits = c(minY, maxY)) +
      theme(legend.position = "bottom",
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    fileID <- gsub(".csv", "", parameters$fn1)
    fileID <- gsub("_gam3", "", fileID)
    pTitle <- paste(fileID, "-", pTitle)
    p1 <- with(merged3, plotMapC4b(Longitude, Latitude, Yield, "Yield", Source, pTitle))
    print(p1)
#-------------------------     
    
## Model
  # - Summary Statistics
    
#------------------------- 
# {Continue}     
    p2 <- ddply(merged3, c("Source"), summarise,
                Min    = round(min(Yield, na.rm = TRUE), 2),
                Q25    = round(quantile(Yield, p = 0.25, na.rm = TRUE), 2),
                Median = round(median(Yield, na.rm = TRUE), 2),
                Avg    = round(mean(Yield, na.rm = TRUE), 2),
                Q75    = round(quantile(Yield, p = 0.75, na.rm = TRUE), 2),
                Max    = round(max(Yield, na.rm = TRUE), 2), 
                StDev  = round(sd(Yield, na.rm = TRUE), 2)
    )
    
    print(p2)
    
    c0 <- p2 %>% filter(., Source == "CAN0")
    can0 <- c0$Avg
    vist <- p2 %>% filter(., Source == "Vista")
    vistAvg <- vist$Avg
    area <- parameters$ha
    crop <- as.character(unique(HA0$crop)[1])
    field <- as.character(unique(HA0$Field)[1])
    actual <- parameters$actual
    date <- as.character(unique(HA0$Date)[1])
    
    df <- data.frame(crop = crop,
                     field = field,
                     ha = area,
                     harvested = date,
                     actual =actual,
                     can = can0,
                     vista = vistAvg
    )  
    
    tableOut <- rbind(tableOut, df)
    
#-------------------------      
    
## Communicate
  # - Add results to PowerPoint presentation
    
#------------------------- 
# {Continue}     
    p4 <-grid.arrange(p1, arrangeGrob(tableGrob(p2), p3, ncol = 2), ncol = 1, widths = c(10))
    print(p4)
    
    setwd(outputPath)
    ggsave(plot = p4, file = gsub(".TIF", ".png", parameters$fn2), width = 11, height = 8.5, units = "in")

    
#-------------------------     
       
#-------------------------    
# Close the for loop   
  }
#------------------------- 
  tableFormat <- data.frame(Crop = tableOut$crop, 
                            Field = tableOut$field, 
                            ha = tableOut$ha, 
                            Harvested = tableOut$harvested, 
                            Actual = tableOut$actual,
                            CAN = tableOut$can,
                            Vista = tableOut$vista
  )
  tableFormat <- tableFormat %>% mutate( cDelt = CAN - Actual,
                                         cPerc = 100 * cDelt/Actual,
                                         vDelt = Vista - Actual,
                                         vPerc = 100 * vDelt/Actual)
  cNames <- c("Crop", "Field", "Ha", "Harvested", "Actual", "CAN", "Vista", "CA Delta", "CA Delta P", "V Delta", "V Delta P" )
  colnames(tableFormat) <- cNames
  order <- c("Crop", "Field", "Ha", "Harvested", "Actual", "CAN",  "CA Delta", "CA Delta P","Vista", "V Delta", "V Delta P" )
  
  orderedTable <- tableFormat[, order]
  
  
  