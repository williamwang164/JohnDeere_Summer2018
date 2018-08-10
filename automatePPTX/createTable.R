# {Load Libraries}
suppressPackageStartupMessages(library(ggplot2))    # Used for several of the plots
suppressPackageStartupMessages(library(maps))       # correct Lon/Lat projections for ggplot2 coord_map()
suppressPackageStartupMessages(library(plyr))       # sort dataframe to make sure in the correct order
suppressPackageStartupMessages(library(ggmap))      # Used for Google Earth style plots
suppressPackageStartupMessages(library(dplyr))      # for 'summarize'
suppressPackageStartupMessages(library(gridExtra)) 
suppressPackageStartupMessages(library(raster)) 
suppressPackageStartupMessages(library(spatialEco)) 

getTables <- function(){
#-------------------------

outputPath <- "~/GitHub/automatePPTX/slideImages"
paramPath <- "~/GitHub/automatePPTX"
paramFN <- "vistaParams.csv"

# {Import the parameter csv}
setwd(paramPath)
vistaparameters <- read.csv(paramFN, header = TRUE, as.is = TRUE)
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
# {Iterate the vistaparameters row by row}
for( i in (1 : dim(vistaparameters)[1])){
  parameters <- vistaparameters[i, ]
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
  #HA$YieldVista <- (1 - parameters$vml)*HA$YieldVista # remove assumed Vista Moisture Level (vml)
  #HA$YieldVista <- HA$YieldVista/(1 - parameters$pay) # add Payable moisture (pay)
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

  minX <- with(HA, min(Longitude, na.rm = TRUE))
  maxX <- with(HA, max(Longitude, na.rm = TRUE))
  minY <- with(HA, min(Latitude, na.rm = TRUE))
  maxY <- with(HA, max(Latitude, na.rm = TRUE))
  
  minX <- round(minX, 4) - 0.0001
  maxX <- round(maxX, 4) + 0.0001
  minY <- round(minY, 4) - 0.0001
  maxY <- round(maxY, 4) + 0.0001
  

  ddply(merged2, c("Source"), summarise,
        Min    = round(min(Yield, na.rm = TRUE), 2),
        Q25    = round(quantile(Yield, p = 0.25, na.rm = TRUE), 2),
        Median = round(median(Yield, na.rm = TRUE), 2),
        Avg    = round(mean(Yield, na.rm = TRUE), 2),
        Q75    = round(quantile(Yield, p = 0.75, na.rm = TRUE), 2),
        Max    = round(max(Yield, na.rm = TRUE), 2), 
        StDev  = round(sd(Yield, na.rm = TRUE), 2))

  bias <- median(merged2$Yield[merged2$Source == "Vista"]) - median(merged2$Yield[merged2$Source == "CAN1"])
  
  merged3 <- merged2
  merged3$Yield[merged3$Source == "Vista"] <- merged3$Yield[merged3$Source == "Vista"] - bias
  
  HA$YieldDiff2 <- with(HA, YieldVista - bias - YieldCAN1)    
  
  fileID <- gsub(".csv", "", parameters$fn1)
  fileID <- gsub("_gam3", "", fileID)

  p2 <- ddply(merged3, c("Source"), summarise,
              Min    = round(min(Yield, na.rm = TRUE), 2),
              Q25    = round(quantile(Yield, p = 0.25, na.rm = TRUE), 2),
              Median = round(median(Yield, na.rm = TRUE), 2),
              Avg    = round(mean(Yield, na.rm = TRUE), 2),
              Q75    = round(quantile(Yield, p = 0.75, na.rm = TRUE), 2),
              Max    = round(max(Yield, na.rm = TRUE), 2), 
              StDev  = round(sd(Yield, na.rm = TRUE), 2)
  )
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
  
  print(p2)
  
  # Close the for loop   O
}

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
return(orderedTable)

}
