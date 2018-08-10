# Author: William Wang
#         Summer 2018 John Deere Intern
#
# This source file contains functions which pertain to:
#   Data filter functions
#   --These functions help with the creation of summary
#     tables.
#   Creation of summary tables
#   --These functions are coded to accomodate creating
#     tables for a variable ammount of unique machines
#======================================================

#======================================================
# Summary Table Functions:
#======================================================

filtHPS <- function(df){
  df %>% dplyr::filter(ms == 4 | ms == 5) %>% dplyr::group_by(Source) %>% dplyr::summarise(
    Performance = mean(opscore, na.rm = TRUE),
    Productivity = mean(gpscore, na.rm = TRUE),
    Power = mean(puscore, na.rm = TRUE),
    Fuel = mean(fescore, na.rm = TRUE),
    Grain = mean(glscore, na.rm = TRUE)
  )
}

createHPS <- function(df){
  serialNumbers <- unique(df$serialnumber)
  i <- 1
  table <- filtHPS(df %>% filter(serialnumber == serialNumbers[i]))
  i <- i + 1
  
  while( i <= length(serialNumbers)){
    table <- rbind(table, filtHPS(df %>% filter(serialnumber == serialNumbers[i])))
    i <- i + 1
  }
  
  return(table)
}

filtHPEU <- function(df){
  df %>%
    dplyr::filter(ms == 4 | ms == 5) %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(
      GrainProd = mean(pro, na.rm = TRUE),
      PowerUtil = mean(pu, na.rm = TRUE),
      FuelEcon = mean(fcih, na.rm = TRUE)
    )
}

createHPEU <- function(df){
  serialNumbers <- unique(df$serialnumber)
  i <- 1
  table <- filtHPEU(df %>% filter(serialnumber == serialNumbers[i]))
  i <- i + 1
  
  while( i <= length(serialNumbers)){
    table <- rbind(table, filtHPEU(df %>% filter(serialnumber == serialNumbers[i])))
    i <- i + 1
  }
  
  return(table)
}

filtPS <- function(df){
  df %>%
    dplyr::filter(ms == 4 | ms == 5 ) %>%
    dplyr::group_by(Source) %>%
    dplyr::summarise(
      EngineHarv = mean(ep, na.rm = TRUE),
      EngineHarvUL = mean(ep, na.rm = TRUE),
      AvgHarvSpecificPwr = mean(ifelse(pro == 0, NA, ep/pro), na.rm = TRUE),
      PropulsionPower = mean(pp, na.rm = TRUE)
    )
}

createPS <- function(df){
  serialNumbers <- unique(df$serialnumber)
  i <- 1
  table <- filtPS(df %>% filter(serialnumber == serialNumbers[i]))
  i <- i + 1
  
  while( i <= length(serialNumbers)){
    table <- rbind(table, filtPS(df %>% filter(serialnumber == serialNumbers[i])))
    i <- i + 1
  }
  
  return(table)
  
}

filtAOS <- function(df){
  df %>% 
    dplyr::filter(ms == 4 | ms == 5) %>% 
    dplyr::group_by(Source) %>% 
    dplyr::summarize(
      HarvestSpeed = mean(gpssp, na.rm = TRUE),
      HarvestArea = sum(gpssp * pmin(hwu, hwi, na.rm = TRUE) / 36000, na.rm = TRUE),
      AvgYield = mean(yield, na.rm = TRUE)
    )
}

createAOS <- function(df){
  serialNumbers <- unique(df$serialnumber)
  i <- 1
  table <- filtAOS(df %>% filter(serialnumber == serialNumbers[i]))
  i <- i + 1
  
  while( i <= length(serialNumbers)){
    table <- rbind(table, filtAOS(df %>% filter(serialnumber == serialNumbers[i])))
    i <- i + 1
  }
  
  return(table)
}

