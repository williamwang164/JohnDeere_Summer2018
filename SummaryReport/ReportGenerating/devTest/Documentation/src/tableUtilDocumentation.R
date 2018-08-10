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
# Template
#     The following is a template of how to create table
#     functions

filtTemplate <- function(df){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Summarises data to be visualized in table form
#        df:    Data frame with only one unique machine categorized
#               by Source ("As Harvest", "Simulated") and contains 
#               all variable/columns of interest
# Output:
#        table: A data frame containing summarised metrics
#--------------------------------------------------------------------------------------------- 

  table <- df %>% 
    dplyr::filter(ms == 4 | ms == 5) %>%        # Filter desired machine states
    dplyr::group_by(Source) %>%                 # Group data by Source
    dplyr::summarise(                           # Sumarise data                         
        metric1 = mean(variable1, na.rm = TRUE),
        metric2 = mean(variable2, na.rm = TRUE),
        metric3 = mean(variable3, na.rm = TRUE),
        metric4 = mean(variable4, na.rm = TRUE),
        metric5 = mean(variable5, na.rm = TRUE)
    )
  
  return(table)
}

createTemplate <- function(df){
#--------------------------------------------------------------------------------------------- 
# Purpose:      Create table summarizing all of the data grouped by machine
#        df:    Data frame with only all machines categorized
#               by Source ("As Harvest", "Simulated") and contains 
#               all variable/columns of interest
# Output:
#        table: A data frame containing summarised metrics for all machines
#--------------------------------------------------------------------------------------------- 
  
  serialNumbers <- unique(df$serialnumber)  # Get all of the machines within the df                    
  
  i <- 1  # Counter designating the current machine, currently 1 (First/Start)
  # Create the table for the first machine
  table <- filtTemplate(df %>% filter(serialnumber == serialNumbers[i]))
  
  i <- i + 1  # Machine 1 is finished, increment the machine counter
  
  while( i <= length(serialNumbers)){ # If there are more than 1 machine, continue creating tables
    # Bind/Append each newly created table to the output table
    table <- rbind(table, filtTemplate(df %>% filter(serialnumber == serialNumbers[i])))
    i <- i + 1 # increment the machine counter
  }
  
  #!NOTE!: This is essentially a DO-While loop since R does not support DO-While grammar
  return(table)
}
#======================================================


#======================================================
# Summary Table Functions:

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

#======================================================