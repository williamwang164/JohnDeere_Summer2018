# Prelim Diagnostics
# Author: William Wang
# Purpose: Functions to determine layout of reports


# Figure out how many machines there are
numMachine <- function(df){
  return(length(unique(df$serialnumber)))
}

# Helper function to determine the ammount of days data spans
daySpan <- function(df){
  return(length(unique(df$gpsd)))
}


# State machine to determine report making
# State:
#   00: Data only contains one machine and only spans accross one day
#   01: Data contains multiple machines but only spans accross one day
#   10: Data only contains one machine but spans accross multiple days
#   11: Data contains multiple machines and spans accross multiple days
reportState <- function(df){
  state <- 0
  if(numMachine(df) > 1) {state <- state + 1}
  if(daySpan(df) > 1) {state <- state + 10}
  return(state)
}

multiDayVers <- function(df){
  # Loop through all of the unique date codes
  for( i in unique(df$dateCode)){
    VersDF <- df %>% 
      dplyr::filter(., dateCode == i)
    VersDF$time <- VersDF$DateTime
    VersDF$model_year <- substr(VersDF$serialnumber, 10, 10)
    VersDF$model <- VersDF$serialnumber %>% substr(., 4, 7)
    VersDF$machine_name <- VersDF$Source
    
    p1 <- ggplot(df, aes(x = DateTime, y = pro, colour = Source)) +
      geom_smooth( method = "loess", method.args = list(degree = 2), size = 1.75, se = FALSE, span = 0.33) +
      coord_cartesian(ylim=c(0, 100) + c(0,1)) +
      timeTheme() +
      labs(x = "Time", y = "Grain Productivity, t/h")
    
    vtab <- get_versatility_tbl(unique(bindedDF$field)[1], "pro", VersDF)
    
    print(p1)
    
    cat(vtab[1], "\n")
    cat(vtab[2], "\n")
    
    cat("\\pagebreak")
  }
}
