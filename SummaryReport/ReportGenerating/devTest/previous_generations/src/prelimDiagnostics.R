# Prelim Diagnostics
# Author: William Wang
# Purpose: Functions to determine layout of reports


# Figure out how many machines there are
numMachine <- function(df){
  return(length(unique(df$serialnumber)))
}

# Helper function to determine the ammount of days data spans
daySpan <- function(df){
  return(legnth(unique(df$gpsd)))
}


# State machine to determine report making
# State:
#   00: Data only contains one machine and only spans accross one day
#   01: Data contains multiple machines but only spans accross one day
#   10: Data only contains one machine but spans accross multiple days
#   11: Data contains multiple machines and spans accross multiple days
reportState <- function(df){
  state <- 0
  if(numMachine > 1) {state <- state + 1}
  if(daySpan > 1) {state <- state + 10}
  return(state)
}


