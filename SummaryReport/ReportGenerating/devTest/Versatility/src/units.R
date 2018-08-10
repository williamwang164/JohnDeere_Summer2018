# Author: William Wang
#         Summer 2018 John Deere Intern
#
# !NOTE!: The logic in the following code did not origionate from the author.
#         The following code was borrowed from Matt Coventon and his group. 
#         Changes were made by the author to accomodate the data structure
#         the author was manipulating. Please contact Matt Coventon for the
#         GitHub repository these functions were derived from.
#
# This source file contains functions which pertain to:
#   Units to output in the versatility function
#======================================================

# Unit DF 
units <- data.frame(name=character(),
                    unit=character(),
                    precision=numeric(),
                    stringsAsFactors=FALSE)

units[1, ] <- list("pro", "t/h", 1)
units[2, ] <- list("pu", "kW", NA)
units[3, ] <- list("fcih", "l/t", NA)
units[4, ] <- list("left hand shoe loss", "Count/s", NA)
units[5, ] <- list("right hand shoe loss", "Count/s", NA)
units[6, ] <- list("combined shoe loss", "Count/s", NA)
units[7, ] <- list("separator loss", "Count/s", NA)
units[8, ] <- list("shoe loss difference", "Count/s", NA)
units[9, ] <- list("left hand shoe loss kernels", "Kernels/s", NA)
units[10,] <- list("right hand shoe loss kernels", "Kernels/s", NA)
units[11,] <- list("combined shoe loss kernels", "Kernels/s", NA)
units[12,] <- list("separator loss kernels", "Kernels/s", NA)
units[13,] <- list("harvesting ground speed", "kph", NA)
units[14,] <- list("yield", "t/ha", NA)
units[15,] <- list("propulsion power", "kW", NA)
units[16,] <- list("harvest specific power", "kW/t/h", NA)
units[17,] <- list("rotor drive pressure", "mV", NA)
units[18,] <- list("crop moisture", "%", NA)
units[19,] <- list("roll", "deg", NA)
units[20,] <- list("pitch", "deg", NA)
units[21,] <- list("harvest time", "h", NA)
units[22,] <- list("max capacity", "t/h", NA)
units[23,] <- list("group max capacity", "t/h", NA)
units[24,] <- list("harvest area", "ha", NA)
units[25,] <- list("mf", "kg/s", NA)
units[26,] <- list("gpscore", NA, 0)
units[27,] <- list("power util", '%', 0)
units[28,] <- list("tailings volume", '%', 1)


get_units <- function(name){
  unit <- units[units$name==name, "unit"]
  ifelse(length(unit) > 0, unit, NA)
}


full_name_with_units <- function(var, ...) {
  unit <- get_units(var)
  paste(var, ifelse(is.na(unit), "", paste(",", unit)), sep="")
}
