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
#   Perperation and Calculation of versatility
#   --The main purpose of this code was to prepare the versatility tables
#
#======================================================

library(plyr)
source("src/units.R")

get.smooth <- function(df, var){
  N <- length(df$time)

  low1 <- loess.smooth(
   x = as.numeric(df$time),
   y = df[[var]],
   family = "gaussian",
   span = 0.33,
   degree = 2,
   evaluation = N
  )

  data1 <- data.frame(time = low1$x )
  data1$field <- unique(df$field)
  data1[, var]  <- low1$y
  data1$machine_name <- df$machine_name
  return(list(data1))
}

smooth_field <- function(var, df){
  machines <- unique(df$machine_name)
  data1 <- rbindlist(sapply(machines,
                function(mn){
                get.smooth(df[df$machine_name==mn,], var)
          }))
  return(data1)
  N <- length(df$time)

  low1 <- loess.smooth(
   x = as.numeric(df$time),
   y = df[[var]],
   family = "gaussian",
   span = 0.33,
   degree = 2,
   evaluation = N
  )

  data1 <- data.frame(time = low1$x )
  data1$field <- unique(df$field)
  data1[, var]  <- low1$y
  data1$machine_name <- df$machine_name
  return(data1)


  smoothed2 <- subset(df, machine_name == "Simulated")

  N <- length(smoothed2$time) #number of points in the output (1 for every second)

  low2 <- loess.smooth(
   x = as.numeric(smoothed2$time),
   y = smoothed2[[var]],
   family = "gaussian",
   span = 0.33,
   degree = 2,
   evaluation = N
  )
  data2 <- data.frame(time = low1$x )
  data2[, var]  <- low1$y
  data2$field <- unique(smoothed2$field)
  data2$machine_name <- "Simulated"

  smoothed <- rbind(data1, data2)
  return(smoothed)


}



dat.max_capacity <- function(var, dat){
  round(max(dat[,c(var)], na.rm=T), 1)
}

dat.versatility <- function(var, dat, mc=NA){
  if(is.na(mc)){ mc <- dat.max_capacity(var, dat) }
  round(sum(dat[,c(var)], na.rm=T) / (sum(complete.cases(dat[,c(var)]), na.rm=T) * mc), 2)
}

model.model_year.get_valid_rows <- function(model, model_year, field, dat){
  dat <- dat[dat$model==model & dat$model_year==model_year & dat$field==field,]
  bucket_counts <- ddply(dat, .(bucket), summarize, counts=length(unique(machine_name)))
  times <- bucket_counts
  return(merge(times, dat, by=c("bucket")))
}

model.model_year.max_capacity <- function(model, model_year, field, var, dat){
  mns <- unique(dat[dat$model==model & dat$model_year==model_year & dat$field==field,"machine_name"])
  list(do.call(rbind, lapply(
    lapply(mns, function(mn){
      list(group_max_capacity=dat.max_capacity(var, model.model_year.get_valid_rows(model, model_year, field, dat)),
           model=model,
           model_year=model_year,
           machine_name=mn)
    }), data.frame)))
}

machine.max_capacity <- function(machine_name, field, var, dat){
  dat.max_capacity(var, dat[dat$machine_name == machine_name & dat$field == field,])
}

model.model_year.versatility <- function(model, model_year, field, var, dat){
  valid_data <- model.model_year.get_valid_rows(model, model_year, field, dat)
  mc <- dat.max_capacity(var, valid_data)
  mns <- unique(valid_data$machine_name)
  list(do.call(rbind, lapply(
    lapply(mns, function(mn){
      list(group_versatility=dat.versatility(var, valid_data[valid_data$machine_name==mn,], mc=mc),
           model=model,
           model_year=model_year,
           machine_name=mn)
    }), data.frame)))
}

machine.versatility <- function(machine_name, field, var, dat){
  dat.versatility(var, dat[dat$machine_name == machine_name & dat$field == field,])
}

add_bucket <- function(dat, bucket_size=60){
  dat$bucket <- cut(dat$time, breaks = c(-Inf, seq(min(dat$time), max(dat$time), by=bucket_size), Inf))
  return(dat)
}

get_field_group_versatility <- function(field, var, dat, bucket_size=60){
  smoothed_dat <- smooth_field(var, dat)
  dat <- merge(smoothed_dat, unique(dat[,c("machine_name", "model", "model_year")]), by="machine_name")
  dat <- as.data.frame(add_bucket(dat, bucket_size=bucket_size))
  mys <- unique(dat[,c("model", "model_year")])
  mys[order(mys$model_year),"group"] <- 1:nrow(mys)
  mys[,"model_year"] <- as.factor(mys[,"model_year"])
  mc <- rbindlist(mapply(model.model_year.max_capacity,
                         as.character(mys$model),
                         as.character(mys$model_year),
                         field,
                         var,
                         list(dat)))
  v <- rbindlist(mapply(model.model_year.versatility,
                        as.character(mys$model),
                        as.character(mys$model_year),
                        field,
                        var,
                        list(dat)))
  return(merge(merge(mc, v, by=c("model", "model_year", "machine_name")), mys, by=c("model", "model_year")))

}

get_machine_versatility <- function(field, var, dat){
  smoothed_dat <- as.data.frame(smooth_field(var, dat))
  # machines <- unique(dat$machine_name)[order(levels(dat$machine_name))]
  machines <- unique(dat$machine_name)
  dates <- paste(unique(sapply(dat[,"localtime"], sub, pattern="(\\d+-\\d+-\\d+).*", replacement="\\1", perl=T)), collapse=", ")
  # cat("Inside GMV, Dates Calced: ", unique(dates),'\n') #For debugging purposes
  data.frame(
    machine_name = machines,
    max_capacity = mapply(machine.max_capacity, machines, field, var, list(smoothed_dat)),
    versatility = mapply(machine.versatility, machines, field, var, list(smoothed_dat)),
    field = rep(field, along.with=machines),
    date = rep(dates, along.with=machines)
  )

}


get_machine_harvest_time <- function(field, dat, divisor=(60*60)){
  # machines <- unique(dat$machine_name)[order(levels(dat$machine_name))]
  machines <- unique(dat$machine_name)
  harvest_time <- sapply(machines, function(mn){
    nrow(dat[dat$machine_name==mn & dat$field==field,]) / divisor
  })

  data.frame(
    machine_name = machines,
    harvest_time = round(harvest_time, 2)
  )
}

get_versatility_tbl <- function(field, var, dat, digits=2) {
  if(all(is.na(dat[,var]))) return(NA)
  group_versatilities <- get_field_group_versatility(field, var, dat)
  machine_versatilities <- get_machine_versatility(field, var, dat)
  harvest_times <- get_machine_harvest_time(field, dat)

  dat <- merge(merge(machine_versatilities, group_versatilities, by=c("machine_name")), harvest_times, by=c("machine_name"))


  #Get titles for the variable
  var_title <- ifelse(var=="gpscore", "Score", "Engineering")
  var_titles <- paste(var_title, c("Max Capacity", "Versatility"))

  upper_tbl <- c("group", "machine_name", "date","harvest_time", "max_capacity","versatility")
  lower_tbl <- c("group", "machine_name", "group_max_capacity", "group_versatility")

  #Get data parts
  single_data <- dat[,upper_tbl]
  group_data <- dat[,lower_tbl]


  #Adjust names if we are using scores (unitless values)
  if(is.na(get_units(var))) {
    upper_tbl <- c("group", "machine name", "date","harvest time", "max capacity score","versatility")
    lower_tbl <- c("group", "machine name", "group max capacity score", "group versatility")

    single_data$max_capacity <- round(single_data$max_capacity, 0)
    group_data$group_max_capacity <- round(group_data$group_max_capacity, 0)
  } else {
    upper_tbl <- c("group", "machine name", "date","harvest time", "max capacity","versatility")
    lower_tbl <- c("group", "machine name", "group max capacity", "group versatility")
  }
  #make tables
  tbl_a <- kable(single_data[order(single_data$machine_name),], align=rep("c", length(upper_tbl)),
                 digits=digits, format="latex", row.names=FALSE,
                 col.names=sapply(upper_tbl, full_name_with_units), booktabs = TRUE)
  tbl_b <- kable(group_data[order(group_data$machine_name),], align=rep("c",length(lower_tbl)),
                 digits=digits, format="latex", row.names=FALSE,
                 col.names=sapply(lower_tbl, full_name_with_units),  booktabs = TRUE)

  #Convert to text
  tables <- c(tbl_a, tbl_b)
  return(tables)
}
