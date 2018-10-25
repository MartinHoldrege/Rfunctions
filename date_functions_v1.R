# Martin Holdrege

# script started 5/25/18

# edited on: 

# functions that make working with dates and times easier.



# doy_h -------------------------------------------------------------------

# calculate decimal days since origin 
# input is a date/time vector

doy_h <- function(date_time, origin = c(12,31,2015)){
    library(lubridate)
    library(chron)
    if(!is.POSIXct(date_time)) stop("date_time not POSIXct")
    month <- month(date_time)
    year <- year(date_time)
    hr <- hour(date_time)+minute(date_time)/60
    
    day <- day(date_time) + hr/24#fractional day
    
    # converting date into decimal days, with day=1 being 1/1/16
    doy.h <- julian(month, day, year, origin = origin)
    doy.h <- round(doy.h, digits=3)
    
    doy.h
}






