# Martin Holdrege

# script started: 5/28/18

# edited on:

# collection of functions to be used when preparing and analyzing diviner data







# absolute mean difference ------------------------------------------------

# x <- (HW_div.m2 %>%
#     filter(plot == 1, depth == 10))[["moisture"]]
# date <- (HW_div.m2 %>%
#           filter(plot == 1, depth == 10))[["date.time"]]

# function to calculate the absolute mean difference of a vector
# name is shortening of difference absolute mean
# where date is any vector that can be coerced to numeric for sorting purposes.
# e.g. date.time/doy.h since a origin would work. 
diff_am <- function(x, date){

    if(!is.numeric(x)) stop("x not a numeric vector")
    if(!is.atomic(date)) stop("date is not an atomic vector")
    
    date <-  as.numeric(date)
    df <- data.frame(x = x, date = date) %>% 
        dplyr::arrange(date) # sorting so that taking difference of truely adjacent values
    vec <- df[["x"]]
    d <- diff(vec)
    ad <- abs(d)
    mean(ad, na.rm = T)
}


# sum Absolute difference -------------------------------------------------


diff_asum <- function(x, date){
    
    if(!is.numeric(x)) stop("x not a numeric vector")
    if(!is.atomic(date)) stop("date is not an atomic vector")
    
    date <-  as.numeric(date)
    df <- data.frame(x = x, date = date) %>% 
        dplyr::arrange(date) # sorting so that taking difference of truely adjacent values
    vec <- df[["x"]]
    d <- diff(vec)
    ad <- abs(d)
    sum(ad, na.rm = T)
}


# sum positive increment ------------------------------------------------------



incr_pos <- function(x, date = NULL, rate = FALSE, obs_month = 365/12){
  # args:
  #   x--numeric vector of soil moisture (vwc, wp etc)
  #   date --optional date vector, can help make sure x is ordered by the date vector. 
  #   rate--logical whether to return output as rate (increment/month), currently
  #     works for daily data
  #   obs_month--number of observations per month (default is for daily data)
  # returns:
  #   summed positive increment, (as total or as rate)

    if(!is.numeric(x)) stop("x not a numeric vector")
    if(!is.atomic(date) & !is.null(date)) stop("date is not an atomic vector")
    
    if (is.null(date)){
        vec <- x
    } else {
        date <-  as.numeric(date)
        df <- data.frame(x = x, date = date) %>% 
            dplyr::arrange(date) # sorting so that taking difference of truely adjacent values
        vec <- df[["x"]]
    }
   
  
    d <- diff(vec)
    pos <- d[d > 0]
    
    n <- length(d[!is.na(d)])
    out <- if(n == 0) {
        NA 
        } else {
            sum(pos, na.rm = TRUE)
        }
    
    out <- if(rate){
      out/n*obs_month # convert to increment/month
    } else {
      out
    }
    
    out
}

# test
if (FALSE){
  incr_pos(as.numeric(rep(NA, 4)), 4:1)
  incr_pos(1:60, rate = TRUE)
}
# sum positive increment for hourly data paw----------------------------------
# more specific function than above. 

# This function might not be useful but could later be adapted for better incr
# calculation
incr_paw_hr <- function(vwc, wp, date.time, wp_threshold = -3, rate = TRUE, 
                        step_sec = 3600, obs_day = 24){
  # args:
  #   vwc--numeric vector of hourly vwc soil moisture
  #   wp--numeric vector of hourly water potential
  #   date.time --optional date vector, can help make sure vwc is ordered. 
  #   rate--logical whether to return output as rate (increment/day), currently
  #     works for daily data
  #   step_sec--number of seconds measurements apart
  #     values discarded if difference between measurements is greater than this
  #   obs_month--number of observations per month (default is for daily data)
  # returns:
  #   summed positive increment, (as total or as rate)
  
  if(!is.numeric(vwc)) stop("vwc not a numeric vector")
  if(!(is.POSIXct(date.time) & is.POSIXt(date.time))) {
    stop("date.time wrong class")
  } 
  
  date.time <-  as.numeric(date.time)
  df <- data.frame(vwc = vwc, wp = wp, date.time = date.time) %>% 
    dplyr::arrange(date.time) %>% # sorting so that only taking difference of truely adjacent values
    mutate(diff.time = c(NA, diff(date.time)),# time since previous meas
           vwc_incr = c(NA, diff(vwc)),
           # plant available only increments:
           vwc_incr_pa = ifelse(wp > wp_threshold, vwc_incr, NA)) %>% 
    filter(diff.time == step_sec)   # only keeping meas 1 hour (default) apart 
  
  n <- nrow(df[!is.na(df$vwc_incr),]) # number of valid measurements
  
  out <- if(n == 0) {
    NA 
  } else {
    sum(pos, na.rm = TRUE)
  }
  
  out <- if(rate){
    out/n*obs_day # convert to increment/month
  } else {
    out
  }
  
  out
}

# df <- hr_incr1 %>%
#   filter(depth == 10, trmt == 0)
# vwc <- hr_incr1$vwc
# wp <- hr_incr1$wp
# date.time <- hr_incr1$date.time


# super specific funs -----------------------------------------------------
# for clarkston
                                   
summarize_vwc <- function(df) {
  # df--grouped dataframe
  stopifnot(
    is.data.frame(df),
    "vwc" %in% names(df)
    
  )
  summarize(df, 
            vwc_se = plotrix::std.error(vwc, na.rm = TRUE),
            vwc = mean(vwc, na.rm = TRUE)) %>% 
    ungroup()
}

summarize_delta_vwc <- function(df) {
  # df--grouped dataframe
  stopifnot(
    is.data.frame(df),
    "delta_vwc" %in% names(df)
    
  )

  df %>% 
    summarize(delta_vwc_se = plotrix::std.error(delta_vwc, na.rm = TRUE),
              delta_vwc = mean(delta_vwc, na.rm = TRUE)) %>% 
    ungroup()

}

summarize_incr <- function(df) {
  # df--grouped dataframe
  stopifnot(
    is.data.frame(df),
    "incr_pos" %in% names(df)
    
  )
  summarize(df, 
            incr_pos_se = plotrix::std.error(incr_pos, na.rm = TRUE),
            incr_pos = mean(incr_pos, na.rm = TRUE)) %>% 
    ungroup()
}

crop_yrs <- function(df) {
  # return df with only  years during which crop occured
  stopifnot(
    is.data.frame(df),
    "year" %in% names(df)
    
  )
  crop_yrs <- c(2017, 2019) 
  
  out <- filter(df, year %in% crop_yrs)
  out
}

