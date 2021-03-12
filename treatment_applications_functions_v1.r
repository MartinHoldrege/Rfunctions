  # martin holdrege

# edited on: 6/9/18 

# functions for calculating treatment events etc generally from met data

library("tidyverse")


# helper functions --------------------------------------------------------
# these functions used within the other functions below

# is input ok?
check <- function(x) {
    if(!is.numeric(x)) stop("input not a numeric vector")
    if(sum(is.na(x)) > 0) warning("input contains NA")
}

# function for creating the new vector to be filled in with left over precip
# ie precip left over during a give time period after a dump of tank
create_left_over <- function(x, trt_size){
    left_over_precip <- rep(NA, times = length(x))

    #initializing first left over precip
    left_over_precip[1] <- x[1] %% trt_size # the remainder

    left_over_precip
}

#  ------------------------------------------------------------------------
# this function is for counting how many days received precip under a given 
# trmt 

num_precip_applications <- function(x, trt_size){
    #vector of DAILY precip occumulation (mm)
    
    check(x)

    x <- x[!is.na(x)]
    #vector of cumulative precip 
    cum_precip <- cumsum(x)
    
    left_over_precip <- create_left_over(x, trt_size)
    
    dumps <- rep(NA, times=length(x))#1 if day had dumped

    dumps[1] <- if(cum_precip[1]/trt_size > 1) {1} else {0}

    for (i in 2:length(cum_precip)){
        #calculating how much precip left over in tank after full dumps of a given size
        left_over_precip[i] <- (cum_precip[i] - cum_precip[i-1] + 
                                    left_over_precip[i-1]) %% trt_size
        
        #vector of dumps 1=yes tank dumped (one or more times) on that day, 0 means did not. 
        dumps[i] <- if((cum_precip[i]-cum_precip[i-1]+left_over_precip[i-1])/trt_size >= 1)
            { 1 } else 0
    }
    
    sum(dumps)
  }



#  ------------------------------------------------------------------------

# function for creating precip treatment events (ie theoretically when tanks dump,
# based on input vector of precip data)

# x <- met17d$precip_tb_cmhr
# trt_size <- .4

trmt_events <- function(x, trt_size){
    # trt_size and vector of hourly (or daily) precip occumulation (x) need to be in the same
    # unit
    check(x)
    
    x <- x[!is.na(x)]
    cum_precip <- cumsum(x)
    
    dumps <- rep(NA, times=length(x)) #1 if day had dumped
    
    left_over_precip <- create_left_over(x, trt_size)
    
    dumps[1] <- cum_precip[1] %/% trt_size # first 
    
    for (i in 2:length(cum_precip)){
        #calculating how much precip left over in tank after full dumps of a given size
        left_over_precip[i] <- (cum_precip[i] - cum_precip[i-1] + 
                                    left_over_precip[i-1]) %% trt_size
        
        # vector of dumps counts, not binary unlike the above function 
        dumps[i] <- (cum_precip[i]-cum_precip[i-1]+left_over_precip[i-1]) %/% 
            trt_size
    }
    
    out <- dumps*trt_size # 
    out
}

# sum(trmt_events(x, .5))
# sum(x)
# 
# # testing
# trt_size <- 6
# x <- rep(c(5,1), 10)
# 
# 
# trmt_events(x, 6)

# calculate amount applied each day --------------------------------------
# amount applied by trmt given observed ambient precip

tipping_bucket <- function(x, trt_size) {
  # args:
  #   x--vector of daily precip
  #   trt_size--amount of precip applied in each tip of bucket (same units at x)
  # returns:
  #   vector of length x with the amount of precip applied each day 
  #   based on tipping bucked model 
  
  check(x) # can't have NAs in x
  
  #vector of cumulative precip 
  cum_precip <- cumsum(x)
  
  dumped <- rep(NA, length(cum_precip))
  for (i in seq_along(cum_precip)) {
    
    integer <- cum_precip[i]%/%trt_size # number of dumps that day
    amount_dumped <- integer*trt_size
    dumped[i] <- amount_dumped
    cum_precip <- cum_precip - amount_dumped # amount remaining (in subsequent days)
    
  }
  
  dumped
}


# test
if (FALSE) {
  x <- c(1, 1, 1, 1, 0, 0, 0, 8, 2, 2, 0, 0, 1, 4, 3, 0, 0)
  trt_size = 3
  out <- tipping_bucket(x = x, trt_size = trt_size)
  x
  out
  sum(out)
  sum(x)
  (sum(x)%/%trt_size)*trt_size # amount applied
}


# -1 c treatment ----------------------------------------------------------

tipping_bucket_neg1 <- function(x, interval, dump_size = 1, trt_size = 0.56) {
  # args:
  #   x--vector of daily precip
  #   interval--interval at which additional small events added
  #   dump_size --size of precip additions
  #   trt_size--amount of precip accomulated before bucket tips 
  #     (when given natural precip)
  # returns:
  #   vector of length x with the amount of precip applied each day 
  #   based on additions at given interval. This is the expecte timeseries
  #    of the '1 mm' treatment (i.e. treatment meant to reflect -1C temp change)
  
  stopifnot(
    is.numeric(interval),
    length(interval) == 1
  )
  
  # removing small amount from each precipt event so there is enought to 
  # redoposit in extra small 1 mm events
  x_sub <- x
  n <- length(x_sub)
  n_extra <- n%/%interval
  
  dump_size <- 1 # ie 1mm events (change for different event sizes)
  sum_extra <- n_extra * dump_size 
  
  n_events <- length(x_sub[x_sub > dump_size]) # num natural events > 1
  size_remove <- sum_extra/n_events
  
  # removing small amount from all events that are > 1
  x_sub[x_sub > dump_size] <- x_sub[x_sub > dump_size] - size_remove
  
  new_dumps <- add_extra_dumps(x_sub, dump_size = dump_size, 
                               interval = interval)
  
  x_sub2 <- tipping_bucket(x_sub, trt_size = trt_size)
  x_sub2 <- ifelse(is.na(new_dumps), x_sub2, new_dumps)
  
  x_sub2
}


# vector of extra dumps ---------------------------------------------------


add_extra_dumps <- function(x, dump_size, interval) {
  # This function is used inside tipping_bucket_neg1
  # args:
  #   x--numeric vector (precip data)
  #   dump_size--how big should the additional events be
  #   interval--how often to add additional events
  # returns:
  #   numeric vector with dumps of size dump_size at the specified interval
  #   put in the location of first 0 value in x found after the interval
  n <- length(x)
  n_extra <- n%/%interval
  
  positions <- seq(from = interval, by = interval, length.out = n_extra)
  
  dump_locations <- rep(NA, n_extra)
  zero_locations <- which(x == 0)
  
  # finding first 0s at the specified intervals
  for (i in seq_along(positions)) {
    dump_locations[i] <- min(zero_locations[zero_locations >= positions[i]])
  }
  
  #  (ie if no zero after last interval)
  dump_locations <- dump_locations[!is.infinite(dump_locations)]
  
  out <- rep(NA_real_, n)
  out[dump_locations] <- dump_size
  
  out
}

if (FALSE) {
  x <- c(3, 0, 2, 0, 0, 0, 2, 2, 2, 0 )
  add_extra_dumps(x, dump_size = 1, interval = 4)
  year <- rep(2017, length(x))
  out <- tipping_bucket_neg1(x, interval = 4, trt_size = 0.1)
  out
  sum(x); sum(out)
  x <- c(3, 0, 2, 0, 0, 0, 2, 2, 2, 2 )
  add_extra_dumps(x, dump_size = 1, interval = 4)
}

