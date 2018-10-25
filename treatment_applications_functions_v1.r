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


# num_precip_applications(x, 6)

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

#  ------------------------------------------------------------------------

