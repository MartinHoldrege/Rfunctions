#Martin Holdrege
#started 10/30/17

#compilation of functions to be used in other scripts

#~~~~~~~~~~~~~~~~~~
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
#~~~~~~~~~~~~~~~~~
presFun=function(x){
  out=sum(x>0)
  return (out)
}#function for counting number of occurences >0 (e.g. number of times a species is present in longform data
#~~~~~~~~~~~~~~
moving.window.quant <- function(x, arrange_by = NULL, window = 20, quant = .1,
                                replace_na = FALSE){
  
    # sorting by arrange_by
    if (is.vector(arrange_by)){
      if(length(arrange_by) != length(x)) stop("arrange_by and x unequal length")
      df <- data.frame(x,arrange_by) %>% 
          arrange(arrange_by)
      x <- df[[1]]
    }  
    
  smoothed <- numeric(length(x)-window)
  for(i in seq_along(smoothed)){
    curr <- x[seq(i,i+window)]
    smoothed[i] <- quantile(curr, quant, na.rm = TRUE)
  }
  smoothed <- c(rep(NA,window/2), smoothed, rep(NA,window/2))
  
  if(!replace_na){
      smoothed[is.na(x)] <- NA
  } #if original data file has NA, smoothed will have NA
      
      
  return(smoothed)
} #moving window quantile function adapted from Will Pearse's function, 
# changed by MH
# arrange_by variabele can be provided to make sure x vector is properly sorted
# quant is quantile to take, window is window width
# arrange_by is option vector to sort x by
# replace_na sets whether smoother will extrapolate to data points where original
# vector had NA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mydiff <- function(x){
  d=abs(diff(x,lag=1))# absolute value of difference between adjacent data points
  c=c(NA,d)#d is one shorter than x, adding NA so vector is same length as x
  return(c)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_bins <- function(x,bins,bin_adjust = 0){
    #bins is vector of bin cuttoffs
    #x is vector to bin
    cuts <-  cut(x, breaks=bins)
    newbins <- bins[unclass(cuts)+1]-bin_adjust#winbin is the upper threshold of the window bin
    newbins
} #by default displays upper edge of bin, can be adjusted down by bin_display
#~~~~~~~~~~~~~~~~~~~~
delete_low_vals <- function(x,coltotest,colname2,threshold){
    low_vals <- x[,coltotest]<threshold
    low_vals <- low_vals %in% TRUE
    x[low_vals, c(colname2,coltotest)] <- NA
    x
} #replace values in two rows of a data frame with NA, based on values of one of those columns (coltotest) being below certain threshold.
#~~~~~~~~~~~~~
plt.mod <- function(model, ...){
    old.par <- par(mfrow=c(2,2))
    plot(model, ...)
    par(old.par)
}#model diagnostic plots, from Will Pearse


#~~~~~~ display model results on a graph
lm_info_display <- function(x, rsqr = T, pvalue = T, eq = F){
    r2 <- round(summary(x)$r.squared,2)
    intercept <-  round(x$coefficients[1],1)
    slope <-  round(x$coefficients[2],2)
    a <- anova(x)
    p <-  a$`Pr(>F)`[1]
    p <- round(p,3)
    if (rsqr == T & pvalue == T & eq == T){
        out <- paste("y = ", intercept,"+",slope,"X",", \nr2 = ",r2,", p = ",p, sep ="")
    } else if (rsqr == T & pvalue == T & eq == F){
        out <- paste("r2=",r2,", p=",p, sep ="")
    } else {out <- "error"}
    out  
} # function that takes model from lm and returns p values etc to paste onto graph
#~~~~~~~~~~~~~
sum_na <- function(x){
    length <-  length(x)
    nas <- sum(is.na(x))
    if (nas == length) {
        out <-  NA
    } else {
        out <- sum(x,na.rm = T)
    }
    out
} # if entire vector is NAs returns NA, else it returns sum with na.rm=T
#ordinary sum() returns 0 if all NAs w/ na.rm = T
#~~~~~~~~~~~~~~~



# doy.h -------------------------------------------------------------------
# library(chron)
# library(lubridate)
doy.h_date_hr <- function(date, hr, origin = c(12,31,2015)){
    library(chron)
    library(lubridate)
    month <- month(date)
    year <- year(date)
    day <- day(date) + hr/24#fractional day
    doy.h <- julian(month, day, year, origin = origin)# converting date into decimal days, with day=1 being 1/1/16
    doy.h <- round(doy.h, digits=3)
    doy.h
} # calculated franctional days since some origin based on date and hour, requires lubridate and chron package

# doy.h_date_hr(ymd("2016-01-01"), 0)


# date.time from date and hr ------------------------------------------------

date_hr <- function(date, hr){
    #library(lubridate)
    hour(date) <- hr
    date <- as.POSIXct(date)
    date
} # takes input of date and hr and returns date.time


#  no cc  --------------------------------------------------------------------

# function that discards the "cc" (control-control, ie,
# shelter-control) plots from a data frame and converts
# trmt back to numeric

nocc <- function(df, trmt = "trmt"){
    
    df <- df[df[[trmt]]!= "cc" & df[[trmt]]!= "CC",]
    df[[trmt]] <- as.numeric(as.character(df[[trmt]]))
    df
}

# un-scale -----------------------------------------------------------------


# takes input of scaled variable (z) and the attributes of that scaled variable
# (attr) to backtransform it, where attr is attributes(z) where, z is the original
# scaled vector

unscale <- function(z, attr){
    s <- attr$`scaled:scale`
    xbar <- attr$`scaled:center`
    x <-  z*s+xbar
    x
}

# re-scale ----------------------------------------------------------------

# takes input of scaled unscaled vector (x) and the attributes of some other 
#scaled vector (attr). Useful for scaling a senthesized data based on mean and
# stdev of actual data

rescale <- function(x, attr){
    s <- attr$`scaled:scale`
    xbar <- attr$`scaled:center`
    z <-  (x-xbar)/s
    z
}


# logit -------------------------------------------------------------------

logit <- function(x, offset = 0) {
  # args:
  #   x --numeric vector
  #   offset--numeric offset, consider adding if x contains 0s
  # returns:
  #   logit of x
  x1 <- x + offset
  log(x1/(1-x1))
}
