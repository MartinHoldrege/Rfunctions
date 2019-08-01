# Martin Holdrege

# script started: 11/19/18

# functions for working the CR1000 met station data. 
# other functions when working with misc. meteorological data)

library(tidyverse)

# col means of complete cases ---------------------------------------------
# not really used anymore (kept just in case), compColMeansGroup now used
compColMeans <- function(df, colregex, median = FALSE){
  # args:
  #   df--data frame
  #   colregex--regular expression that matches the column names to select
  #   median- logical, whether to also display median
  # Returns:
  #    means of columns selected only including the rows with complete.cases
  #   or means and medians
  

  df2 <- df %>% 
    select(matches(colregex)) %>% 
    filter(., complete.cases(.)) %>% # only keeping rows with no NAs
    colMeans()
  if (median) {
    mean <- df %>% 
      select(matches(colregex)) %>% 
      filter(., complete.cases(.)) %>% # only keeping rows with no NAs
      summarise_all(funs(mean))
    
    median <-df %>% 
      select(matches(colregex)) %>% 
      filter(., complete.cases(.)) %>% # only keeping rows with no NAs
      summarise_all(funs(median))
    
    df2 <- bind_rows(mean, median)
    df2$fun <- c("mean", "median")
    
  }
  df2
}

# col means by a group variable -------------------------------------------

# groups_by plot, didn't figure out the non-standard eval to make this selectable
compColMeansGroup <- function(df, colregex, median = FALSE){
  # args:
  #   df--data frame
  #   colregex--regular expression that matches the column names to select
  #   median- logical, whether to also display median
  # Returns:
  #    means of columns selected only including the rows with complete.cases
  #   or means and medians
  
  df2 <- df %>% 
    select(plot, matches(colregex)) %>% 
    filter(., complete.cases(.)) %>%  # only keeping rows with no NAs
    group_by(plot)
  
  mean <- df2 %>% 
    summarise_all(list(~mean(.))) %>% 
    mutate(fun = "mean")
    
  if (median) {

    median <-df2 %>% 
      summarise_all(list(~median(.))) %>% 
      mutate(fun = "median")
    
    out <- bind_rows(mean, median)
    return(out)

  }
  mean
}

# is shelter --------------------------------------------------------------

is_shelter <- function(x){
  # args:
  #   x-- character vector created created as "key " column in gather, that has
  #       variable names in it. must contain '_in' or '_out'--ie inside or 
  #       outside shelter
  # returns:
  #   factor vector, of shelter or ambient
  shelter <- str_detect(x, "_in")
  ambient <- str_detect(x, "_out")
  
  # error handling
  test <- ifelse((!shelter & !ambient) | (shelter & ambient),
                 1, 0) 
  if(sum(test) != 0) stop("x must contain '_in' or '_out'")
  
  out <- ifelse(shelter, "shelter", "ambient")
  out <- factor(out, levels = c("ambient", "shelter"))
  out
}


# function for number of days half of precip ------------------------------

precip_half <- function(x, threshold = 0){
  # args:
  #   x-- vector of daily precip for one year
  #   threshold--discard values under a given threshold (good if worried about 
  #   sensor sensitivity of old data)
  # returns:
  #   number of wettest days it took to get half of the annual precip
  #   (this is a metric of precip intensity) 
  #   trying to do what is described by Pendergrass and Knutti 2018
  
  if(sum(!is.na(x)) == 0) return(NA) # if all NA
  
  prop_NA <- sum(is.na(x))/length(x) # proportion missing
  
  if(prop_NA > 0.3) warning("greater than 30% missing values")
  
  x <- x[x >= threshold]
  
  total <- sum(x, na.rm = TRUE) # annual precip
  x2 <- sort(x, decreasing = TRUE)
  frac <- x2/total # faction of total precip
  cum <- cumsum(frac) # cumulative fraction of total precip
  
  # number of days it took to get over half annual precip:
  over <- which(cum > .5)[1]
  under <- over - 1 # num days to get just under half of precip
  diff <- cum[over] - cum[under]
  
  # interpolating between "over" and "under
  diff.5 <- 0.5 - cum[under] # amount that under (num days) is under half the precip
  
  frac.day <- diff.5/diff # fractional day above "under" number of days
  out <- under + frac.day # number of days took to get half precip
  
  out
}

# amount of precip in n days ------------------------------

precip_n <- function(x, n, threshold = 0){
  # args:
  #   x-- vector of daily precip for one year (or month etc)
  #   n-- number of days to use
  #   threshold--discard values under a given threshold (good if worried about 
  #   sensor sensitivity of old data)
  # returns:
  #   fraction of total precip in the wettest n days of the year
  
  if(sum(!is.na(x)) == 0) return(NA) # if all NA
  x <- x[x >= threshold]
  total <- sum(x, na.rm = TRUE)
  prop_NA <- sum(is.na(x))/length(x) # proportion missing
  
  if(prop_NA > 0.3) warning("greater than 30% missing values")
  
  x2 <- sort(x, decreasing = TRUE)
  cum <- cumsum(x2) # cumulative precip
  out <- cum[n]/total
  out
}
# sum or NA ------------------------------------------------------------
sum_orNA <- function(x){
  length <- sum(!is.na(x))
  out <- ifelse(length>0, sum(x, na.rm = TRUE), NA)
  out
} # function to deal with issue that sum(x, na.rm = T) equals 0 if vector all NAs

# F to C conversion -------------------------------------------------------

f_to_c <- function(f) (f-32)*5/9 # converting to celsius



# parse met station data --------------------------------------------------

parse_met <- function(x){
  x %>% 
    mutate(DATE = ymd(DATE),
           month = month(DATE, label = TRUE),
           year = year(DATE),
           TMAX = f_to_c(TMAX), # convert to celsius
           TMIN = f_to_c(TMIN),
           PRCP = PRCP*25.4, # in to mm conversion
           TAVG = (TMAX + TMIN) / 2
    ) %>% 
    select(-STATION, NAME)
}


# -------------------------------------------------------------------------


filter_pull <- function(df, filter_var, pull_var = "year", cutoff = 20) {
  # args:
  #   df--a dataframe
  #   threshold_var--name of variable to filter by
  #   pull_var--name of variable to return
  #   cutoff--threshold above which threshold_var should be
  # returns:
  #   vector 
  stopifnot(is.data.frame(df),
            is.character(filter_var),
            is.character(pull_var),
            is.numeric(cutoff))
  out <- df[df[[filter_var]] > cutoff, ][[pull_var]]
  out
  
}


# calculate snow water equivelent from met data ---------------------------

swe <- function(df, prcp_var, indicator_var, indicator_type){
  # args:
  #   df--data frame of weather data
  #   prcp_var -- name of the precipitation column (e.g. tipping bucket data)
  #   indicator_var -- variable name for that variable that indicates if the 
  #       prcp is snow or rain (either temp data or snow data)
  #   indicator_type -- string either "temp" or "snow", 
  # returns:
  #   snow water equivelent (i.e. values of the prcp var when it was probably snowing)
  #       ie when it was either snowing or below 0 temp
  
  col_names <- names(df)
  
  stopifnot(all(indicator_type %in% c("temp", "snow")),
            all(c(prcp_var, indicator_var) %in% col_names) # valid col names?
  )
  
  prcp <- df[[prcp_var]]
  indicator <- df[[indicator_var]]
  
  either_na <- is.na(prcp) | is.na(indicator) # if NAs in input data return NA
  
  # was it snowing that day?
  is_snow <- if(indicator_type == "snow"){ # snow data
    ifelse(indicator > 0, TRUE, FALSE)
  } else { # temp data
    ifelse(indicator < 0, TRUE, FALSE)
  }
  out <- ifelse(either_na, NA,
                ifelse(is_snow, prcp, 0))
  out
}



# NR-lite wind speed correction -------------------------------------------

Rn_cor <- function(Rn.obs, ws) {
  # args:
  #   Rn--net radiation (W/m^2), measured by nr-lite
  #   ws--wind speed (m/s) 
  # returns:
  #   Rn corrected (as per Campbell nr-lite manual)
  
  
  stopifnot(
    is.numeric(Rn.obs),
    is.numeric(ws),
    all(ws >= 0 | is.na(ws))
  )
  
  if(max(ws, na.rm = TRUE) > 22) {
    warning("wind speed unrealistically high (> 22 m/s (50 mph)")
  }
  
  Rn_windy <- Rn.obs*(1 + 0.021286*(ws-5)) # wind correction
  
  # makes no correction if ws missing
  Rn.cor <- ifelse(ws < 5 | is.na(ws), Rn.obs, Rn_windy)
  Rn.cor
}

# Rn_cor(1:10, c(1:9, NA))


# time series correction --------------------------------------------------


timeseries_cor_sd <- function(x, time, ref_x, ref_time, sd_num = 4, min_ref_n = 10) {
  # args:
  #   x--numeric vector of the time series vector to be cleaned
  #   time--numeric vector that bins x, e.g. (month, doy, week, etc)
  #   ref_x--reference time series vector (could be same as x)
  #   ref-time--numeric vector that bins ref_x, (should contain same values as time)
  #   sd_num--number of sds above the ref_x mean for a bin above which a value will be replace with NA
  #   min_ref_n--gives warning if ref_x has less than that number of non NA values for a given time bin
  # returns:
  #   numberic vector, outliers replaced with NA
  
  stopifnot(
    length(x) == length(time),
    length(ref_x) == length(ref_time),
    all(!is.na(time)), # can't have missing time
    # test all input vectors numeric:
    all(map_lgl(list(x, time, ref_x, ref_time), is.numeric)),
    length(sd_num) == 1,
    sd_num >= 0,
    all(unique(time) %in% unique(ref_time)) # ref_time must have all bins
  )
  
  time_bins <- unique(time)
  
  x_out <- x
  for (bin in time_bins){
    ref_x_sub <- ref_x[ref_time == bin] 
    
    # enough ref values?
    n_ref <- sum(!is.na(ref_x_sub))
    if(n_ref < min_ref_n){
      warning(paste("only", n_ref, 
                    "non NA reference values available for ref_time ==", bin))
    }
    ref_mean <- mean(ref_x_sub, na.rm = TRUE)
    ref_sd <- sd(ref_x_sub, na.rm = TRUE)
    
    upper <- ref_mean + sd_num*ref_sd
    lower <- ref_mean - sd_num*ref_sd
    
    x_out[time == bin] <- ifelse(x[time == bin] > upper | x[time == bin] < lower,
                                 NA, x[time == bin])
    
  }
  
  num_outlier <- sum(is.na(x_out)) - sum(is.na(x))
  message(paste(num_outlier, "outliers were replaced with NA"))
  
  x_out
}

# example use of the function:
if (FALSE) {
  # make some data
  x <- c(rnorm(100, 0, 1), rnorm(100, 5, 1))
  ref_x <- x
  # add noise
  x[c(20, 30:35, 150:155)] <- x[c(20, 30:35, 150:155)] + 10

  time <- rep(c(1, 2), each = 100)
  
  ref_time <- time

  x_cor <- timeseries_cor_sd(x, time, ref_x, ref_time)
  
  par(mfrow = c(1, 2))
  plot(x, ylim = range(x))
  plot(x_cor, ylim = range(x))
} 


# add plot_name column ----------------------------------------------------

add_plot_name <- function(df) {
  # args:
  #  df--data frame with a plot column
  # returns:
  #  df with new col plot_name (e.g. "plot 4")
  stopifnot(
    is.data.frame(df),
    "plot" %in% names(df),
    is.numeric(df$plot)
  )
  
  out <- df %>% 
    mutate(plot_name = paste("plot", plot))
  out
}


# min/max better handles all NAs ----------------------------------------------


fun_na <- function(f) {
  # args:
  #  f--a function
  # returns:
  #   returns a function,  returns outcome of f (na.rm option), unless all NAs, then NA (instead of Inf)
  
  fun <- function(x, na.rm = FALSE){
    stopifnot(is.numeric(x))
    x2 <- x[!is.na(x)]
    out <- if(length(x2) < 1) {
      NA
    } else {
      f(x, na.rm = na.rm)
    }
    out
  }

  fun
}

max_na <- fun_na(max)

min_na <- fun_na(min)

if (FALSE) {
  max_na(NA_real_)
  max_na(1:10)
  max_na(c(1:10, NA))
  max_na(c(1:10, NA), na.rm = TRUE)
  min_na(1:10)
}
