# Martin Holdrege

# script started: 11/19/18

# functions for working the CR1000 met station data. 
# other functions when working with misc. meteorological data)

library(tidyverse)

# col means of complete cases ---------------------------------------------

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

