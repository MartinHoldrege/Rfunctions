# Martin Holdrege

# script started: 11/19/18

# functions for working the CR1000 met station data. (in future should also be 
# used for other functions when working with misc. meteorological data)

library(tidyverse)

# col means of complete cases ---------------------------------------------

compColMeans <- function(df, colregex){
  # args:
  #   df--data frame
  #   colregex--regular expression that matches the column names to select
  # Returns:
  #    means of columns selected only including the rows with complete.cases
  df %>% 
    select(matches(colregex)) %>% 
    filter(., complete.cases(.)) %>% # only keeping rows with no NAs
    colMeans()
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