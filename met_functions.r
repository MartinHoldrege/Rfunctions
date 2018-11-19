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
