# Martin Holdrege

# script started: 11/8/18

# functions for working with ibutton data

mean_abdiff <- function(x, date = NULL,...){
    # args:
    #   x--numeric vector
    #   date--optional vector to sort x by. 
    # returns:
    #   mean of the absolute differences between adjacent elements of x
    if(!is.null(date)){
        x <- x[order(date)]
    }
    
    diffs <- diff(x, lag = 1)
    mean(abs(diffs),...)
}


# is shelter --------------------------------------------------------------

is_shelter <- function(trmt){
  # args: 
  #   trmt--vector (usually factor) of treatment types
  # returns:
  #   character string, of whether that trmt has a shelter or is ambient
  trmt <- as.character(trmt)
  out <- ifelse(trmt == "cc" | trmt == "CC", "ambient", "shelter")
  factor(out, levels = c("ambient", "shelter"))
}
