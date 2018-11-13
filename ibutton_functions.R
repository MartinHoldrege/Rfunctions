# Martin Holdrege

# script started: 11/8/18

# functions for working with ibutton data

mean_abdiff <- function(x, date = NULL){
    # args:
    #   x--numeric vector
    #   date--optional vector to sort x by. 
    # returns:
    #   mean of the absolute differences between adjacent elements of x
    if(!is.null(date)){
        x <- x[order(date)]
    }
    
    diffs <- diff(x, lag = 1)
    mean(abs(diffs))
}
