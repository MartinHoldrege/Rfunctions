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

# date field can help make sure x is ordered by the date vector. 

incr_pos <- function(x, date = NULL){
    
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
    out <- if(length(d[!is.na(d)]) ==0) {
        NA 
        } else {
            sum(pos, na.rm = TRUE)
        }
    out
}

incr_pos(as.numeric(rep(NA, 4)), 4:1)

