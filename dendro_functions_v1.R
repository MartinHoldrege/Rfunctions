# Martin Holdrege

# script started: 5/1/18

# edited: 5/2/18, 6/27/18, 6/28/18

# Purpose: functions to be used when cleaning/analzying/plotting etc the hardware ranch
# dendrometer data.

library(tidyverse)


# deleting jumps in data --------------------------------------------------

delete_diffs <- function(x, threshold = 1){
    x <- x %>% 
        arrange(date.time) 
    names <- names(x)
    cols <- names[str_detect(names,"mm_\\d\\d.\\d")]
    for(i in seq_along(cols)){
        diff <- mydiff(x[,cols[i]]) # mydiff is in general functions
        x[,cols[i]] <- ifelse(diff > abs(threshold), NA, x[,cols[i]])
    } # deleting values with diffs over given threshold
    x
}


# Graphing functions ------------------------------------------------------

# by plot sensor level graphs
g <- function(x, plot_num, alpha = 0.5){
    # error check
    if(nchar(plot_num) != 2) warning("incorrect # of digits in plot_num") 
    
    cols <- str_c("mm_", plot_num, ".", c(1,2,3)) # col labels
    
    trmt <- trmts_HWRanch(as.numeric(plot_num)) %>% as.character() #trmt label
    
    ggplot(data = x, aes(x = date.time))+
        geom_point(aes_string(y = cols[[1]]), alpha = alpha)+
        geom_point(aes_string(y = cols[[2]]), color = "blue", alpha = alpha)+
        geom_point(aes_string(y = cols[[3]]), color = "red" , alpha = alpha)+
        theme_bw(base_size = 16) +
        labs(title = "Processed dendrometer data",
             subtitle = str_c("plot = ", plot_num, ", treatment = ",trmt),
             x = "date", y = "growth (mm)")
}

#histograms
g_hist <- function(x, facet, title = NULL, subtitle = NULL, xlim = c(-1,3), 
                   binwidth = 0.1){
    ggplot(data = x)+
        geom_histogram(aes(x = mm_growth, y = ..density..), binwidth = binwidth)+
        coord_cartesian(xlim= xlim)+
        facet_wrap(as.formula(paste("~", facet)))+
        labs(title = title,
             subtitle = subtitle)+
        theme_bw(base_size = 16)
}


# sensor replacement  -----------------------------------------------------

# x <- c05
# 
# doy_last_good <- 341.583
# doy_replaced <- 713.417

dendro_adjust <- function(x, sensor_new, doy_last_good, doy_replaced, window = 1, 
                          delete = TRUE){
    
    names <- names(x)
    cols <-  names[str_detect(names,"mm_\\d\\d.\\d")]
    sensor <- cols[str_detect(cols,str_c("mm_\\d\\d.", sensor_new))]
    others <- cols[!cols%in%sensor] # the other sensors
    
    mean_na <- function(x) mean(x,na.rm = T)
    
    initial_other <- x %>% 
        filter(doy.h <= doy_last_good, doy.h >= (doy_last_good - window)) %>% 
        select(others) %>% 
        summarize_all(
            mean_na
        ) %>% 
        as.numeric() %>% 
        mean(na.rm = T) # mean of other sensors before new sensor went bad
    
    final_other <- x %>% 
        filter(doy.h <= (doy_replaced + window), doy.h >= doy_replaced) %>% 
        select(others) %>% 
        summarize_all(
            mean_na
        ) %>% 
        as.numeric() %>% 
        mean(na.rm = T) # mean of other sensors after new sensor replaced.
    
    delta_other <- final_other - initial_other
    
    initial_new <- x %>% 
        filter(doy.h <= doy_last_good, doy.h >= (doy_last_good - window)) %>% 
        select(sensor) %>% 
        summarize_all(
            mean_na
        ) %>% 
        as.numeric() %>% 
        mean(na.rm = T) # mean of other sensors before new sensor went bad
    
    final_new <- x %>% 
        filter(doy.h <= (doy_replaced + window), doy.h >= doy_replaced) %>% 
        select(sensor) %>% 
        summarize_all(
            mean_na
        ) %>% 
        as.numeric() # 24 hr average after new sensor replaced
    
    target <- initial_new+delta_other # what replaced sensor should be starting at
    
    adjust <- target - final_new # how much to adjust sensor 
    y <- x
    
    y[[sensor]] <- ifelse(y$doy.h >= doy_replaced, 
                          y[[sensor]] + adjust,
                          y[[sensor]])
    if(delete){
        y[y$doy.h > doy_last_good & y$doy.h < doy_replaced, sensor] <- NA
    }
    y
  
} 

# x <- dataframe for specific sensor
# doy_last_good is the doy.h of the last good value of the sensor before 
# it went bad.
# doy_replaced is the first doy.h when new sensor started taking good measurement.
# Delete logical is asking whether you want to replace the values in the doy range
# provided, with NA
# window is how many days 
# head(y)
# g(y, "05")



#  summarizing data -----------------------------------------------------------

dendro_means <- function(x, var = NULL, other_group = NULL){
    vars <- c(other_group,"trmt","doy.h","year","hrs","doy", "date.time")
    dots <- lapply(vars, as.symbol)
    x <-  x %>%
        group_by(.dots = dots) %>% 
        summarize(
            mm_growth = mean(mm_growth, na.rm = T)
        ) %>%
        ungroup() 
    
    if(!is.null(var)) {
        names <- names(x)
        names(names) <- names(x)
        names["mm_growth"] <- var
        names(x) <- names
    }
    x
} # summarizing sensor level data, other_group adds additional grouping variable
# var changes the name of the variable being computed


#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------


# functions for interpolating gaps in sensor/plot from data in others --------



# fit y = mx + b ----------------------------------------------------------


lm_coefs <- function(x, y){
    lm <- lm(y~x)
    lm$coefficients   
}



safely_lm_coefs <- purrr::safely(lm_coefs, otherwise = {
                                     warning("error in lm_coeff, skipped safely")
                                 c("(Intercept)" = NA, "x" = NA)}
                                 ) 

safely2_lm_coefs <- function(x, y){
    out <- safely_lm_coefs(x,y)
    out$result
}

x <- c(1,2); y <- c(3,4)
x <- c(1,2); y <- c(NA,NA)

# detecting sequence of NAs -----------------------------------------------

# where x is time, y is growth, and wanting to detect section of nas of given minimum length 
# default minimum gap length is 48 (ie 48 consequtive NAs in y)

na_range <- function(x, y,...){
    #~~~
    given = list(...)
    default = list(gap_length = 48)
    args <- modifyList(default, given)
    gap_length = args[["gap_length"]]
    #~~~
    
    if(length(x) != length(y)) stop("x and y have unequal length")
    
    df_in <- tibble(x = x, y = y) %>% 
        arrange(x)
        
    r <- rle(is.na(df_in$y))
    lengths <- r[["lengths"]]
    values <- r[["values"]]
    
    df_out <- tibble("lengths" = lengths, "values" = values) %>% 
        mutate(end_pos = cumsum(lengths), # end position of seq
               start_pos = end_pos - lengths+1,
               start_x = df_in$x[start_pos], # start x value
               end_x = df_in$x[end_pos]) %>% # index of start of sequence
        filter(values == TRUE, lengths >= gap_length)  
     
    df_out
}




# calculate sequence end averages ---------------------------------

# x is time, y1 is the "reference" sequence of values (likely average of other two plots/sensors)
# y2 is the sequence for which missing data needs to be added
# this outputs the 1 day window avg on either side of the NA gap
# and the day avg on either end of the snippet that will be translocated
# window is the number of units across which to avg


avg_ends <- function(x, y1, y2,...){
    
    #~~~
    given = list(...)
    default = list(window = 1)
    args <- modifyList(default, given)
    window = args[["window"]]
    #~~~
    
    df <- tibble(x, y1, y2) %>% arrange(x)
    range <- na_range(df$x, df$y2,...)

    
    for(i in seq_along(range$lengths)){
        range_i <- range[i, ]
        # avg start of segment to use for replacement
        range[["s_y1_avg"]][i] <- 
            with(range_i, mean(df$y1[df$x >= start_x & df$x < (start_x + window)], 
                                 na.rm = TRUE))
        # avg end of segment to use for replacement
        range[["e_y1_avg"]][i] <- 
            with(range_i, mean(df$y1[df$x  <= end_x & df$x > (end_x - window)], 
                                 na.rm = TRUE))
        # avg of y2 section before NAs
        range[["s_y2_avg"]][i] <- 
            with(range_i, mean(df$y2[df$x < start_x & df$x >= (start_x - window)], 
                                 na.rm = TRUE))
        # avg of section after NAs
        range[["e_y2_avg"]][i] <- 
            with(range_i, mean(df$y2[df$x > end_x & df$x <= (end_x + window)], 
                                 na.rm = TRUE))
        # avg of x for start of segment
        range[["s_xy1_avg"]][i] <- 
            with(range_i, mean(df$x[df$x >= start_x & df$x < (start_x + window)], 
                               na.rm = TRUE))
        # avg x end of segment to use for replacement
        range[["e_xy1_avg"]][i] <- 
            with(range_i, mean(df$x[df$x  <= end_x & df$x > (end_x - window)], 
                               na.rm = TRUE))
        # avg x of y2 section before NAs
        range[["s_xy2_avg"]][i] <- 
            with(range_i, mean(df$x[df$x < start_x & df$x >= (start_x - window)], 
                               na.rm = TRUE))
        # avg x of section after NAs
        range[["e_xy2_avg"]][i] <- 
            with(range_i, mean(df$x[df$x > end_x & df$x <= (end_x + window)], 
                               na.rm = TRUE))
        
    }
    range
}




# predict from slope and intercept ----------------------------------------

pred_mb <- function(x_vec, mb){
    if(length(mb)!= 2) stop("only slope and intercept should be given")
    
    y = mb[2]*x_vec + mb[1]
    y
}

# calculate adjuster models----------------------------------------------------

# calculates the adjuster model. this is the model to multiply the original
# time series by to be able to use it to replace the missing data. 

# "..." can take values for "window" and "gap_length"
interpolate_NA <- function(x, y1, y2,...){
    
    if(!is.numeric(x) || !is.numeric(y1) || !is.numeric(y2)) stop("input not numeric")
    
    dat <- tibble(x, y1, y2) %>% arrange(x)
    x  <-  dat$x; y1 <-  dat$y1; y2 <- dat$y2
    y2_new <- y2
    df_ends <- avg_ends(x, y1, y2,...)
    for (i in seq_along(df_ends$lengths)){
        df_i <- df_ends[i,]
        
        logical <- x >= df_i$start_x & x <= df_i$end_x # range of missing segment
        x_vec <- x[logical]
        
        # lm connecting beginning and end of replacement segment
        lm_y1 <- with(df_i, safely_lm_coefs(x = c(s_xy1_avg, e_xy1_avg), 
                                      y = c(s_y1_avg, e_y1_avg)))
        lm_y1 <- lm_y1$result
        # lm connecting begin/end of either side of NA region 
        lm_y2 <- with(df_i, safely_lm_coefs(x = c(s_xy2_avg, e_xy2_avg), 
                                      y = c(s_y2_avg, e_y2_avg)))
        lm_y2 <- lm_y2$result
        #prediction from above lm
        y1_pred <- pred_mb(x_vec, lm_y1)
        y2_pred <- pred_mb(x_vec, lm_y2)
        
        diff_pred <- y2_pred - y1_pred
        
        y2_new[logical] <- y1[logical] + diff_pred
    }
    y2_new
}


# consistent row means (for when one column or the other has gaps) --------

# one average vector (y1) needed for interpolate_NA function. 
# however, this is usualy a vector of two other column rowMeans. And if these 
# two columns have NAs that can cause unrealist jumps in interpolate_NA

# y1a <- df2[[1]]; y1b <- df2[[2]]

# calculate rowMeans of two rows interpilated based on each other


interp_row_means <- function(x, y1a, y1b,...){
    df <- tibble(x, y1a, y1b) %>% arrange(x)
    
    df$y1a_new <- interpolate_NA(x= df$x, y1 = df$y1b, y2 = df$y1a)
    df$y1b_new <- interpolate_NA(x= df$x, y1 = df$y1a, y2 = df$y1b)
    
     out <- df %>% 
        select(ends_with("_new"))
     
     rowMeans(out, na.rm = TRUE)
}

   
