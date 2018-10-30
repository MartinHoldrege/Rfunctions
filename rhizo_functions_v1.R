# Martin Holdrege

# script started: 6/25/18

# edited on: 7/30/18,

# functions used with working with minirhizotron data.


# root count --------------------------------------------------------------

root_count <- function(x, win_a = 1){
    y <- x[!is.na(x)]
    l_y <- length(y)
    z <- y[y>0]
    l_z <- length(z)
    if (l_y == 0){
        out <- NA
    } else {
        out <- l_z
    }
    out/win_a #divide by window area to get root count per cm2
} # counting number of roots in a window returns NA if all NAs (meaning window not measured)


# win2depth ---------------------------------------------------------------

# win2depth (calculating depth of a given window by window number and tube angle)
# angle is degrees off horizontal
#window is the minirhizotron window
win2depth <- function(window, angle){
    radians <- angle*pi/180
    sin_angle <- sin(radians)
    hypotenuse <- window*1.358 #(windows to cm)
    depth = sin_angle*hypotenuse
    depth
}
# win2depth(70, 37)


# NewRoots count ----------------------------------------------------------
# problem with this function is that returns "0" new roots for a given session 
# even if result should be NA because that window wasn't sampled. 

NewRoots <- function(df, sessions, output = "freq"){
  # args:
  #   df: tibble (generally formed from nest()) of session numbers
  #       when a given root id occured. dim should be nx1
  #   sessions: numeric vector of session numbers that were sampled
  #   output: one of "session", "freq" or "both".  session just outputs the 
  #       session numbers. freq outputs the frequencies of first roots for the
  #       session numbers, both outputs both session number and frequencies
  # returns
  #   list of number of new roots per session for the given df
  
  x <- df[[1]]
  
  #    error handling:
  if (ncol(df) != 1) {
    stop("df should be a dataframe or tibble with ncol = 1")
  }
  if (!is.vector(sessions) | !is.numeric(sessions)){
    stop ("sessions should be a numeric vector")
  }
  check <- x %in% c(sessions, NA)
  if(sum(!check) > 0){ 
    stop("sessions input vector does not contain all observed values")}
  
  # sessions that didn't have new roots appear
  not_new <- sessions[!(sessions  %in% x)] %>% 
    table() %>% 
    as.data.frame()
  
  names(not_new) <- c("session", "freq")
  not_new$freq <- 0 # no new roots during these sessions
  
  if (sum(is.na(x) == 0)){
    freqs <- table(x) %>% as.data.frame() # frequency of occurence by session
    names(freqs) <- c("session", "freq")
    
    freq_df <- rbind(freqs, not_new) 
  } else {
    freq_df <- not_new 
  }
  
  freq_df <- freq_df %>% 
    mutate(session = as.numeric(as.character(session)),
           freq = as.numeric(as.character(freq)))
  
  if (output == "freq") {
    out <- freq_df$freq
  } else if (output == "session"){
    out <- freq_df$session
  } else {
    out <- freq_df
  }
  
  out
}
