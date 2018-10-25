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
