# Martin Holdrege

# Various figure parameters (colors etc) that might be useful across projects



# legend labels -----------------------------------------------------------


legend_lab <- expression(1~mm~(-1~degree*C),
                         control~(0~degree*C),
                         2~mm~(1~degree*C),
                         3~mm~(2~degree*C),
                         4~mm~(3~degree*C),
                         8~mm~(5~degree*C),
                         18~mm~(10~degree*C))


# colors ------------------------------------------------------------------

# excluding cc trmt

# colors
pal <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))
pal <- pal[-c(1, 4)]
pal[2] <- "black"
