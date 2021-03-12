# Martin Holdrege

# Various figure parameters (colors etc) that might be useful across projects


source("~/Analysis/Rfunctions/assign_trmts_functions.r")
# legend labels -----------------------------------------------------------

library(dplyr)
legend_lab <- trmts_HWRanch(c(7, 4, 3, 11, 5, 12, 14), dump_label = TRUE) %>% 
  as.character()
  # as.numeric() %>% 
  # round(0) %>% 
  # paste("mm")

# legend_lab[2] <- "control"
names(legend_lab) <- names(trmts_HWRanch(c(7, 4, 3, 11, 5, 12, 14), convert2mm = TRUE))

# colors ------------------------------------------------------------------

# excluding cc trmt

# colors
pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
pal <- pal[-c(1:2, 4:5)]
pal[2] <- "black"
# length(pal)


pal_17_19 <- c("#d8b365", "#5ab4ac") # pallette for separating years (clarkston)

