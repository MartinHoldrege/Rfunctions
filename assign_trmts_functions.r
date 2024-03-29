#Martin Holdrege
#functions for assigning the correct treatments to plot numbers at Hardware Ranch
#and clarkston.


# trmts_clark -------------------------------------------------------------


#creating vector of the trmts based on vector of plot numbers at clarkston
trmts_clark <- function(x, convert2mm = FALSE, dump_label = FALSE){
    # args:
    #  x--plot number
    #  convert2mm --logical whether to output treatment as mean event size (mm)
    #   dump_label--logical, whether to instead output the treatment label 
    #     (i.e. minimum event size)
    # returns:
    #   vector with length of x
    stopifnot(is.numeric(x),
              all(x %in% 1:14))
    lookup <- c("1"=3, "2"=1,"3"=3,"4"=0,"5"=0,"6"="cc","7"=-1,"8"=2,
                "9"=5,"10"="cc","11"="cc","12"=10,"13"=0,"14"=3)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    out <- unname(lookup[x])
    if (convert2mm | dump_label) {
        # not sure why this works--recursion--c2mm_HWRanch calls trmts_clark
        out <- c2mm_clark(out, dump_label = dump_label)
    }
    out
 }

#creating vector of the trmts based on vector of plot numbers at Hardware
trmts_HWRanch <- function(x, convert2mm = FALSE, dump_label = FALSE){
    stopifnot(is.numeric(x),
              all(x %in% 1:14))
    lookup <- c("1"="cc", "2"=0,"3"=1,"4"=0,"5"=3,"6"="cc","7"=-1,"8"=3,
                             "9"=0,"10"="cc","11"=2,"12"=5,"13"=3,"14"=10)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    out <- unname(lookup[x])
    if (convert2mm | dump_label) {
        out <- c2mm_HWRanch(out, dump_label = dump_label)
    }
    out
}


# convert trmt levels from deg C to mm ------------------------------------


c2mm_HWRanch <- function(x, dump_label = FALSE) {
    # args:
    #   x--vector of treatment levels (old deg C labeling)
    #   return labels of the dump size instead
    # returns:
    #   vector of treatment levels converted to mm precip (mean event sizes)
    #       or trmt label (dump size, mm)
    
    stopifnot(
        is.logical(dump_label)
    )

    x_char <- as.character(x)
    
    if (!dump_label) {
        # mean event sizes (from tipping bucket applied to non-winter months)
        lookup_nocc <- c("-1" = 4.8, "0" = 5.3, "1" = 6.2, "2" = 7.2, "3" = 8.4, "5" = 10.8, 
                         "10" = 19.4)
    } else {
        lookup_nocc <- c("-1" = "1 mm", "0" = 'Control', "1" = "2 mm", 
                         "2" = "3 mm", "3" = "4 mm", "5" = "8 mm", 
                         "10" = "18 mm")
    }

    
    if(is.numeric(x)){
        stopifnot(all(x %in% c(-1, 0, 1, 2, 3, 5, 10)))
        out <- lookup_nocc[x_char]
        
    } else if(is.factor(x)) {
        stopifnot(
            all(x_char %in% as.character(trmts_HWRanch(1:14)))
            )
        lookup_all <- c("cc" = "cc", lookup_nocc)
        lookup_all <- factor(lookup_all, levels = unname(lookup_all), ordered = TRUE)
        out <- lookup_all[x_char]
    } else {
        stop("x must be numeric or factor")
    }
    out
}

# test
if (FALSE) {
   x <- trmts_HWRanch(1:14) 
   c2mm_HWRanch(x)
   c2mm_HWRanch(x, dump_label = TRUE)
   trmts <- c(-1, 0, 1, 2, 3, 5, 10)
   c2mm_HWRanch(trmts)
   c2mm_HWRanch(trmts, dump_label = TRUE)
   # ~~~~~~
   scale_c <- scale(trmts)
   scale_mm <- scale(c2mm_HWRanch(trmts))
   # these are the calculated (from tipping bucket) mean event sizes
   scale_mm_mean <- scale(c(4.76, 5.31, 6.18, 7.21, 8.38, 10.82, 19.42))
   # when scaled, the different ways of representing the treatments
   # follow nearly a 1:1 relationship
   par(mfrow = c(2, 2))
   plot(scale_mm ~ scale_c); abline(a = 0, b = 1)
   plot(scale_mm_mean ~ scale_c); abline(a = 0, b = 1)
   plot(scale_mm_mean ~ scale_mm); abline(a = 0, b = 1)
   # ~~~~~~~~
   
   c2mm_HWRanch(c(-1, 2.5, 3)) # shouldn't run
   trmts_HWRanch(1:14, convert2mm = TRUE)
}


# c2mm_HWRanch applied to dfs ------------------------------------------------

c2mm_df_HWRanch <- function(df) {
    # args:
    #   df--dataframe containing trmt variable
    # returns:
    #   df with trmt converted from C to mm
    stopifnot(
        is.data.frame(df),
        "trmt" %in% names(df)
    )
    df$trmt <- c2mm_HWRanch(df$trmt)
    
    df
}

#test
if (FALSE) {
    df <- data.frame(trmt = trmts_HWRanch(1:14), plot = 1:14)
    df
    c2mm_df_HWRanch(df)
}


# lohi function factory ---------------------------------------------------

lohi_factory <- function(f) {
  # f--either trmts_clark or trmts_hwranch
  new_fun <- function(x) {
    # args:
    #   x--vector of plot numbers
    # returns:
    #   factor of low or high intensity (trmt)
    stopifnot(
      is.numeric(x),
      all(x %in% 1:14)
    )
    
    trmt <- as.character(f(x)) # convert to trmt (deg C)
    trmt <- ifelse(trmt == "cc", "0", trmt)
    trmt <- as.numeric(as.character(trmt))
    trmt_lohi = cut(trmt, c(-1.5, 2.5, 10.5),
                    labels = c("low intensity", "high intensity"))
    trmt_lohi
  }
  new_fun
}

# hi lo intensity hardware ------------------------------------------------


lohi_HWRanch <- lohi_factory(trmts_HWRanch)

if (FALSE) {
    lohi_HWRanch(1:14)
    lohi_HWRanch(c(1, 2, 3, 4, 6, 7, 9, 10, 11))
    lohi_HWRanch(c(5, 8, 12, 13, 14))
}


# hi lo intensity clark  ------------------------------------------------

# grouping plots into two categories, low precip intensity and high precip
# intensity
lohi_clark <- lohi_factory(trmts_clark)

if (FALSE) {
  lohi_clark(1:14)
}

# c 2 mm trmts clark ------------------------------------------------------

# convert trmt levels from deg C to mm 
# treatments labels can be in deg C, ie the temperature increase
# the precip intensity trmt corresponds to.
# this function converts that into mm (ie event size)
c2mm_clark <- function(x, dump_label = FALSE) {
    # args:
    #   x--vector of treatment levels (old deg C labeling)
    #   return labels of the dump size instead
    # returns:
    #   vector of treatment levels converted to mm precip (mean event sizes)
    #       or trmt label (dump size, mm)
    
    stopifnot(
        is.logical(dump_label)
    )
    
    x_char <- as.character(x)
    
    if (!dump_label) {
        # mean event sizes (from tipping bucket applied to non-winter months)
        # during experiment time only
        lookup_nocc <- c("-1" = 4.9, "0" = 5.6, "1" = 6.9, "2" = 8.3, "3" = 9.1,
                         "5" = 11.5,  "10" = 20.2)
    } else {
        lookup_nocc <- c("-1" = "1 mm", "0" = 'Control', "1" = "2 mm", 
                         "2" = "3 mm", "3" = "4 mm", "5" = "8 mm", 
                         "10" = "18 mm")
    }
    
    if(is.numeric(x)){
        stopifnot(all(x %in% c(-1, 0, 1, 2, 3, 5, 10)))
        out <- lookup_nocc[x_char]
        
    } else if(is.factor(x)) {
        stopifnot(
            all(x_char %in% as.character(trmts_clark(1:14)))
        )
        lookup_all <- c("cc" = "cc", lookup_nocc)
        lookup_all <- factor(lookup_all, levels = unname(lookup_all), ordered = TRUE)
        out <- lookup_all[x_char]
    } else {
        stop("x must be numeric or factor")
    }
    out
}

# test
if (FALSE) {
    x <- trmts_clark(1:14) 
    c2mm_clark(x)
    c2mm_clark(x, dump_label = TRUE)
    trmts <- c(-1, 0, 1, 2, 3, 5, 10)
    c2mm_clark(trmts)
    c2mm_clark(trmts, dump_label = TRUE)
    # ~~~~~~~~
    c2mm_clark(c(-1, 2.5, 3)) # shouldn't run
    trmts_clark(1:14, convert2mm = TRUE)
}
