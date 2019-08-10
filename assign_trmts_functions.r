#Martin Holdrege
#functions for assigning the correct treatments to plot numbers at Hardware Ranch
#and clarkston.

#creating vector of the trmts based on vector of plot numbers at clarkston
trmts_clark <- function(x){
    stopifnot(is.numeric(x),
              all(x %in% 1:14))
    lookup <- c("1"=3, "2"=1,"3"=3,"4"=0,"5"=0,"6"="cc","7"=-1,"8"=2,
                "9"=5,"10"="cc","11"="cc","12"=10,"13"=0,"14"=3)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    unname(lookup[x])
 }

#creating vector of the trmts based on vector of plot numbers at Hardware
trmts_HWRanch <- function(x, convert2mm = FALSE){
    stopifnot(is.numeric(x),
              all(x %in% 1:14))
    lookup <- c("1"="cc", "2"=0,"3"=1,"4"=0,"5"=3,"6"="cc","7"=-1,"8"=3,
                             "9"=0,"10"="cc","11"="2","12"=5,"13"=3,"14"=10)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    out <- unname(lookup[x])
    if (convert2mm) {
        out <- c2mm_HWRanch(out)
    }
    out
}


# convert trmt levels from deg C to mm ------------------------------------


c2mm_HWRanch <- function(x) {
    # args:
    #   x--vector of treatment levels (old deg C labeling)
    # returns:
    #   vector of treatment levels converted to mm precip (numbers from AK)

    x_char <- as.character(x)
    
    lookup_nocc <- c("-1" = 3.9, "0" = 4.6, "1" = 7.7, "2" = 10, "3" = 12, "5" = 17, 
                    "10" = 31)
    
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
   trmts <- c(-1, 0, 1, 2, 3, 5, 10)
   c2mm_HWRanch(trmts)
   
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
