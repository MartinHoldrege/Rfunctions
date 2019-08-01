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
    #   vector of treatment levels converted to mm precip

    x_char <- as.character(x)
    
    lookup_nocc <- c("-1" = 1, "0" = 1.5, "1" = 2, "2" = 3, "3" = 4, "5" = 8, 
                    "10" = 18)
    
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
   c2mm_HWRanch(c(-1, 0, 1, 2, 3, 5, 10))
   c2mm_HWRanch(c(-1, 2.5, 3)) # shouldn't run
   trmts_HWRanch(1:14, convert2mm = TRUE)
}
