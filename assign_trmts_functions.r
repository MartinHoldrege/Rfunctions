#Martin Holdrege
#functions for assigning the correct treatments to plot numbers at Hardware Ranch
#and clarkston.

#creating vector of the trmts based on vector of plot numbers at clarkston
trmts_clark <- function(x){
    lookup <- c("1"=3, "2"=1,"3"=3,"4"=0,"5"=0,"6"="cc","7"=-1,"8"=2,
                "9"=5,"10"="cc","11"="cc","12"=10,"13"=0,"14"=3)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    unname(lookup[x])
 }

#creating vector of the trmts based on vector of plot numbers at Hardware
trmts_HWRanch <- function(x){
    lookup <- c("1"="cc", "2"=0,"3"=1,"4"=0,"5"=3,"6"="cc","7"=-1,"8"=3,
                             "9"=0,"10"="cc","11"="2","12"=5,"13"=3,"14"=10)
    lookup <- factor(lookup, ordered =TRUE,levels=c("cc","-1","0","1","2","3","5","10"))
    x <- as.character(x)
    unname(lookup[x])
}


