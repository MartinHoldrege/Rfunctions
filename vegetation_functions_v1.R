# Martin Holdrege

# script started: 5/28/18

# edited on:

# function useful for working with vegetation data

#~~~~~~~~~~~~~~~~~~~~~~~~~
jaccards=function(x,y){
    #x and y are vectors of spp abundances
    #they must be the same length!
    if(length(x)!=length(y)) stop("Bad abundances!")
    # get number of species present at both sites
    a = length(which(x>0 & y>0))
    # get number of species present at first but not second site
    b = length(which(x>0 & y==0))
    # get number of species present at second but not first site
    c = length(which(x==0 & y>0))
    out = a/(a+b+c)
    out
} #jaccard's similarity
#~~~~~~~~~~~~~~~~~~~~~~~~
ED = function(x,y){
    #x and y are vectors of spp abundances
    #they must be the same length!
    if(length(x)!=length(y)) stop("Bad abundances!")
    out =  sqrt(sum((x-y)^2))
    out
} # Euclidean distance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BC= function(x,y){
    #x and y are vectors of spp abundances
    #they must be the same length!
    if(length(x)!=length(y)) stop("Bad abundances!")
    out =  sum(abs(x-y))/sum(x+y)
    out
} #Bray Curtis 