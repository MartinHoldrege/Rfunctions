# Martin Holdrege

# script started on 2/23/18

# edited on: 6/10/18

# functions for working with 229 data (ie going from raw T values to WP etc) 


# delta T norm ------------------------------------------------------------


deltaT_norm <- function(deltaT, Tdry, Twet, Ti, iterations = 10, tolerance = 0.001){
    #following iterative process described in Flint et al 2002
    #step 1
    Ts0 <- (Tdry-deltaT)/(Tdry-Twet) # T_star equation 2 in flint et al 2002
    #step 2, initial s star estimate
    s_star <- -0.0133*Ts0^5 + 0.0559*Ts0^4 - 0.0747*Ts0^3 + 0.0203*Ts0^2 +
            0.011*Ts0 + 0.0013
    n <- 0
    change <- 2
    while (sum(n<iterations, change > tolerance,na.rm=T, na.rm = T) == 2){
        n <- n + 1 # so loop can't run inifinitely if doesn't "converge"
        #step 3--new estimate of T star, ie temperature correction
        Ts <- Ts0 - s_star*(Ti - 20) #where 20 is the calibration temperature
        # step 4 --new estimate of s*
        s_star <- -0.0133*Ts^5 + 0.0559*Ts^4 - 0.0747*Ts^3 + 0.0203*Ts^2 +
            0.011*Ts + 0.0013
        change <- abs(Ts0-Ts)
        Ts #getting ready for anoth run through loop
    }
    Ts
} #delta_T is uncorrected deltat T, T_dry and T_wet are the delta Ts for max wet and dry (lab measured), Ti is starting temp 


# scaled delta T norm -----------------------------------------------------


deltaT_norm_s <- function(x){
    wp0 <-  -0.056 
    m <-  0.45
    n <- 1
    wp <- -1000 # 
    T1000 <- (wp^n/wp0^n+1)^(-m) #value of delta T norm when wp is -1000
    Tscaled <- x*(1-T1000)+T1000 # scaled delta T norm
    Tscaled
} #scaled T_norm also based on an equation from Flint et al 2002, input to function is delta T norm  



# water potential from delta T norm ---------------------------------------


wp_flint <- function(x, wp0 = -0.056, m = 0.45, n = 1){
    wp <- wp0*(x^(-1/m)-1)^(1/n)
    wp
} #formulat for calculating matric water potential (Mpa) from flint et al 2002


# function testing --------------------------------------------------------


#function testing:

# deltaT_norm(deltaT =  3.215, Tdry = 3.216, 
#             Twet = 0.822, Ti = 19, 
#             iterations = 10, tolerance = 0.001)
# 
# deltaT_norm(deltaT = NA, Tdry = 3.216, 
#             Twet = 0.822, Ti = NA, 
#             iterations = 10, tolerance = 0.01)
# 
# tnorm <- seq(0,1, length.out = 1000)
# tnorm_scaled <- deltaT_norm_s(tnorm)
# 
# wp <- wp_flint(tnorm)
# wp_s <- wp_flint(tnorm_scaled)


#par(mfrow = c(1,1))
#plot(log10(-wp)~tnorm, type = "l")
#lines(log10(-wp_s)~tnorm, type = "l", col = "blue")


# calculating VWC from WP -------------------------------------------------

# vwc calculated from water potential
# take WP (in MPa) and depth (cm) as input. based on soil characteristic curves
# from the site. 

vwc_hw <- function(x, depth){
    x <- -1*x
    vwc <- ifelse(depth < 15, log(x/8358.8)/(-48.4),
                  ifelse(depth >= 15 & depth < 30, log(x/15499)/(-48.81),
                         log(x/8627.4)/(-48.19)))
    vwc
}

# test

# depths <- c(10, 20, 70)
# for(i in seq_along(depths)){
#     print(depths[i])
#     print(vwc_hw(-0.033, depths[i]))
#     print(vwc_hw(-1.5, depths[i]))
# }

