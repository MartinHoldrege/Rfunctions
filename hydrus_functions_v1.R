# Martin Holdrege

# script started: 6/9/18

# edited on: 6/20/18, 6/21/18

# functions for working hydrus 1D output files etc. 

library(tidyverse)
library(lubridate)


# reading in Obs_Node file ------------------------------------------------

# the following function is adapted from read.obs_node function from the 
# hydrusR package. 

read_obs_node <- function(project.path, out.file = "Obs_Node.out") {
    
    obs_node_out = data.table::fread(input = file.path(project.path, out.file),
                                     fill = TRUE, blank.lines.skip = FALSE)
    
    nodes <- obs_node_out[9,] %>% 
        str_c(collapse = "") %>% 
        str_extract_all("\\d+") # extracting node names
    nodes <- nodes[[1]]
    
    cols <- obs_node_out[11,] %>% 
        str_c(str_c(collapse = ",")) 
    nodes <- rep(nodes, each = 3)
    col_names <- paste(cols[-1], nodes, sep = "_")
    col_names <- c(cols[1], col_names)
    
    obs_node_out = obs_node_out[-c(1:11, nrow(obs_node_out)), ]
    names(obs_node_out) <- col_names
    
    obs_node_out <- as.tibble(obs_node_out) %>% 
        mutate_all(parse_guess)
    
    obs_node_out
}


# gather_obs_node ---------------------------------------------------------

# function takes input from read_obs_node (ie hydrus Obs_node object),
# makes it into long form with one column each for depth, pressure head, vwc,flux

gather_obs_node <- function(x){
    cols <- names(x)
    x_long1 <- x %>% 
        gather(key = "key", value = "value", cols[-1]) %>% 
        mutate(
            var = str_extract(key, "^[a-z|A-Z]+"),
            depth = str_extract(key, "\\d+$")
        ) %>% 
        select(-key) %>% 
        spread(key = var, value = value) %>% 
        mutate(
            depth = as.numeric(depth),
            depth = as.factor(depth)
        )
    
    x_long2 <- x_long1 %>% 
        rename(pressure_head = h, vwc = theta) 
    
    names(x_long2) <- names(x_long2) %>% str_to_lower()
    
    x_long2
} 

#  ------------------------------------------------------------------------

# make time vector for hydrus (ie time is in hours 1-n, with accompanying date/time)
# vector so the data can be merged with observed data. 

hydrus_time_vec <- function(start_date_time = "2017-01-01 0:00", days_length = 365){
    num_hours <- days_length*24 # tot num hours
    hours <- 0:(num_hours-1) # hour count, 
    
    
    date_time <- tibble(date_time = ymd_hm(start_date_time)+ 60*60*hours, 
                        time = hours + 1)
    date_time
}

# hydrus_time_vec(start_date_time = "2017-01-01 0:00")


# read Fit.IN beginning ---------------------------------------------------

# reads in the beginning section of the Fit.IN file (i.e. not the stuff
# like soil moisture, that is provided seperately for inverse solution)
# to be used inside subsequent functions


read_fit_in_start <- function(project.path){
    file.profile.dat <-  file.path(project.path, "Fit.IN")
    data1 <-  readLines(con = file.profile.dat, n = -1L, 
                        encoding = "unknown")
    row_num <- str_detect(data1, "HO\\(N\\)") %>% 
        which() # last row of the intro data
    
    intro <- data1[1:(row_num)]
    
    last <- data1[length(data1)] # last element
    
    out <- c(intro, last)
    out
}


# change NOBB in Fit.IN file ----------------------------------------------

# function to be used in subsequent function
# changes the NOBB value (number of datapoints going into inverse solution)
# based on the data frame going into it. 

NOBB_change <- function(data_vec, fit_in_start){
    if(! is.vector(data_vec)) stop("NOBB_change function requires vector input")
    row <- str_detect(fit_in_start, "NOBB") %>% which() # row with NOBB header
    
    if(length(row)!= 1) stop("Fit.in should only have one NOBB header")
    
    nobb_row <- fit_in_start[(row + 1)] # row with NOBB data value
    
    old_nobb <- str_extract(nobb_row, "^\\s+\\d+") # nobb value
    
    n_data <- length(data_vec) %>% as.character()
    n_char <- nchar(old_nobb)
    
    new_nobb <- str_pad(n_data, width = n_char, side = "left")
    
    fit_in_start[(row + 1)] <- paste0(new_nobb,
        str_sub(nobb_row, start = (n_char + 1), end = nchar(nobb_row))
        ) # replacing row with new nobb value included
    
    fit_in_start
}

# testing

# fit_in_start <- read_fit_in_start(project.path = "hydrus_files/HW2017_v3.1_copy")
# data_vec <- data_file1


# write Fit.IN file -------------------------------------------------------

# function for writing the Fit.IN file. When doing inverse solution and need
# to input more data point than the GUI can handle (10,000)

# takes a data frame with the proper columns, x (time), y (data, ie soil moisture)
# type of data (1, 2, 3 etc), and weights, give col names if they don't match
# the default names
# provide the path the folder in which the hydrus files are. 

write_fit_in <- function(df, project.path, x = "x", y = "y", type = "type",
                                position = "position", weights = "weights",
                                round_y = 2){
    
    names_df <- names(df)
    
    if(!all(c(x, y, type, position, weights) %in% names_df)){
        stop("column names not in dataframe, specify correct column names")
    }

    x_vec <- str_pad(df[[x]], width = 12, side = "left")
    y_vec <- round(df[[y]], round_y) %>% 
        str_pad(width = 13,  side = "left")
    type_vec <- str_pad(df[[type]], width = 6,  side = "left")
    position_vec <- str_pad(df[[position]], width = 6,  side = "left")
    weights_vec <- str_pad(df[[weights]], width = 9,  side = "left")
    

    data_file1 <- str_c(x_vec, y_vec, type_vec, position_vec, weights_vec,
                  sep = "")

    intro <- read_fit_in_start(project.path) # reading/keeping soil characteristic
    # info from the existing Fit.IN file. 
    
    intro <- NOBB_change(data_vec = data_file1, fit_in_start = intro) # updating
    # num of data values hydrus will use for inverse solution
    
    out <- c(intro[1:(length(intro)-1)], data_file1, intro[length(intro)])
    
    out_path <- file.path(project.path, "Fit.IN")
    
    sink(out_path, type="output")
    writeLines(out)
    sink()
    
    message(paste("in hydrus number of data points in objective function is:",
                  length(data_file1)))
}



# funcion testing

# project.path <- "hydrus_files/HW2017_v3.2_ambient"
# df <- hw1_2017b.a[1:16000,]
# names(df) <- c("x", "y", "type", "position", "weights")
# 
# x = "time"; y = "pressure_head"; type = "type"; position = "sensor"; weights = "weight"
# x = "x"; y = "y"; type = "type"; position = "position"; weights = "weights"
# 
# write_fit_in(df = df, project.path = "hydrus_files/HW2017_v3.1_copy")


# calculate hCritA --------------------------------------------------------

# function for calculating hCritA from RH and temp (C).
calc_hCritA <- function(Hr, temp){
    tempK <- temp + 273.15
    
    h <-  log(Hr)*(8.314*tempK)/(0.018015*9.81)
    hcm <- h*100 # convert to cm
    hcm
}

# calc_hCritA(.4, 20)


# convert pressure head to wp ---------------------------------------------

h_to_wp <- function(x){
    x*9.80665*10^(-5)
}
