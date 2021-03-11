# Martin Holdrege

# Script started 1/18/2021

# This script contains functions(and some constants) used in other scripts on the
# KNB repository that contains data/code of the winter wheat study at the clarkston
# field site. 


# figure parameters -------------------------------------------------------

# colors used for figures in other scripts
pal <- rev(RColorBrewer::brewer.pal(11, "RdYlBu"))
pal <- pal[-c(1:2, 4:5)]
pal[2] <- "black"


# base for gamm figures (lai, ndvi etc)

# base for pub qual figs
width1 <- 5 # fig height
height1 = 3 # fig height
pub_base <- function() {
  # x axis labels
  dates <- paste0("2017-", 5:8, "-1") # first day of month
  doys <- yday(ymd(dates))
  date_labels <- month(dates, label = TRUE, abbr = TRUE)
  
  out <- list(facet_wrap(~year),
              scale_color_manual(values =  pal[c(1, 6)]),
              scale_fill_manual(values =  pal[c(1, 6)]),
              theme(legend.position = "top",
                    legend.title = element_blank()),
              theme_classic(),
              scale_x_continuous(breaks = doys, labels = date_labels),
              theme(legend.title = element_blank(),
                    legend.position = "top")
  )
  
  out
}

# make df for gamm() ------------------------------------------------------

# used to create dataframes for predict() function and model
# objects created by mgcv::gamm()

continuous_df <- function(df, trmt_var = NULL) {
  # args:
  #   df--dataframe with date column
  #   trmt_var --string name of trmt variable
  # returns:
  #   data frame with complete sequence of date with each combo of trmt_var
  #   (for predict() with gamm objects)
  stopifnot(c("date", trmt_var) %in% names(df))
  
  dates <- seq(from = min(df$date), to = max(df$date), by = 1)
  
  if(is.null(trmt_var)) {
    out <- tibble(date = dates,
                  doy = lubridate::yday(date))
  } else {
    out <- expand_grid(
      date = dates,  
      !!trmt_var := factor(df[[trmt_var]], levels(df[[trmt_var]]))) %>% 
      mutate(doy = lubridate::yday(date))
  }
  out
}


# ictab to tibble ---------------------------------------------------------


ICtab2df <- function(table, mod_lookup) {
  # args:
  #   table: object from bbmle::ICtab() function
  #   named lookup vector to replace row.names with better model names
  # returns:
  #   tibble version of ictab table, with delta loglik re-computed also
  stopifnot(
    class(table) == "ICtab",
    length(attr(table, "row.names")) == length(mod_lookup)
  )
  table2 <- table
  class(table2) <- "list"
  table3 <- table2 %>% 
    as_tibble() %>% 
    mutate(mod = attr(table, "row.names"),
           # dloglike makes more sense as difference from best
           dLogLik = max(logLik)-logLik) 
  
  table4 <- table3 %>% 
    mutate(Model = mod_lookup[mod],
           Weight = ifelse(weight < 0.001,
                           "<0.001",
                           weight)) %>% 
    dplyr::select(Model, everything(), -mod, -weight)
  table4
}

# gamm preds --------------------------------------------------------------

# predicted values from GAMM models

gamm_preds <- function(gamm_list, df_list, mod = "lohi") {
  # args:
  #   gamm_list--list of list of gamms
  #   df_list list of dfs (same length as outer gamm_list list)
  #   mod--string, name of element in gamm_list to use 
  # returns:
  #   list of dfs with model predictions
  preds <- map2(gamm_list, df_list, function(x, df) {
    x2 <- x[[mod]] # extract model object
    out <- df
    pred <- predict(x2$gam, df, se.fit = TRUE)
    out[[paste0("yhat_", mod)]] <- pred$fit
    out[[paste0(mod, "_u.ci")]] <- pred$fit + 2*pred$se.fit
    out[[paste0(mod, "_l.ci")]] <- pred$fit - 2*pred$se.fit
    out
  })
  preds
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

# trmts_clark -------------------------------------------------------------

#creating vector of the trmts based on vector of plot numbers at clarkston site
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

#  no cc  --------------------------------------------------------------------

# function that discards the "cc" (control-control, ie,
# shelter-control) plots from a data frame and converts
# trmt back to numeric

nocc <- function(df, trmt = "trmt"){
  
  df <- df[df[[trmt]]!= "cc" & df[[trmt]]!= "CC",]
  df[[trmt]] <- as.numeric(as.character(df[[trmt]]))
  df
}


# vwc from wp --------------------------------------------------------
# this function is based on site specific soil water curves, and converts
# wp to vwc

vwc_clark <- function(x, depth){
  # args:
  #   x --numeric vector (water potential)
  #   depth --numeric vector (depth, in cm)
  # returns:
  #   volumetric water content
  x <- -1*x
  vwc <- ifelse(depth < 15, 
                log(x/3916.6)/(-43.89),
                ifelse(depth >= 15 & depth < 30, log(x/6587.2)/(-48.64),
                       log(x/16451)/(-53.69)))
  vwc
}
