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