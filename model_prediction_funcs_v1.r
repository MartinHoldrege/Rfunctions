# martin holdrege

# code started: 4/24/18


# preds_trmt function developed to output model predictions (originally developed)
# for glm.nb models of germination data

# mu_and_CI function adds model predictions and CI to figure, as well as the p-value. 

# also used for model validation

# mod test
# mod <- nb1
# data <- g_inc_nc
# other_var <- "species"
# group <- "species"
#~~

#  ------------------------------------------------------------------------

preds_trmt <- function(mod, data, other_var = NULL){
    df <- data.frame(trmt = seq_range(data$trmt, n = 100))
    df <- if(is.null(other_var)){
        df
    } else {
        df <- expand.grid(trmt = df$trmt, other_var = unique(data[[other_var]]))
        names(df)[names(df)== "other_var"] <- other_var
        df
    } # the if else takes care of models w/ just trmt or trmt and e.g. species
    
    mu <- predict(mod, df, type = "response", se.fit = T) # model predictions
    df %>% 
        mutate(
            mu_fit = mu$fit,
            mu_se =  mu$se.fit,
            LL = mu_fit - 1.96*mu_se, # lower 95% confidence limit
            UL = mu_fit + 1.96*mu_se
        )
} # function that spits out the predictions and 95% CI of the model with only trmt as predictor
# or including "other_var" (ie species or trmt





#  ------------------------------------------------------------------------

mu_and_CI <- function(plot, preds, mod, group = NULL, display_p = T){
    p <- as.data.frame(mod)
    p <- signif(p[2, "Pr(Chi)"],2) # p value from likelhood ratio test
    label <- preds %>% 
        summarize(
            x = max(trmt),
            y = max(UL),
            label = paste("p = ",p, "(likelihood ratio test, testing treatment effect)")
        ) # making label for figure
    text <- if(display_p == F){
        NULL
        } else {
            geom_text(data = label, aes(x = x, y = y, label = label),
                          vjust = "top", hjust = "right", inherit.aes = F)
        }
    plot+
        geom_ribbon(data = preds, aes_string("trmt", ymin = "LL", ymax = "UL", 
                                             group = group),alpha = 0.2, inherit.aes = F)+ # CI
        geom_line(data = preds, aes_string("trmt", "mu_fit", group = group))+
        coord_cartesian(ylim = c(min(x$LL), max(x$UL)))+
        text # adding p value to graph
        
    # change function to reflect issue of also having a categorical variable
    
} # adds prediction and confidence interval from model to a plot, uses output from preds_trmt.
# also requires model object (mod--the LRT) and group (if applicable)


#  ------------------------------------------------------------------------

# predict and residuals ---------------------------------------------------

pred_resids <- function(model, data) {
  # args:
  #     model object (created for glmer others may be ok)
  #     data--df used to fit the model
  # returns:
  #     df with predictions and residuals and 
  data %>% 
    filter(complete.cases(.)) %>% 
    mutate(
      p.link = predict(model, type = "link"),
      p.response = predict(model, type = "response"),
      r.pearson = residuals(model, type = "pearson"),
      r.deviance = residuals(model, type = "deviance")
    )
}

# residuals vs prediction by factor ---------------------------------------

rvpred_byfactor <- function(model, data, ran, fixed = NULL, 
                       x = "p.link", y = "r.deviance", caption = NULL) {
  # args:
  #     model object (created for glmer others may be ok)
  #     data--df used to fit the model
  #     ran--random effects (variable names) to facet_wrap (character vector) (or null)
  #     fixed--fixed effects (variable names) to facet_wrap (character vector)
  #       needs to be categorical not continuous
  #     x--p.link or p.response (predicted value on response or link scale)
  #     y--r.deviance or r.pearson (type of residual)
  #     caption --plot caption
  # returns:
  #     plots of residual vs predicted split faceted by ran/fixed effects. 
  
  df <- pred_resids(model, data) # residuals and predictions
  
  # base model object:
  g1 <- ggplot(df, aes_string(x, y)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_classic() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = paste0("fitted value", " (", x,")"),
         y = paste0("residual", " (", y,")"),
         caption = caption)
  # residual vs prediction by levels of random effects
  ran_plot <- NULL
  if (!is.null(ran)){

    ran_l <- as.list(ran)
    
    # plots for each random effect level as per Harrison et al. 2018 (PeerJ)
    ran_plot <- lapply(ran_l, function(ran){
      g1 +
        facet_wrap(as.formula(paste("~", ran))) +
        labs(subtitle = paste("Random effect:", ran),
             title = "residuals vs fitted values by level of random effect")
    })
  }
  
  # plotting resid vs pred by levels of fixed effects
  fixed_plot <- NULL
  if (!is.null(fixed)){
    
    fixed_l <- as.list(fixed)
    
    # plots for each fixed effect level. 
    fixed_plot <- lapply(fixed_l, function(fixed){
      g1 +
        facet_wrap(as.formula(paste("~", fixed))) +
        labs(subtitle = paste("Fixed effect:", fixed),
             title = "residuals vs fitted values by level of a fixed effect")
    })
  }
  
  return(list(ran_plot, fixed_plot))
}


# predict on new data -----------------------------------------------------

predict_new <- function(model, df_scale, df_org,...) {
  # args:
  #     model--model object
  #     df_scale--df to predict on (scaled in way used when fitting mod)
  #     df_org--comparable to df_scale but df on original scale of data
  #     ... -- other args to pass onto predict()
  # returns:
  #     df on original data scale with predicted values (link and response)
  df_org$p.response <-  predict(model, df_scale, type = "response",...)
  df_org$p.link <-  predict(model, df_scale, type = "link",...)
  df_org
}

# log transform data ---------------------------------------------------------

log_mult <- function(x, mult = 0.5) {
  # args:
  #   x--numeric vector
  #   mult--constant (rule of thumb is according to SD is 0.5)
  # returns:
  #   log of x + mult X min non-zero value. for when x contains 0. 
  xna <- x[!is.na(x)]
  if(min(xna) < 0) stop("x can't be negative")
  
  min_non0 <- min(xna[xna > 0]) # minimum non zero value
  min_mult <- mult*min_non0 # min value multipled by constant
  out <- log(x + min_mult)
  out
}

# testing:
# x <- c(0, NA, .5, 100, 2)
# log_mult(x)
# log(x + 0.25)
