# martin holdrege

# code started: 4/24/18

# edited on: 4/25/18

# preds_trmt function developed to output model predictions (originally developed)
# for glm.nb models of germination data

# mu_and_CI function adds model predictions and CI to figure, as well as the p-value. 


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

