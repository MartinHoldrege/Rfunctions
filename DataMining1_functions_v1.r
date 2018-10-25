# Martin Holdrege

# started: 9/16/18

# useful code and functions for data mining 1 course 
# taught by Richard Cutler
library(rpart)

# error rate --------------------------------------------------------------

error_rate <- function(table, signif = 4){
    n <- sum(table)
    correct <- sum(diag(table))
    wrong <- n-correct
    error <- wrong/n*100
    signif(error, signif)
} # error rate from confusion matrix (table input), given in percent

# percent correctly classified-------------------------------------------------

pcc <- function(table, signif = 4){
    100-error_rate(table, signif = signif)
} 

# logistic regression contingency table -----------------------------------

lr_cont <- function(obs, model_obj){
    table(obs, round(predict(model_obj, type = "response")))
} # vector of observed and model object as input


# kappa (logistic regression) ---------------------------------------------
# function from richard cutler

kappa = function(x){
    n=sum(x)
    pobs=(x[1,1]+x[2,2])/n
    pexp=(sum(x[1,])*sum(x[,1])+sum(x[2,])*sum(x[,2]))/n^2
    kappa=(pobs-pexp)/(1-pexp)
    t1=0
    t2=0
    t3=0
    pii=x/n
    pidot=apply(pii,1,sum)
    pdotj=apply(pii,2,sum)
    for(i in 1:2){
        t1 = t1 + pii[i,i]*((1-pexp) - (1-pobs)*(pidot[i]+pdotj[i]))^2
    }
    t2 = pii[1,2]*(pdotj[1]+pidot[2])^2 + pii[2,1]*(pdotj[2] + pidot[1])^2
    t3 = (pobs*pexp-2*pexp+pobs)^2
    vhat = (t1 + t2*(1-pobs)^2 -t3)/(n*(1-pexp)^4)
    se=sqrt(vhat)
    return(c(kappa,se))
}


# classification summary (logistic regression) ----------------------------
# from Richard Cutler

# takes predicted probabilities 
# AUC uses package "verification" uses function roc.area.
# classification summary. 

class.sum=function(truth,predicted){
    xt=table(truth,round(predicted+0.000001))
    pcc=round(100*sum(diag(xt))/sum(xt),2)
    spec=round(100*xt[1,1]/sum(xt[1,]),2)
    sens=round(100*xt[2,2]/sum(xt[2,]),2)
    kap=round(kappa(xt)[1],4) #MH note: I've had problems with kappa
    au=round(verification::roc.area(truth,predicted)$A,4)
    return(cbind(c("Percent Correctly Classified = ","Specificity = ","Sensitivity = ","Kappa =","AUC= "),c(pcc,spec,sens,kap,au)))
}


#  logistic regression 10-fold CV ---------------------------------------------
# adapted from code from Richard Cutler

lr_10fold <- function(df, model){
    lr.xval <- rep(0,nrow(df)) # 10-fold cross validation. 
    xvs=rep(1:10,length=nrow(df))
    xvs=sample(xvs)
    mod <- as.formula(model)
    for(i in 1:10){
        train <- df[xvs!=i,]
        test <- df[xvs==i,]
        glub <- glm(mod,family=binomial,data=train)
        lr.xval[xvs==i] <- predict(glub,test,type="response")
    }

    lr.xval 
} # df is data frame, and model is string representing the model call


# k-NN 10-fold CV ----------------------------------------------------------
# adapted from R. Cutler

knn_10fold <- function(df, model, k, posterior = FALSE){
    xval <- rep(NA_real_,nrow(df)) # 10-fold cross validation. 
    xvs=rep(1:10,length=nrow(df))
    xvs=sample(xvs)
    xval <- rep(NA_real_,nrow(df)) # 10-fold cross validation.
    mod <- as.formula(model)
    if(!posterior){
         for(i in 1:10){
            train <- df[xvs!=i,]
            test <- df[xvs==i,]
            glub <- klaR::sknn(mod, data=train, kn = k)
            xval[xvs==i] <- predict(glub,test)$class
        }
    } else {
        for(i in 1:10){
            train <- df[xvs!=i,]
            test <- df[xvs==i,]
            glub <- klaR::sknn(mod, data=train, kn = k)
            xval[xvs==i] <- predict(glub,test)$posterior[,2]
        }
    }
    xval
} # df is dataframe, model is string of model formula, k is parameter for knn
# and posterior is logical so function output will either be predicted class (if FALSE)
# or predicted probabilities. The way this is setup now posterior = TRUE will only work
# for binary predictions. 

# rpart 10-fold CV ----------------------------------------------------------
# adapted from R. Cutler
# regression/classification trees. 

rpart_10fold <- function(df, model, cp, posterior = FALSE, method = "class", 
                         character = FALSE, ...){
    xval <- rep(NA_real_,nrow(df))  
    xvs=rep(1:10,length=nrow(df))
    xvs=sample(xvs)
    xval <- rep(NA_real_,nrow(df)) 
    mod <- as.formula(model)
    if(!posterior){
        for(i in 1:10){
            train <- df[xvs!=i,]
            test <- df[xvs==i,]
            rp=rpart(mod,method = method,data = train,
                     control = rpart.control(cp = cp,...),...)
            if(character){
                xval[xvs==i] <- predict(rp, test, type = "class") %>% 
                    as.character()  
            } else {
                xval[xvs==i] <- predict(rp, test, type = "class")
            }
        } # output  is class
    } else {
        for(i in 1:10){
            train <- df[xvs!=i,]
            test <- df[xvs==i,]
            rp=rpart(mod,method= method ,data=train,
                     control=rpart.control(cp= cp), ...)
            xval[xvs==i] <- predict(rp,test, type = "prob")[,2]
        }
    } # output is predicted probability
xval
}# df is dataframe, model is string of model formula, 
# and posterior is logical so function output will either be predicted class (if FALSE)
# or predicted probabilities. The way this is setup now posterior = TRUE will only work
# for binary predictions. 
# method = class (classification tree) is defualt, cp is the parameter for tree prunning. 
# character -- if prediction should be a character vector (helpful if otherwise getting numberic factor levels as output)



