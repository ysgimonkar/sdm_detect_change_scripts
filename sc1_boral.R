# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(boral)
library(lubridate)
library(pROC)
library(parallel)
library(caret)

# Loading data
load("sc1_simulations.RData")

bor_dat <- function(data){
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(boral)
  library(lubridate)
  library(pROC)
  library(parallel)
  library(caret)
  
  training.samples <- data$sst %>%
    createDataPartition(p = 0.75, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]   
  
  # select which data
  
  pa <- train.data[, c(1:18)]
  en <- train.data[, c(22:24)]
  
  # model fit
  
  T1 <- Sys.time()
  
  md_fit <- boral::boral(y = pa, 
                         X = en, 
                         formula.X = stats::as.formula(paste0('cbind(',paste(paste0('spp',1:18), collapse =','),")~ poly(sst, degree=2, raw=TRUE) + dsStart + chla")), 
                         save.model = TRUE, model.name = "simulation_jagsboralmodel.txt",
                         family = "binomial", 
                         lv.control = list(num.lv = 2),
                         prior.control = list(type=c("normal","normal", "normal","uniform"),hypparams = c(10, 10, 10, 30)),
                         mcmc.control = list(n.burnin = 10000, n.iteration = 40000, n.thin = 30))
  
  T2 <- Sys.time()
  tm <- difftime(T2,T1)
  
  # model prediction
  
  test.data <- add_column(test.data, sst2 = (test.data$sst)^2, .after = "sst")
  
  boral_pred <- boral::predict.boral(md_fit, newX = test.data[, c(22:25)], scale = "response",
                                     est = "mean", predict.type = "marginal")
  bp <- boral_pred$linpred
  colnames(bp) <- rownames(md_fit[["X.coefs.mean"]])
  
  # RMSE
  
  Y = test.data[1:18]
  predY <- bp
  
  computeRMSE = function(Y, predY){
    ns = dim(Y)[2]
    RMSE = rep(NA,ns)
    for (j in 1:ns){
      RMSE[j] = sqrt(mean((Y[,j]-predY[,j])^2, na.rm=TRUE))
    }
    return(RMSE)
  }
  rmse <- computeRMSE(Y, predY) 
  
  # TJur R2
  
  computeTjurR2 = function(Y, predY){
    ns = dim(Y)[2]
    R2 = rep(NA, ns)
    for (j in 1:ns) {
      R2[j] = mean(predY[which(Y[, j] == 1), j]) - mean(predY[which(Y[,j] == 0), j])
    }
    return(R2)
  }
  tjur <- computeTjurR2(Y, predY)
  
  # AUC
  
  computeAUC = function(Y, predY){
    ns = dim(Y)[2]
    AUC = rep(NA,ns)
    ## take care that Y has only levels {0,1} as specified in auc() below
    Y <- ifelse(Y > 0, 1, 0)
    for (j in 1:ns){
      sel = !is.na(Y[,j])
      if(length(unique(Y[sel,j]))==2)
        AUC[j] = pROC::auc(Y[sel,j],predY[sel,j], levels=c(0,1),direction="<")
    }
    return(AUC)
  }
  au <- computeAUC(Y, predY)
  
  # Combine all lists into one list
  
  par <- list()
  par[[1]] <- md_fit
  par[[2]] <- tm
  par[[3]] <- bp
  par[[4]] <- rmse
  par[[5]] <- tjur
  par[[6]] <- au
  
  names(par) <- c("boral_fit", "boral_time", "boral_predictions", "boral_RMSE", "boral_TjurR2", "boral_AUC")
  
  rm(list = c("pa", "en", "T1", "md_fit", "T2", "tm", "boral_pred", "bp", 
              "Y", "predY", "computeRMSE", "computeTjurR2", "computeAUC", "rmse", "tjur", "au", 
              "test.data", "train.data", "training.samples"))
  
  return(par)
}

cl <- parallel::makeCluster(detectCores())

boral_par <- parallel::parLapply(cl, 
                                 simulation1,
                                 bor_dat)

parallel::stopCluster(cl)

# save(boral_par, file = "sc1_boral.RData")



