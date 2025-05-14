# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ecomix)
library(lubridate)
library(pROC)
library(parallel)
library(caret)

# Loading data

load("sc1_simulations.RData")

sam <- function(data){
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ecomix)
  library(lubridate)
  library(pROC)
  library(parallel)
  library(caret)
  
  training.samples <- data$sst %>%
    createDataPartition(p = 0.75, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]
  
  a <- list()
  
  # model fit
  archetype_formula <- as.formula(paste0(paste0('cbind(',paste(colnames(train.data)[grep("spp",colnames(train.data))],
                                                               collapse = ", "),")~ poly(sst, degree=2, raw=TRUE) + dsStart + chla")))
  
  species_formula <- ~ 1
  
  T1 <- Sys.time()
  a[[1]] <- ecomix::species_mix(archetype_formula = archetype_formula, 
                                species_formula = species_formula,    
                                data = train.data,            
                                nArchetypes = 4,        
                                family = 'bernoulli',       
                                control = list(quiet = TRUE))
  T2 <- Sys.time()
  a[[2]] <- difftime(T2,T1)
  
  # model predictions from test data
  
  env_data <- test.data[, 22:24]
  
  a[[3]] <- predict(a[[1]], newdata = env_data, prediction.type = "species", type = "response")
  
  # RMSE
  
  Y = test.data[,1:18]
  predY <- a[[3]]
  
  computeRMSE = function(Y, predY){
    ns = dim(Y)[2]
    RMSE = rep(NA,ns)
    for (i in 1:ns){
      RMSE[i] = sqrt(mean((Y[, i]-predY[, i])^2, na.rm=TRUE))
    }
    return(RMSE)
  }
  
  a[[4]] <- computeRMSE(Y, predY)
  
  # TJur R2
  
  computeTjurR2 = function(Y, predY) {
    ns = dim(Y)[2]
    R2 = rep(NA, ns)
    for (i in 1:ns) {
      R2[i] = mean(predY[which(Y[, i] == 1), i]) - mean(predY[which(Y[,i] == 0), i])
    }
    return(R2)
  }
  
  a[[5]] <- computeTjurR2(Y, predY)
  
  # AUC
  
  computeAUC = function(Y, predY){
    ns = dim(Y)[2]
    AUC = rep(NA,ns)
    ## take care that Y has only levels {0,1} as specified in auc() below
    Y <- ifelse(Y > 0, 1, 0)
    for (i in 1:ns){
      sel = !is.na(Y[,i])
      if(length(unique(Y[sel,i]))==2)
        AUC[i] = pROC::auc(Y[sel,i],predY[sel,i], levels=c(0,1),direction="<")
    }
    return(AUC)
  }
  
  a[[6]] <- computeAUC(Y, predY)
  
  # SAM Bootstrap for Partial effects plot
  eff.df <- ecomix::effectPlotData(focal.predictors = c("sst", "dsStart", "chla"), a[[1]], ngrid = 20)
  
  a[[7]] <- ecomix::species_mix.bootstrap(a[[1]], nboot = 10, quiet = TRUE)
  
  names(a) <- c("sam_fit", "sam_time", "sam_predictions", 
                "sam_RMSE", "sam_TjurR2", "sam_AUC", "sam_bootstrap")
  
  rm(list = c("archetype_formula", "species_formula", "T1", "T2", "env_data",
              "Y", "predY", "computeRMSE", "computeTjurR2", "computeAUC", "eff.df",
              "test.data", "train.data", "training.samples"))
  
  return(a)
}

cl <- parallel::makeCluster(detectCores())

sam_par <- parallel::parLapply(cl, 
                               simulation1,
                               sam)

parallel::stopCluster(cl)

# save(sam_par, file = "Chapter-1/Scenario 1/Data/sc1_sam.RData")
