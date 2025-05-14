# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mistnet)
library(lubridate)
library(pROC)
library(parallel)
library(caret)

# Loading data

load("sc1_simulations.RData")

missnet <- function(data){
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(mistnet)
  library(lubridate)
  library(pROC)
  library(parallel)
  library(caret)
  
  load("mistnet_cv1.RData")
  
  training.samples <- data$sst %>%
    createDataPartition(p = 0.75, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]  
  
  x <- as.matrix(train.data[,22:24])
  y <- as.matrix(train.data[,1:18])
  
  env_vars <- c("sst", "dsStart", "chla")
  species <- c(paste0("spp", 1:18))
  
  fit <- function(x, y, hyperparams, i){
    MNet_mod = mistnet(
      x = x,
      y = y,
      layer.definitions = list(
        defineLayer(
          nonlinearity = rectify.nonlinearity(),
          size = hyperparams$n.layer1[i],
          prior = gaussian.prior(mean = 0, sd = .1)
        ),
        
        defineLayer(
          nonlinearity = sigmoid.nonlinearity(),
          size = ncol(y),
          prior = gaussian.prior(mean = 0, sd = .1)
        )
      ),
      loss=bernoulliLoss(),
      updater = adagrad.updater(learning.rate = hyperparams$learning.rate[i]),
      sampler = gaussian.sampler(ncol = hyperparams$sampler.size[i], sd = 1),
      n.importance.samples = hyperparams$n.importance.samples[i],
      n.minibatch = hyperparams$n.minibatch[i],
      training.iterations = 0,
      initialize.biases = TRUE,
      initialize.weights = TRUE
    )
    MNet_mod$layers[[1]]$biases[] = 1 # First layer biases equal 1
    
    start.time = Sys.time()
    while(
      difftime(Sys.time(), start.time, units = "secs") < hyperparams$fit.seconds[i]
    ){
      MNet_mod$fit(100)
      cat(".")
      # Update prior variance
      for(layer in MNet_mod$layers){
        layer$prior$update(
          layer$weights, 
          update.mean = FALSE, 
          update.sd = TRUE,
          min.sd = .01
        )
      }
      # Update mean for final layer
      MNet_mod$layers[[2]]$prior$update(
        layer$weights, 
        update.mean = TRUE, 
        update.sd = FALSE,
        min.sd = .01
      )
    } # End while
    
    MNet_mod
  }
  
  hyperparams = data.frame(
    n.minibatch = 25, #default value
    sampler.size = rep(2:5, 9),
    n.importance.samples = 25, #default value
    n.layer1 = rep(c(8,10,12,14,16,18,20,22,24),each=4),
    learning.rate = 0.1,
    fit.seconds = 90)
  
  T1 <- Sys.time()
  
  mn = fit(
    x = as.matrix(train.data[,env_vars]),
    y = as.matrix(train.data[,species]), 
    hyperparams, 
    which.max(logliks[,5]))
  
  T2 <- Sys.time()
  tm <- difftime(T2,T1) 
  
  mpred <- predict(mn, as.matrix(na.omit(test.data[, 22:24])), n.importance.samples = 25)
  mp <- apply(mpred, 1:2, mean)
  colnames(mp) <- c(paste0("spp", 1:18))
  
  # RMSE
  
  Y = test.data[1:18]
  predY <- mp
  
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
  par[[1]] <- mn
  par[[2]] <- tm
  par[[3]] <- mp
  par[[4]] <- rmse
  par[[5]] <- tjur
  par[[6]] <- au
  
  names(par) <- c("mistnet_fit", "mistnet_time", "mistnet_predictions", "mistnet_RMSE", "mistnet_TjurR2", "mistnet_AUC")
  
  rm(list = c("x", "y", "env_vars", "species", "fit", "hyperparams", "mn", "T1", "T2", "tm", "mpred", "mp",
              "Y", "predY", "computeRMSE", "computeTjurR2", "computeAUC", "rmse", "tjur", "au", "mistnet.results", 
              "logliks", "test.data", "train.data", "training.samples"))  
  
  return(par)
}

cl <- parallel::makeCluster(detectCores())

mistnet_par <- parallel::parLapply(cl, 
                                   simulation1,
                                   missnet)

parallel::stopCluster(cl)

# save(mistnet_par, file = "Chapter-1/Scenario 1/Data/sc1_mistnet.RData")
