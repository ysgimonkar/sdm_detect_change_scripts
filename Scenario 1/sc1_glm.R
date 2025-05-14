# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pROC)
library(parallel)
library(sdm)
library(caret)

# Loading data

load("sc1_simulations.RData")

glm_dat <- function(data){
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(pROC)
  library(parallel)
  library(caret)
  library(stats)
  
  training.samples <- data$sst %>%
    createDataPartition(p = 0.75, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]     
  
  # create a list of long datasets
  long <- train.data %>%
    pivot_longer(cols = c(1:18), names_to = "species", values_to = "occ")
  
  # create a list of individual species dataset for all 100 subsets
  data_list <- split(long, factor(long$species, levels = c(paste0("spp", c(1:18)))))
  
  # create model function
  model_fun <- function(x){
    glm(formula = formula(occ ~ poly(sst, degree = 2) + dsStart + chla), data = x, 
        family = binomial(link = "logit"))}
  
  # fit the model
  T1 <- Sys.time()
  glm_fit <- lapply(data_list, model_fun)
  T2 <- Sys.time()
  tm <- difftime(T2,T1)

  # create a list of long test datasets
  long2 <- test.data %>%
    pivot_longer(cols = c(1:18), names_to = "species", values_to = "occ")
  
  # create a list of individual species dataset 
  data_list2 <- split(long2, factor(long2$species, levels = c(paste0("spp", c(1:18))))) 
  
  # create model prediction function
  predict_fun <- function(x,y){
    predict(x, newdata = as.data.frame(y), type = "response")
  }
  
  # predict brt
  glm_pred <- mapply(predict_fun, x = glm_fit, y = data_list2)
  
  # RMSE
  Y = test.data[1:18]
  predY <- glm_pred
  
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
  
  a <- list()
  a[[1]] <- glm_fit
  a[[2]] <- tm
  a[[3]] <- glm_pred
  a[[4]] <- rmse
  a[[5]] <- tjur
  a[[6]] <- au
  a[[7]] <- train.data
  
  names(a) <- c("glm_fit", "glm_time", "glm_predictions", "glm_RMSE", "glm_TjurR2", "glm_AUC", "dataset")
  
  rm(list = c("long", "data_list", "model_fun", "T1", "glm_fit", "T2",
              "predict_fun", "glm_pred", "Y", "predY", "rmse", "tjur", "au", "computeRMSE", "computeTjurR2", "computeAUC",
              "test.data", "train.data", "training.samples", "data_list2", "long2", "tm"))
  
  
  return(a)
}

cl <- parallel::makeCluster(detectCores())

glm_par <- parallel::parLapply(cl, 
                               simulation1$datasets,
                               glm_dat)

parallel::stopCluster(cl)

rm(cl)
rm(glm_dat)

# save(glm_par, file = "sc1_glm.RData")
