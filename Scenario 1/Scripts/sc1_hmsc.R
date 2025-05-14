# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Hmsc)
library(lubridate)
library(pROC)
library(caret)

# Loading data

load("Ch1_sc1_100_simulations.RData")

hmsc <- function(data){
  
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(Hmsc)
  library(lubridate)
  library(pROC)
  library(caret)
  
  training.samples <- data$sst %>%
    createDataPartition(p = 0.75, list = FALSE)
  train.data  <- data[training.samples, ]
  test.data <- data[-training.samples, ]    
  
  train.data$id <- 1:length(train.data$intercept)
  
  S = train.data[,c(25, 20, 21)]
  X = train.data[,22:24]
  Y = train.data[,1:18]
  
  if (is.numeric(as.matrix(Y)) || is.logical(as.matrix(Y)) && is.finite(sum(Y, na.rm=TRUE))) {
    print("Y looks OK")
  } else {
    print("Y should be numeric and have finite values")	}
  # Check that the stydy design data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(S))) {
    print("S has NA values - not allowed for")
  } else {
    print("S looks ok")	}
  # Check that the covariate data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(X))) {
    print("X has NA values - not allowed for")
  } else {
    print("X looks ok")	}
  
  Y = as.matrix(Y)
  
  XFormula <- ~ poly(sst, degree = 2, raw = TRUE) + dsStart + chla
  
  sdes <- data.frame(sample = as.factor(1:length(S$lon)))
  
  xy = as.matrix(cbind(S$lon, S$lat))
  colnames(xy) = c("x-coordinate","y-coordinate")
  
  rL.nngp <- Hmsc::HmscRandomLevel(sData = xy, longlat = TRUE, sMethod = 'NNGP',nNeighbours = 10)
  rL.nngp <- Hmsc::setPriors(rL.nngp, nfMin = 1, nfMax = 1)
  
  Ypa = 1*(Y > 0)
  
  mod <- Hmsc::Hmsc(Y = Ypa, XData = X, XFormula = XFormula,
                    distr = "probit", studyDesign = sdes,
                    ranLevels = list("sample" = rL.nngp))
  
  modelnames <- "presence_absence"
  
  # model fit
  
  T1 <- Sys.time()
  nChains = 4
  thin = 100 
  samples = 250 
  
  hm <- Hmsc::sampleMcmc(mod, samples = samples, thin = thin,
                         adaptNf = rep(ceiling(0.4*samples*thin),mod$nr), 
                         transient = ceiling(0.5*samples*thin),
                         nChains = nChains,
                         nParallel = nChains) 
  
  T2 <- Sys.time()
  tm <- difftime(T2,T1)
  
  sdes2 <- data.frame(sample = as.factor(1:length(test.data$lon)))
  hmp <- Hmsc:::predict.Hmsc(hm, studyDesign = sdes2, XData = test.data[,22:24], 
                             ranLevels = hm$rL, expected = TRUE, predictEtaMean = TRUE)
  
  smat <- Reduce(`+`, hmp)
  
  hpred <- smat / length(hmp)
  
  # RMSE
  
  Y = test.data[1:18]
  predY <- hpred
  
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
  par[[1]] <- hm
  par[[2]] <- tm
  par[[3]] <- hpred
  par[[4]] <- rmse
  par[[5]] <- tjur
  par[[6]] <- au
  
  names(par) <- c("hmsc_fit", "hmsc_time", "hmsc_predictions", "hmsc_RMSE", "hmsc_TjurR2", "hmsc_AUC")
  
  rm(list = c("S", "X", "Y", "XFormula", "sdes", "xy", "rL.nngp", "Ypa", "mod", "modelnames", "nChains", "thin", "samples", "tm",
              "T1", "hm", "T2", "hmp", "smat", "hpred", "Y", "predY", "rmse", "tjur", "au", "computeRMSE", "computeTjurR2", "computeAUC",
              "sdes2", "test.data", "train.data", "training.samples"))
  
  return(par)
} 

cl <- parallel::makeCluster(parallel::detectCores())

hmsc_par <- parallel::parLapply(cl, 
                                simulation1,
                                hmsc)

parallel::stopCluster(cl)

# save(hmsc_par, file = "sc1_hmsc.RData")

