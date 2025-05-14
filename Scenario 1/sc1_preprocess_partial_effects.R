# loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ecomix)
library(terra)
library(raster)
library(pROC)
library(boral)
library(rgdal)
library(rasterVis)
library(tibble)
library(sdm)
library(mistnet)
library(Hmsc)
library(parallel)
library(plotrix)

######################### RAW ############################

load("sc1_sam.RData")

raw_eff <- function(model_par){
  
  load("sc1_simulations.RData")
  
  fit <- model_par$sam_fit
  
  X <- fit[["titbits"]][["data"]][22:24]
  
  newx1 <- seq(range(X[,1])[1],range(X[,1])[2],len=20)
  newx11 <- newx1^2
  newx2 <- seq(range(X[,2])[1],range(X[,2])[2],len=20)
  newx3 <- seq(range(X[,3])[1],range(X[,3])[2],len=20)
  
  eff_data <- list()
  eff_data[[1]] <- cbind(1, newx1, newx11, mean(newx2), mean(newx3))
  eff_data[[2]] <- cbind(1, mean(newx1), mean(newx11), newx2, mean(newx3))
  eff_data[[3]] <- cbind(1, mean(newx1), mean(newx11), mean(newx2), newx3)
  
  raw <- as.matrix(data.frame(intercept = c(0,0,0,0),
                              sst = c(0, 1.5, -1.5, 1),    
                              sst2 = c(0, 0, 0, -1),
                              dsStart = c(-0.05, 0.25, -0.25, 1.5),
                              chla = c(0, 0, 0, 0)))
  
  spp_arc <- simulation1[["info"]][[1]][["SAMs"]]
  
  raw <- raw[spp_arc, , drop = FALSE]
  rownames(raw) <- simulation1[["info"]][[1]][["names"]][1:18]
  raw[,1] <- simulation1[["info"]][[1]][["alpha"]]
  
  yy_sst <- plogis(eff_data[[1]]%*%t(raw))
  xx_sst <- eff_data[[1]][,2]
  
  yy_dsstart <- plogis(eff_data[[2]]%*%t(raw))
  xx_dsstart <- eff_data[[2]][,4]
  
  yy_chla <- plogis(eff_data[[3]]%*%t(raw))
  xx_chla <- eff_data[[3]][,5]
  
  par <- list()
  par[[1]] <- xx_sst
  par[[2]] <- yy_sst
  par[[3]] <- xx_dsstart
  par[[4]] <- yy_dsstart
  par[[5]] <- xx_chla
  par[[6]] <- yy_chla
  
  names(par) <- c("raw_xx_sst", "raw_yy_sst", "raw_xx_dss", "raw_yy_dss", "raw_xx_chl", "raw_yy_chl")
  
  return(par)
  
  rm(list = "fit", "X", "newx1", "newx11", "newx2", "newx3", "eff_data", "spp_arc", 
     "raw", "yy_sst", "xx_sst", "yy_dsstart", "xx_dsstart", "yy_chla", "xx_chla")
  
}

cl <- parallel::makeCluster(detectCores())
raw_pe <- parallel::parLapply(cl, sam_par, raw_eff)
parallel::stopCluster(cl)

raw_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$raw_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$raw_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$raw_xx_chl)
    yy_sst[[i]] <- model_pe[[i]]$raw_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$raw_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$raw_yy_chl
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("raw_xx_sst", "raw_yy_sst_mean", "raw_yy_sst_ci",
                            "raw_xx_dss", "raw_yy_dss_mean", "raw_yy_dss_ci",
                            "raw_xx_chl", "raw_yy_chl_mean", "raw_yy_chl_ci")
  
  return(m)
}

raw_pp <- raw_xy(raw_pe) 

rm(list = c("cl", "raw_pe", "raw_eff", "raw_xy"))

#-------------------------------------------------------

######################## SAM #############################

# SAM Partial effects: xx and yy
sam_eff <- function(model_par){
  
  library(ecomix)
  
  model_fit <- model_par$sam_fit
  model_boot <- model_par$sam_bootstrap
  
  tt <- terms(model_fit$titbits$archetype_formula)
  tt <- delete.response(tt)
  vars <- all.vars(parse(text = tt))
  nvars = length(vars)
  
  pred.data <- model_fit$titbits$data
  pred.data <- pred.data[, vars, drop = FALSE]
  
  focal.predictors <- c("sst", "dsStart", "chla")
  focal.ids <- lapply(focal.predictors, grep, colnames(pred.data))
  
  # lists for data structures
  mfs <- list()
  f.focal <- list()
  v.focal <- list()
  n.focal <- list()
  factors <- NULL
  ngrid <- 20
  
  for(ii in 1:nvars){
    factors[ii] <- is.factor(pred.data[, vars[ii]])
  }
  
  for(i in 1:length(focal.ids)){
    f.focal[[i]] <- factors[focal.ids[[i]]]
    if(any(f.focal[[i]])) v.focal[[i]] <- pred.data[, focalids[[i]]]
    else v.focal[[i]] <- pred.data[, focal.ids[[i]],drop=FALSE]
    n.focal[[i]] <- seq_len(nvars)[-unlist(focal.ids[[i]])]
    
    xx <- list()
    
    for(j in 1:length(focal.ids[[i]])) {
      if(f.focal[[i]][j]) {
        xx[[j]] = levels(v.focal[[i]][j])
        ngrid = length(xx)
      } else {
        mi = min(v.focal[[i]][j])
        ma = max(v.focal[[i]][j])
        xx[[j]] = seq(mi, ma, length.out = ngrid)
      }
    }
    XDataNew = data.frame(xx, stringsAsFactors = TRUE)
    colnames(XDataNew) = vars[focal.ids[[i]]]
    
    for(k in seq_len(length(n.focal[[i]]))){
      non.focal = n.focal[[i]][k]
      f.non.focal = factors[non.focal]
      v.non.focal = pred.data[, vars[non.focal]]
      if(f.non.focal){
        XDataNew[, vars[non.focal]] = Mode(v.non.focal)
      }
      if(!f.non.focal){
        v.non.focal = pred.data[, vars[non.focal]]
        XDataNew[, vars[non.focal]] = mean(v.non.focal)
      }
    }
    mfs[[i]] <- XDataNew[,vars]
  }
  names(mfs) <- focal.predictors
  class(mfs) <- "species_mix_effectPlotData"
  
  add_row <- function(x, degree = 2){
    factors <- NULL
    for( i in 1:ncol(x)){
      factors[i] <- is.factor(x[,i])
    }
    
    x[nrow(x) + 1:degree ,factors] <- unique(x[nrow(x),factors])
    x[nrow(x) + ((1:degree)-degree) ,!factors] <- rnorm(ncol(x[nrow(x),!factors])*degree, sd=1e-6)
    
    return(x)
  }
  
  xnew <- lapply(mfs, add_row)
  object <- model_fit
  object2 <- model_boot
  nboot <- 0
  CI <- c(0.025, 0.975)
  type = "response"
  typePred = "species"
  response.var = NULL
  
  
  
  partial.preds <- suppressMessages(lapply(xnew, function(ii) predict(object=object, object2=object2,
                                                                      newdata = ii, offset = NULL,
                                                                      nboot = nboot, alpha=CI[2]-CI[1],
                                                                      prediction.type = typePred, type=type)))
  
  ## remove the dummy columns
  remove_dummy_rows <- function(x, partial.preds){
    nrow.orig <- nrow(x)
    if(length(partial.preds)==4){
      partial.preds$ptPreds <- partial.preds$ptPreds[1:nrow.orig, ,drop=FALSE]
      partial.preds$bootPreds <- partial.preds$bootPreds[1:nrow.orig, ,drop=FALSE]
      partial.preds$bootSEs <- partial.preds$bootSEs[1:nrow.orig, ,drop=FALSE]
      partial.preds$bootCIs <- partial.preds$bootCIs[1:nrow.orig, , , drop=FALSE]
    } else {
      partial.preds <- partial.preds[1:nrow.orig, ,drop=FALSE]
    }
    return(partial.preds)
  }
  
  partial.preds <- lapply(1:length(mfs),function(ii)remove_dummy_rows(mfs[[ii]],partial.preds[[ii]]))
  names(partial.preds) <- names(mfs)
  
  idx <- seq(1,ncol(partial.preds[[i]][[1]]))
  
  a <- list()
  a[[1]] <- mfs[[1]][,names(mfs[1])]
  a[[2]] <- partial.preds[[1]]$bootPreds[,idx,drop=FALSE]
  a[[3]] <- mfs[[2]][,names(mfs[2])]
  a[[4]] <- partial.preds[[2]]$bootPreds[,idx,drop=FALSE]
  a[[5]] <- mfs[[3]][,names(mfs[3])]
  a[[6]] <- partial.preds[[3]]$bootPreds[,idx,drop=FALSE]
  
  names(a) <- c("sam_xx_sst", "sam_yy_sst",
                "sam_xx_dss", "sam_yy_dss", 
                "sam_xx_chla", "sam_yy_chla")
  return(a)
  
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
sam_pe <- parallel::parLapply(cl, sam_par, sam_eff)
parallel::stopCluster(cl)

# function for combining all 100 xx and yy into one
sam_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$sam_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$sam_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$sam_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$sam_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$sam_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$sam_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("sam_xx_sst", "sam_yy_sst_mean", "sam_yy_sst_ci",
                            "sam_xx_dss", "sam_yy_dss_mean", "sam_yy_dss_ci",
                            "sam_xx_chl", "sam_yy_chl_mean", "sam_yy_chl_ci")
  
  return(m)
}

sam_pp <- sam_xy(sam_pe) 

rm(list = c("cl", "sam_par", "sam_pe", "sam_eff", "sam_xy"))

#---------------------------------------------------------------------------

######################## BORAL #############################

load("sc1_boral.RData")

# create a list of 100 fits
boral_fit <- list()
for (i in 1:100){
  boral_fit[[i]] <- boral_par[[i]]$boral_fit
}

rm(boral_par)

# BORAL Partial effects: xx and yy
boral_eff <- function(model_fit){
  
  library(boral)
  
  pred.data <- model_fit$X
  focal.predictors <- c("sst", "dsStart", "chla")
  focal.ids <- lapply(focal.predictors, grep, colnames(pred.data))
  vars <- colnames(pred.data)
  nvars <- length(vars)
  
  # lists for data structures
  mfs <- list()
  f.focal <- list()
  v.focal <- list()
  n.focal <- list()
  factors = NULL
  
  for(ii in 1:nvars){
    factors[[ii]] <- is.factor(pred.data[,vars[ii]])
  }
  
  for(i in 1:length(focal.ids)){
    f.focal[[i]] <- factors[focal.ids[[i]]]
    v.focal[[i]] <- pred.data[, focal.ids[[i]], drop = FALSE]
    n.focal[[i]] <- seq_len(nvars)[-unlist(focal.ids[[i]])]
    
    xx <- list(list())
    ngrid = 20
    
    for(j in 1:length(focal.ids[[i]])){
      mi = min(v.focal[[i]][,j])
      ma = max(v.focal[[i]][,j])
      xx[[j]] =  seq(mi, ma, length.out = ngrid)
    }
    
    XDataNew = data.frame(xx, stringsAsFactors = TRUE)
    colnames(XDataNew) = vars[focal.ids[[i]]] 
    
    for(k in seq_len(length(n.focal[[i]]))){
      non.focal = n.focal[[i]][k]
      v.non.focal = pred.data[, vars[non.focal]]
      XDataNew[, vars[non.focal]] = mean(v.non.focal)
    }
    
    mfs[[i]] <- XDataNew[,vars]
  }
  
  names(mfs) <- focal.predictors
  class(mfs) <- "species_mix_effectPlotData"
  
  for (i in seq(length(mfs))){
    mfs[[i]]$`poly(sst, degree = 2, raw = TRUE)2` <- mfs[[i]]$`poly(sst, degree = 2, raw = TRUE)1`^2
  }
  
  # newrow.ids <- matrix(data = seq(1:50), ncol = 1)
  # colnames(newrow.ids) <- "ID2"
  
  boral_pred <- function(ii){
    predict.boral(model_fit, newX = ii, predict.type = "marginal", scale = "response")
  }
  
  partial.preds <- lapply(mfs, boral_pred)
  
  a <- list()
  a[[1]] <- mfs[[1]][,1]
  a[[2]] <- partial.preds$sst$linpred
  colnames(a[[2]]) <- rownames(model_fit[["X.coefs.mean"]])
  a[[3]] <- mfs[[2]][,3]
  a[[4]] <- partial.preds$dsStart$linpred
  colnames(a[[4]]) <- rownames(model_fit[["X.coefs.mean"]])
  a[[5]] <- mfs[[3]][,4]
  a[[6]] <- partial.preds$chla$linpred
  colnames(a[[6]]) <- rownames(model_fit[["X.coefs.mean"]])
  
  
  names(a) <- c("boral_xx_sst", "boral_yy_sst",
                "boral_xx_dss", "boral_yy_dss", 
                "boral_xx_chla", "boral_yy_chla")
  return(a)    
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
boral_pe <- parallel::parLapply(cl, 
                                boral_fit, 
                                boral_eff)
parallel::stopCluster(cl)

# function for combining all 100 xx and yy into one
boral_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$boral_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$boral_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$boral_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$boral_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$boral_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$boral_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("boral_xx_sst", "boral_yy_sst_mean", "boral_yy_sst_ci",
                            "boral_xx_dss", "boral_yy_dss_mean", "boral_yy_dss_ci",
                            "boral_xx_chl", "boral_yy_chl_mean", "boral_yy_chl_ci")
  
  return(m)
}

boral_pp <- boral_xy(boral_pe) 

rm(list = c("cl", "boral_fit", "boral_pe", "boral_eff", "boral_xy", "i"))

#----------------------------------------------------------------------

######################## HMSC #############################

load("sc1_hmsc.RData")

# create a list of 100 fits
hm_fit <- list()
for (i in 1:100){
  hm_fit[[i]] <- hmsc_par[[i]]$hmsc_fit
}

rm(list = c("hmsc_par", "i"))

hmsc_eff <- function(model_fit){
  
  library(Hmsc)
  
  mfs <- list()
  
  sst_grad <- constructGradient(model_fit, focalVariable = "sst", ngrid = 20, non.focalVariables = list(dsStart = list(1), chla = list(1)))
  mfs[[1]] <- sst_grad$XDataNew
  
  dss_grad <- constructGradient(model_fit, focalVariable = "dsStart", ngrid = 20, non.focalVariables = list(sst = list(1), chla = list(1)))
  mfs[[2]] <- dss_grad$XDataNew
  mfs[[2]] <- mfs[[2]][,c("sst", "dsStart", "chla")]
  
  chl_grad <- constructGradient(model_fit, focalVariable = "chla", ngrid = 20, non.focalVariables = list(sst = list(1), dsStart = list(1)))
  mfs[[3]] <- chl_grad$XDataNew
  mfs[[3]] <- mfs[[3]][,c("sst", "dsStart", "chla")]
  
  sdes <- data.frame(sample = as.factor(1:20))
  
  hmsc_pred <- function(x){
    predict(model_fit, XData = x, studyDesign = sdes, ranLevels =  model_fit$rL, expected = TRUE, predictEtaMean = TRUE)
  }
  
  partial.preds <-lapply(mfs, hmsc_pred)
  
  sum_sst <- Reduce(`+`, partial.preds[[1]])
  sum_dsStart <- Reduce(`+`, partial.preds[[2]])
  sum_chla <- Reduce(`+`, partial.preds[[3]])
  
  a <- list()
  a[[1]] <- mfs[[1]]$sst
  a[[2]] <- sum_sst / length(partial.preds[[1]])
  
  a[[3]] <- mfs[[2]]$dsStart
  a[[4]] <- sum_dsStart / length(partial.preds[[2]])
  
  a[[5]] <- mfs[[3]]$chla
  a[[6]] <- sum_chla / length(partial.preds[[3]])
  
  names(a) <- c("hmsc_xx_sst", "hmsc_yy_sst",
                "hmsc_xx_dss", "hmsc_yy_dss",
                "hmsc_xx_chla", "hmsc_yy_chla")
  return(a)
  
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
hmsc_pe <- parallel::parLapply(cl, 
                               hm_fit, 
                               hmsc_eff)
parallel::stopCluster(cl)

# function for combining all 100 xx and yy into one
hmsc_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$hmsc_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$hmsc_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$hmsc_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$hmsc_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$hmsc_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$hmsc_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("hmsc_xx_sst", "hmsc_yy_sst_mean", "hmsc_yy_sst_ci",
                            "hmsc_xx_dss", "hmsc_yy_dss_mean", "hmsc_yy_dss_ci",
                            "hmsc_xx_chl", "hmsc_yy_chl_mean", "hmsc_yy_chl_ci")
  
  return(m)
}

hmsc_pp <- hmsc_xy(hmsc_pe) 

rm(list = c("cl", "hm_fit", "hmsc_pe", "hmsc_eff", "hmsc_xy"))

#------------------------------------------------------------------------------

######################## MISTNET #############################

load("sc1_mistnet.RData")

# create a list of 100 fits
mn_fit <- list()
for (i in 1:100){
  mn_fit[[i]] <- mistnet_par[[i]]$mistnet_fit
}

# remove rest of the objects
rm(mistnet_par)

# function to create 100 xx and yy values
mistnet_eff <- function(model_fit){
  
  library(mistnet)
  
  pred.data <- model_fit$x
  focal.predictors <- c("sst", "dsStart", "chla")
  focal.ids <- lapply(focal.predictors, grep, colnames(pred.data))
  vars <- colnames(pred.data)
  nvars <- length(vars)
  
  # lists for data structures
  mfs <- list()
  f.focal <- list()
  v.focal <- list()
  n.focal <- list()
  factors = NULL
  
  for(ii in 1:nvars){
    factors[[ii]] <- is.factor(pred.data[,vars[ii]])
  }
  
  for(i in 1:length(focal.ids)){
    f.focal[[i]] <- factors[focal.ids[[i]]]
    v.focal[[i]] <- pred.data[, focal.ids[[i]],drop=FALSE]
    n.focal[[i]] <- seq_len(nvars)[-unlist(focal.ids[[i]])]
    
    ngrid = 20
    xx <- list()
    
    for(j in 1:length(focal.ids[[i]])){
      mi = min(v.focal[[i]][,j])
      ma = max(v.focal[[i]][,j])
      xx[[j]] =  seq(mi, ma, length.out = ngrid)
    }
    
    XDataNew = data.frame(xx, stringsAsFactors = TRUE)
    colnames(XDataNew) = vars[focal.ids[[i]]]
    
    for(k in seq_len(length(n.focal[[i]]))){
      non.focal = n.focal[[i]][k]
      v.non.focal = pred.data[, vars[non.focal]]
      XDataNew[, vars[non.focal]] = mean(v.non.focal)
      
    }
    mfs[[i]] <- XDataNew[,vars]
  }
  
  names(mfs) <- focal.predictors
  class(mfs) <- "species_mix_effectPlotData"
  
  mn_pred <- function(ii){
    predict(model_fit, as.matrix(na.omit(ii)), n.importance.samples = 25)
  }
  
  partial.preds <- lapply(mfs, mn_pred)
  
  pp <- function(x){
    apply(x, 1:2, mean)
  }
  
  mp <- lapply(partial.preds, pp)
  
  for(i in 1:3){
    colnames(mp[[i]]) <- c(paste0("spp", 1:18))
  }
  
  
  a <- list()
  a[[1]] <- mfs[[1]][,1]
  a[[2]] <- mp[[1]]
  
  a[[3]] <- mfs[[2]][,2]
  a[[4]] <- mp[[2]]
  
  a[[5]] <- mfs[[3]][,3]
  a[[6]] <- mp[[3]]
  
  names(a) <- c("mn_xx_sst", "mn_yy_sst",
                "mn_xx_dss", "mn_yy_dss", 
                "mn_xx_chla", "mn_yy_chla")
  
  rm(list = c("pred.data", "focal.predictors", "focal.ids", "vars", "nvars",
              "mfs", "f.focal", "v.focal", "n.focal", "factors", "xx", "ngrid", "mi", "ma",
              "XDataNew","non.focal", "f.non.focal", "v.non.focal", "mfs",
              "mnpred", "partial.preds", "pp", "mp"))
  
  return(a)  
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
mistnet_pe <- parallel::parLapply(cl, 
                                  mn_fit, 
                                  mistnet_eff)
parallel::stopCluster(cl)


# function for combining all 100 xx and yy into one
mn_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$mn_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$mn_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$mn_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$mn_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$mn_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$mn_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("mn_xx_sst", "mn_yy_sst_mean", "mn_yy_sst_ci",
                            "mn_xx_dss", "mn_yy_dss_mean", "mn_yy_dss_ci",
                            "mn_xx_chl", "mn_yy_chl_mean", "mn_yy_chl_ci")
  
  return(m)
}

mistnet_pp <- mn_xy(mistnet_pe) 

# remove other unwanted parameters
rm(list = c("cl", "mn_fit", "mistnet_pe", "mistnet_eff", "mn_xy", "i"))

# -------------------------------------------------------------------

######################## GLM #############################

load("sc1_glm.RData")
glm_eff <- function(model_par){
  
  library(stats)
  
  data <- model_par$dataset
  pred.data <- data[,22:24]
  focal.predictors <- c("sst", "dsStart", "chla")
  focal.ids <- lapply(focal.predictors, grep, colnames(pred.data))
  vars <- colnames(pred.data)
  nvars <- length(vars)
  
  # lists for data structures
  mfs <- list()
  f.focal <- list()
  v.focal <- list()
  n.focal <- list()
  factors = NULL
  
  for(ii in 1:nvars){
    factors[[ii]] <- is.factor(pred.data[,vars[ii]])
  }
  
  for(i in 1:length(focal.ids)){
    f.focal[[i]] <- factors[focal.ids[[i]]]
    v.focal[[i]] <- pred.data[, focal.ids[[i]],drop=FALSE]
    n.focal[[i]] <- seq_len(nvars)[-unlist(focal.ids[[i]])]
    
    ngrid = 20
    xx <- list()
    
    for(j in 1:length(focal.ids[[i]])){
      mi = min(v.focal[[i]][,j])
      ma = max(v.focal[[i]][,j])
      xx[[j]] =  seq(mi, ma, length.out = ngrid)
    }
    
    XDataNew = data.frame(xx, stringsAsFactors = TRUE)
    colnames(XDataNew) = vars[focal.ids[[i]]]
    
    for(k in seq_len(length(n.focal[[i]]))){
      non.focal = n.focal[[i]][k]
      v.non.focal = pred.data[, vars[non.focal]]
      XDataNew[, vars[non.focal]] = mean(v.non.focal)
      
    }
    mfs[[i]] <- XDataNew[,vars]
  }
  
  names(mfs) <- focal.predictors
  class(mfs) <- "list"
  
  fit <- model_par$glm_fit
  
  sst <- list()
  for (i in 1:18) {
    sst[[i]] <- predict(fit[[i]], newdata = mfs$sst, type = "response")
  }
  
  dss <- list()
  for (i in 1:18) {
    dss[[i]] <- predict(fit[[i]], newdata = mfs$dsStart, type = "response")
  }
  
  chla <- list()
  for (i in 1:18) {
    chla[[i]] <- predict(fit[[i]], newdata = mfs$chla, type = "response")
  }
  
  mat_sst <- matrix(unlist(sst), ncol = 18)
  mat_dss <- matrix(unlist(dss), ncol = 18)
  mat_chl <- matrix(unlist(chla), ncol = 18)
  
  a <- list()
  a[[1]] <- mfs[[1]][,1]
  a[[2]] <- mat_sst
  
  a[[3]] <- mfs[[2]][,2]
  a[[4]] <- mat_dss
  
  a[[5]] <- mfs[[3]][,3]
  a[[6]] <- mat_chl
  
  names(a) <- c("glm_xx_sst", "glm_yy_sst",
                "glm_xx_dss", "glm_yy_dss", 
                "glm_xx_chla", "glm_yy_chla")
  
  rm(list = c("pred.data", "focal.predictors", "focal.ids", "vars", "nvars",
              "mfs", "f.focal", "v.focal", "n.focal", "factors", "xx", "ngrid", "mi", "ma",
              "XDataNew","non.focal", "f.non.focal", "v.non.focal", "mfs",
              "sst", "dss", "chla"))
  
  return(a)  
  
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
glm_pe <- parallel::parLapply(cl, 
                              glm_par, 
                              glm_eff)
parallel::stopCluster(cl)

# function for combining all 100 xx and yy into one
glm_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$glm_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$glm_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$glm_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$glm_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$glm_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$glm_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("glm_xx_sst", "glm_yy_sst_mean", "glm_yy_sst_ci",
                            "glm_xx_dss", "glm_yy_dss_mean", "glm_yy_dss_ci",
                            "glm_xx_chl", "glm_yy_chl_mean", "glm_yy_chl_ci")
  
  return(m)
}

glm_pp <- glm_xy(glm_pe) 

rm(list = c("cl", "glm_pe", "glm_eff", "glm_xy"))

#-------------------------------------------------------------------------

######################## BRT #############################

load("sc1_brt.RData")

brt_eff <- function(model_par){
  
  library(gbm)
  
  data <- model_par$dataset
  pred.data <- data[,22:24]
  focal.predictors <- c("sst", "dsStart", "chla")
  focal.ids <- lapply(focal.predictors, grep, colnames(pred.data))
  vars <- colnames(pred.data)
  nvars <- length(vars)
  
  # lists for data structures
  mfs <- list()
  f.focal <- list()
  v.focal <- list()
  n.focal <- list()
  factors = NULL
  
  for(ii in 1:nvars){
    factors[[ii]] <- is.factor(pred.data[,vars[ii]])
  }
  
  for(i in 1:length(focal.ids)){
    f.focal[[i]] <- factors[focal.ids[[i]]]
    v.focal[[i]] <- pred.data[, focal.ids[[i]],drop=FALSE]
    n.focal[[i]] <- seq_len(nvars)[-unlist(focal.ids[[i]])]
    
    ngrid = 20
    xx <- list()
    
    for(j in 1:length(focal.ids[[i]])){
      mi = min(v.focal[[i]][,j])
      ma = max(v.focal[[i]][,j])
      xx[[j]] =  seq(mi, ma, length.out = ngrid)
    }
    
    XDataNew = data.frame(xx, stringsAsFactors = TRUE)
    colnames(XDataNew) = vars[focal.ids[[i]]]
    
    for(k in seq_len(length(n.focal[[i]]))){
      non.focal = n.focal[[i]][k]
      v.non.focal = pred.data[, vars[non.focal]]
      XDataNew[, vars[non.focal]] = mean(v.non.focal)
      
    }
    mfs[[i]] <- XDataNew[,vars]
  }
  
  names(mfs) <- focal.predictors
  class(mfs) <- "list"
  
  fit <- model_par$brt_fit
  
  sst <- list()
  for (i in 1:18) {
    sst[[i]] <- predict(fit[[i]], newdata = mfs$sst, type = "response")
  }
  
  dss <- list()
  for (i in 1:18) {
    dss[[i]] <- predict(fit[[i]], newdata = mfs$dsStart, type = "response")
  }
  
  chla <- list()
  for (i in 1:18) {
    chla[[i]] <- predict(fit[[i]], newdata = mfs$chla, type = "response")
  }
  
  mat_sst <- matrix(unlist(sst), ncol = 18)
  mat_dss <- matrix(unlist(dss), ncol = 18)
  mat_chl <- matrix(unlist(chla), ncol = 18)
  
  a <- list()
  a[[1]] <- mfs[[1]][,1]
  a[[2]] <- mat_sst
  
  a[[3]] <- mfs[[2]][,2]
  a[[4]] <- mat_dss
  
  a[[5]] <- mfs[[3]][,3]
  a[[6]] <- mat_chl
  
  names(a) <- c("brt_xx_sst", "brt_yy_sst",
                "brt_xx_dss", "brt_yy_dss", 
                "brt_xx_chla", "brt_yy_chla")
  
  rm(list = c("pred.data", "focal.predictors", "focal.ids", "vars", "nvars",
              "mfs", "f.focal", "v.focal", "n.focal", "factors", "xx", "ngrid", "mi", "ma",
              "XDataNew","non.focal", "f.non.focal", "v.non.focal", "mfs",
              "sst", "dss", "chla"))
  
  return(a)  
  
}

# parallel processing of 100 xx and yy values
cl <- parallel::makeCluster(detectCores())
brt_pe <- parallel::parLapply(cl, 
                              brt_par, 
                              brt_eff)
parallel::stopCluster(cl)

# function for combining all 100 xx and yy into one
brt_xy <- function(model_pe){
  
  library(dplyr)
  
  xx_sst <- list()
  xx_dss <- list()
  xx_chl <- list()
  yy_sst <- list()
  yy_dss <- list()
  yy_chl <- list()
  
  for(i in 1:100){
    xx_sst[[i]] <- as.matrix(model_pe[[i]]$brt_xx_sst)
    xx_dss[[i]] <- as.matrix(model_pe[[i]]$brt_xx_dss)
    xx_chl[[i]] <- as.matrix(model_pe[[i]]$brt_xx_chla)
    yy_sst[[i]] <- model_pe[[i]]$brt_yy_sst
    yy_dss[[i]] <- model_pe[[i]]$brt_yy_dss
    yy_chl[[i]] <- model_pe[[i]]$brt_yy_chla
  }
  
  sumxsst <- Reduce(`+`, xx_sst)
  sumxdss <- Reduce(`+`, xx_dss)
  sumxchl <- Reduce(`+`, xx_chl)
  
  sst_xx_mean <- as.numeric(sumxsst / length(xx_sst))
  dss_xx_mean <- as.numeric(sumxdss / length(xx_dss))
  chl_xx_mean <- as.numeric(sumxchl / length(xx_chl))
  
  n_rows <- nrow(yy_sst[[1]])
  n_cols <- ncol(yy_sst[[1]])
  
  sst_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_mean) <- colnames(yy_sst[[1]])
  sst_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(sst_yy_ci) <- colnames(yy_sst[[1]])
  
  dss_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_mean) <- colnames(yy_dss[[1]])
  dss_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(dss_yy_ci) <- colnames(yy_dss[[1]])
  
  chl_yy_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_mean) <- colnames(yy_chl[[1]])
  chl_yy_ci <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(chl_yy_ci) <- colnames(yy_chl[[1]])
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_sst <- sapply(yy_sst, function(mat) mat[i, j])
      sst_yy_mean[i, j] <- mean(values_sst)
      sst_yy_ci[i, j] <- 1.96*std.error(values_sst)
      
      values_dss <- sapply(yy_dss, function(mat) mat[i, j])
      dss_yy_mean[i, j] <- mean(values_dss)
      dss_yy_ci[i, j] <- 1.96*std.error(values_dss)
      
      values_chl <- sapply(yy_chl, function(mat) mat[i, j])
      chl_yy_mean[i, j] <- mean(values_chl)
      chl_yy_ci[i, j] <- 1.96*std.error(values_chl)
    }
  }
  
  m <- list()
  m[[1]] <- sst_xx_mean
  m[[2]] <- sst_yy_mean
  m[[3]] <- sst_yy_ci 
  m[[4]] <- dss_xx_mean
  m[[5]] <- dss_yy_mean
  m[[6]] <- dss_yy_ci
  m[[7]] <- chl_xx_mean
  m[[8]] <- chl_yy_mean
  m[[9]] <- chl_yy_ci
  
  names(m) <- names(m) <- c("brt_xx_sst", "brt_yy_sst_mean", "brt_yy_sst_ci",
                            "brt_xx_dss", "brt_yy_dss_mean", "brt_yy_dss_ci",
                            "brt_xx_chl", "brt_yy_chl_mean", "brt_yy_chl_ci")
  
  return(m)
}

brt_pp <- brt_xy(brt_pe) 

rm(list = c("cl", "brt_pe", "brt_eff", "brt_xy"))

#-------------------------------------------------------------------------

# save(boral_pp, brt_pp, glm_pp, hmsc_pp, mistnet_pp, raw_pp, sam_pp, file = "sc1_partial_effects.RData")
