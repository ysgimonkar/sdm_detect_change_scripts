# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ecomix)
library(raster)
library(pROC)
library(boral)
library(terra)
# library(rgdal)
library(rasterVis)
library(tibble)
library(sdm)
library(mistnet)
library(Hmsc)
library(plotrix)
library(parallel)

############################ Model Extrapolations (except HMSC and BORAL) ###############
######################### Raw ###########################

# load extrapolation data
load("extrapolation_data.RData")
load("sc1_simulations.RData")

# Full

rawp_full_fun <- function(data){
  
  raw <- as.matrix(data.frame(intercept = c(0,0,0,0),
                              sst = c(0, 1.5, -1.5, 1),    
                              sst2 = c(0, 0, 0, -1),
                              dsStart = c(-0.05, 0.25, -0.25, 1.5),
                              chla = c(0, 0, 0, 0)))
  
  spp_arc <- simulation1[["info"]][[1]][["SAMs"]]
  
  raw <- raw[spp_arc, , drop = FALSE]
  rownames(raw) <- simulation1[["info"]][[1]][["names"]][1:18]
  raw[,1] <- simulation1[["info"]][[1]][["alpha"]]
  
  full <- data$full
  full <- add_column(full, sst2 = full$sst^2, .after = "sst") 
  full$intercept <- 1
  full <- dplyr::select(full, c(intercept, sst, sst2, dsStart, chla))
  full <- as.matrix(full)
  
  raw_full <- plogis(full%*%t(raw))
  
  return(raw_full)
  
  rm(list = "raw", "spp_arc", "full", "raw_full")
}
raw_full <- rawp_full_fun(ext_data)

# Initial

rawp_initial_fun <- function(data){
  
  raw <- as.matrix(data.frame(intercept = c(0,0,0,0),
                              sst = c(0, 1.5, -1.5, 1),    
                              sst2 = c(0, 0, 0, -1),
                              dsStart = c(-0.05, 0.25, -0.25, 1.5),
                              chla = c(0, 0, 0, 0)))
  
  spp_arc <- simulation1[["info"]][[1]][["SAMs"]]
  
  raw <- raw[spp_arc, , drop = FALSE]
  rownames(raw) <- simulation1[["info"]][[1]][["names"]][1:18]
  raw[,1] <- simulation1[["info"]][[1]][["alpha"]]
  
  initial <- data$initial
  initial <- add_column(initial, sst2 = initial$sst^2, .after = "sst") 
  initial$intercept <- 1
  initial <- dplyr::select(initial, c(intercept, sst, sst2, dsStart, chla))
  initial <- as.matrix(initial)
  
  raw_initial <- plogis(initial%*%t(raw))
  
  return(raw_initial)
  
  rm(list = "raw", "spp_arc", "initial", "raw_initial")
}
raw_initial <- rawp_initial_fun(ext_data)

# Final

rawp_final_fun <- function(data){
  
  raw <- as.matrix(data.frame(intercept = c(0,0,0,0),
                              sst = c(0, 1.5, -1.5, 1),    
                              sst2 = c(0, 0, 0, -1),
                              dsStart = c(-0.05, 0.25, -0.25, 1.5),
                              chla = c(0, 0, 0, 0)))
  
  spp_arc <- simulation1[["info"]][[1]][["SAMs"]]
  
  raw <- raw[spp_arc, , drop = FALSE]
  rownames(raw) <- simulation1[["info"]][[1]][["names"]][1:18]
  raw[,1] <- simulation1[["info"]][[1]][["alpha"]]
  
  final <- data$final
  final <- add_column(final, sst2 = final$sst^2, .after = "sst") 
  final$intercept <- 1
  final <- dplyr::select(final, c(intercept, sst, sst2, dsStart, chla))
  final <- as.matrix(final)
  
  raw_final <- plogis(final%*%t(raw))
  
  return(raw_final)
  
  rm(list = "raw", "spp_arc", "final", "raw_final")
}
raw_final <- rawp_final_fun(ext_data)

raw_ext <- list()
raw_ext[[1]] <- raw_full
raw_ext[[2]] <- raw_initial
raw_ext[[3]] <- raw_final

names(raw_ext) <- c("full", "initial", "final")

rm(list = c("rawp_final_fun", "rawp_full_fun", "rawp_initial_fun", "raw_full", "raw_initial", "raw_final"))

save(raw_ext, file = "sc1_raw_extrapolations.RData")

########################## SAM ###############################

# load the models
load("sc1_sam.RData")

# Full

samp_full_fun <- function(model_par){
  
  library(ecomix)
  
  fit <- model_par$sam_fit
  
  load("extrapolation_data.RData")
  
  full <- ext_data$full[3:5]
  
  full_p <- predict(fit, newdata = full, prediction.type = "species", type = "response")
  
  return(full_p)
  
  rm(list = c("fit", "full"))
}
cl <- parallel::makeCluster(detectCores()-1)
sam_full <- parallel::parLapply(cl, sam_par, samp_full_fun)
parallel::stopCluster(cl)

# Initial

samp_initial_fun <- function(model_par){
  
  library(ecomix)
  
  fit <- model_par$sam_fit
  
  load("extrapolation_data.RData")
  
  initial <- ext_data$initial[3:5]
  
  initial_p <- predict(fit, newdata = initial, prediction.type = "species", type = "response")
  
  return(initial_p)
  
  rm(list = c("fit", "initial"))
}
cl <- parallel::makeCluster(detectCores()-1)
sam_initial <- parallel::parLapply(cl, sam_par, samp_initial_fun)
parallel::stopCluster(cl)

# Final

samp_final_fun <- function(model_par){
  
  library(ecomix)
  
  fit <- model_par$sam_fit
  
  load("extrapolation_data.RData")
  
  final <- ext_data$final[3:5]
  
  final_p <- predict(fit, newdata = final, prediction.type = "species", type = "response")
  
  return(final_p)
  
  rm(list = c("fit", "final"))
}
cl <- parallel::makeCluster(detectCores()-1)
sam_final <- parallel::parLapply(cl, sam_par, samp_final_fun)
parallel::stopCluster(cl)

rm(list = c("cl", "sam_par", "samp_full_fun", "samp_initial_fun", "samp_final_fun"))

save(sam_final, sam_full, sam_initial, file = "sc1_sam_extrapolations.RData")

# Mean

sam_ext_fun <- function(model_full, model_initial, model_final){
  
  n_rows <- nrow(sam_full[[1]])
  n_cols <- ncol(sam_full[[1]])
  
  full_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(full_mean) <- colnames(sam_full[[1]])
  
  initial_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(initial_mean) <- colnames(sam_initial[[1]])
  
  final_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(final_mean) <- colnames(sam_final[[1]])
  
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_full <- sapply(sam_full, function(mat) mat[i, j])
      full_mean[i, j] <- mean(values_full)
      
      values_initial <- sapply(sam_initial, function(mat) mat[i, j])
      initial_mean[i, j] <- mean(values_initial)
      
      values_final <- sapply(sam_final, function(mat) mat[i, j])
      final_mean[i, j] <- mean(values_final)
    }
  }
  
  a <- list()
  a[[1]] <- full_mean
  a[[2]] <- initial_mean
  a[[3]] <- final_mean
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("n_rows", "ncols", "sam_full", "sam_final", "sam_initial"))
}

sam_ext <- sam_ext_fun(sam_full, sam_initial, sam_final) 

rm(list = c("sam_full", "sam_final", "sam_initial", "sam_ext_fun"))

save(sam_ext, file = "sc1_sam_extrapolations_mean.RData")

############################ MISTNET ###################################

load("sc1_mistnet.RData")

# Full

mnp_full_fun <- function(model_par){
  
  library(mistnet)
  
  fit <- model_par$mistnet_fit
  
  load("extrapolation_data.RData")
  
  full <- ext_data$full[3:5]
  
  mpred <- predict(fit, as.matrix(na.omit(full)), n.importance.samples = 25)
  full_p <- apply(mpred, 1:2, mean)
  colnames(full_p) <- c(paste0("spp", 1:18))
  
  return(full_p)
  
  rm(list = c("fit", "full", "mpred"))
}
cl <- parallel::makeCluster(detectCores()-1)
mistnet_full <- parallel::parLapply(cl, mistnet_par, mnp_full_fun)
parallel::stopCluster(cl)

# Initial

mnp_initial_fun <- function(model_par){
  
  library(mistnet)
  
  fit <- model_par$mistnet_fit
  
  load("extrapolation_data.RData")
  
  initial <- ext_data$initial[3:5]
  
  mpred <- predict(fit, as.matrix(na.omit(initial)), n.importance.samples = 25)
  initial_p <- apply(mpred, 1:2, mean)
  colnames(initial_p) <- c(paste0("spp", 1:18))
  
  return(initial_p)
  
  rm(list = c("fit", "initial"))
}
cl <- parallel::makeCluster(detectCores()-1)
mistnet_initial <- parallel::parLapply(cl, mistnet_par, mnp_initial_fun)
parallel::stopCluster(cl)

# Final

mnp_final_fun <- function(model_par){
  
  library(mistnet)
  
  fit <- model_par$mistnet_fit
  
  load("extrapolation_data.RData")
  
  final <- ext_data$final[3:5]
  
  mpred <- predict(fit, as.matrix(na.omit(final)), n.importance.samples = 25)
  final_p <- apply(mpred, 1:2, mean)
  colnames(final_p) <- c(paste0("spp", 1:18))
  
  return(final_p)
  
  rm(list = c("fit", "final"))
}
cl <- parallel::makeCluster(detectCores()-1)
mistnet_final <- parallel::parLapply(cl, mistnet_par, mnp_final_fun)
parallel::stopCluster(cl)

rm(list = c("cl", "mnp_full_fun", "mnp_initial_fun", "mnp_final_fun", "mistnet_par"))

save(mistnet_final, mistnet_full, mistnet_initial, file = "sc1_mistnet_extrapolations.RData")

# Mean

mistnet_ext_fun <- function(model_full, model_initial, model_final){
  
  n_rows <- nrow(mistnet_full[[1]])
  n_cols <- ncol(mistnet_full[[1]])
  
  full_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(full_mean) <- colnames(mistnet_full[[1]])
  
  initial_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(initial_mean) <- colnames(mistnet_initial[[1]])
  
  final_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(final_mean) <- colnames(mistnet_final[[1]])
  
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_full <- sapply(mistnet_full, function(mat) mat[i, j])
      full_mean[i, j] <- mean(values_full)
      
      values_initial <- sapply(mistnet_initial, function(mat) mat[i, j])
      initial_mean[i, j] <- mean(values_initial)
      
      values_final <- sapply(mistnet_final, function(mat) mat[i, j])
      final_mean[i, j] <- mean(values_final)
    }
  }
  
  a <- list()
  a[[1]] <- full_mean
  a[[2]] <- initial_mean
  a[[3]] <- final_mean
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("n_rows", "ncols"))
}

mistnet_ext <- mistnet_ext_fun(mistnet_full, mistnet_initial, mistnet_final) 

rm(list = c("mistnet_full", "mistnet_final", "mistnet_initial", "mistnet_ext_fun"))

save(mistnet_ext, file = "sc1_mistnet_extrapolations_mean.RData")

######################### GLM ################################

load("sc1_glm.RData")

glmp_full_fun <- function(model_par){
  
  fit <- model_par$glm_fit
  
  load("extrapolation_data.RData")
  
  full <- ext_data$full
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(full), type = "response")
  }
  
  glm_pred <- lapply(fit, predict_fun)
  
  glm_full <- do.call(cbind, lapply(glm_pred, as.matrix))
  colnames(glm_full) <- names(glm_pred)
  
  return(glm_full)
  
  rm(list = c("fit", "full", "predict_fun", "glm_pred"))
}
cl <- parallel::makeCluster(detectCores()-1)
glm_full <- parallel::parLapply(cl, glm_par, glmp_full_fun)
parallel::stopCluster(cl)

glmp_initial_fun <- function(model_par){
  
  library(sdm)
  
  fit <- model_par$glm_fit
  
  load("extrapolation_data.RData")
  
  initial <- ext_data$initial
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(initial), type = "response")
  }
  
  glm_pred <- lapply(fit, predict_fun)
  
  glm_initial <- do.call(cbind, lapply(glm_pred, as.matrix))
  colnames(glm_initial) <- names(glm_pred)
  
  return(glm_initial)
  
  rm(list = c("fit", "initial", "predict_fun", "glm_pred"))
  
}
cl <- parallel::makeCluster(detectCores()-1)
glm_initial <- parallel::parLapply(cl, glm_par, glmp_initial_fun)
parallel::stopCluster(cl)

glmp_final_fun <- function(model_par){
  
  library(sdm)
  
  fit <- model_par$glm_fit
  
  load("extrapolation_data.RData")
  
  final <- ext_data$final
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(final), type = "response")
  }
  
  glm_pred <- lapply(fit, predict_fun)
  
  glm_final <- do.call(cbind, lapply(glm_pred, as.matrix))
  colnames(glm_final) <- names(glm_pred)
  
  return(glm_final)
  
  rm(list = c("fit", "final", "predict_fun", "glm_pred"))
  
}
cl <- parallel::makeCluster(detectCores()-1)
glm_final <- parallel::parLapply(cl, glm_par, glmp_final_fun)
parallel::stopCluster(cl)

rm(list = c("cl", "glmp_full_fun", "glmp_initial_fun", "glmp_final_fun", "glm_par"))

save(glm_final, glm_full, glm_initial, file = "sc1_glm_extrapolations.RData")

# Mean

glm_ext_fun <- function(model_full, model_initial, model_final){
  
  n_rows <- nrow(glm_full[[1]])
  n_cols <- ncol(glm_full[[1]])
  
  full_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(full_mean) <- colnames(glm_full[[1]])
  
  initial_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(initial_mean) <- colnames(glm_initial[[1]])
  
  final_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(final_mean) <- colnames(glm_final[[1]])
  
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_full <- sapply(glm_full, function(mat) mat[i, j])
      full_mean[i, j] <- mean(values_full)
      
      values_initial <- sapply(glm_initial, function(mat) mat[i, j])
      initial_mean[i, j] <- mean(values_initial)
      
      values_final <- sapply(glm_final, function(mat) mat[i, j])
      final_mean[i, j] <- mean(values_final)
    }
  }
  
  a <- list()
  a[[1]] <- full_mean
  a[[2]] <- initial_mean
  a[[3]] <- final_mean
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("n_rows", "ncols"))
}

glm_ext <- glm_ext_fun(glm_full, glm_initial, glm_final) 

rm(list = c("glm_full", "glm_final", "glm_initial", "glm_ext_fun"))

save(glm_ext, file = "sc1_glm_extrapolations_mean.RData")

########################### BRT ####################################

load("sc1_brt.RData")

brtp_full_fun <- function(model_par){
  
  library(gbm)
  
  fit <- model_par$brt_fit
  
  load("extrapolation_data.RData")
  
  full <- ext_data$full
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(full), type = "response")
  }
  
  brt_pred <- lapply(fit, predict_fun)
  
  brt_full <- do.call(cbind, lapply(brt_pred, as.matrix))
  colnames(brt_full) <- names(brt_pred)
  
  return(brt_full)
  
  rm(list = c("fit", "full", "predict_fun", "brt_pred"))
}
cl <- parallel::makeCluster(detectCores()-1)
brt_full <- parallel::parLapply(cl, brt_par, brtp_full_fun)
parallel::stopCluster(cl)

brtp_initial_fun <- function(model_par){
  
  library(gbm)
  
  fit <- model_par$brt_fit
  
  load("extrapolation_data.RData")
  
  initial <- ext_data$initial
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(initial), type = "response")
  }
  
  brt_pred <- lapply(fit, predict_fun)
  
  brt_initial <- do.call(cbind, lapply(brt_pred, as.matrix))
  colnames(brt_initial) <- names(brt_pred)
  
  return(brt_initial)
  
  rm(list = c("fit", "initial", "predict_fun", "brt_pred"))
  
}
cl <- parallel::makeCluster(detectCores()-1)
brt_initial <- parallel::parLapply(cl, brt_par, brtp_initial_fun)
parallel::stopCluster(cl)

brtp_final_fun <- function(model_par){
  
  library(gbm)
  
  fit <- model_par$brt_fit
  
  load("extrapolation_data.RData")
  
  final <- ext_data$final
  
  predict_fun <- function(x){
    predict(x, newdata = as.data.frame(final), type = "response")
  }
  
  brt_pred <- lapply(fit, predict_fun)
  
  brt_final <- do.call(cbind, lapply(brt_pred, as.matrix))
  colnames(brt_final) <- names(brt_pred)
  
  return(brt_final)
  
  rm(list = c("fit", "final", "predict_fun", "brt_pred"))
  
}
cl <- parallel::makeCluster(detectCores()-1)
brt_final <- parallel::parLapply(cl, brt_par, brtp_final_fun)
parallel::stopCluster(cl)

rm(list = c("cl", "brtp_full_fun", "brtp_initial_fun", "brtp_final_fun", "brt_par"))

save(brt_final, brt_full, brt_initial, file = "sc1_brt_extrapolations.RData")

# Mean

brt_ext_fun <- function(model_full, model_initial, model_final){
  
  n_rows <- nrow(brt_full[[1]])
  n_cols <- ncol(brt_full[[1]])
  
  full_mean <- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(full_mean) <- colnames(brt_full[[1]])
  
  initial_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(initial_mean) <- colnames(brt_initial[[1]])
  
  final_mean<- matrix(NA, nrow = n_rows, ncol = n_cols)
  colnames(final_mean) <- colnames(brt_final[[1]])
  
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      values_full <- sapply(brt_full, function(mat) mat[i, j])
      full_mean[i, j] <- mean(values_full)
      
      values_initial <- sapply(brt_initial, function(mat) mat[i, j])
      initial_mean[i, j] <- mean(values_initial)
      
      values_final <- sapply(brt_final, function(mat) mat[i, j])
      final_mean[i, j] <- mean(values_final)
    }
  }
  
  a <- list()
  a[[1]] <- full_mean
  a[[2]] <- initial_mean
  a[[3]] <- final_mean
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("n_rows", "ncols"))
}

brt_ext <- brt_ext_fun(brt_full, brt_initial, brt_final) 

rm(list = c("brt_full", "brt_final", "brt_initial", "brt_ext_fun"))

save(brt_ext, file = "sc1_brt_extrapolations_mean.RData")
