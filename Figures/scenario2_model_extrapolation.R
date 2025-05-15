# loading libraries
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
library(viridis)
library(paletteer)

############################ RASTER ######################

ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

r_obj <- rast(xmin=53, xmax=160, ymin= -69, ymax= -38) 
res(r_obj) <- res(ras_stack)
crs(r_obj) <- crs(ras_stack)

load("extrapolation_data.RData")

full <- ext_data$full
location <- as.matrix(full[,c(1,2)])

rm(list = c("ras_stack", "full", "ext_data"))

############################ LOAD RDATA FILES ####################################

load("sc2_boral_extrapolations_mean.RData")
load("sc2_brt_extrapolations_mean.RData")
load("sc2_glm_extrapolations_mean.RData")
load("sc2_hmsc_extrapolations_mean.RData")
load("sc2_mistnet_extrapolations_mean.RData")
load("sc2_sam_extrapolations_mean.RData")
load("sc2_raw_extrapolations.RData")

########################### EXTRAPOLATION FUNCTION #################################

ext_fun <- function(model_ext){
  
  response1 <- c(7, 9, 18)
  response2 <- c(3, 4, 5, 8, 11, 14, 15, 17)
  response3 <- c(2, 6, 10, 16)
  response4 <- c(1, 12, 13)
  
  full <- model_ext$full
  initial <- model_ext$initial
  final <- model_ext$final
  
  #full
  res1_mean <- apply(full[, response1], 1, mean)
  res1_mean_low <- apply(full[, response1], 1, mean) - 1.96*apply(full[, response1], 1, std.error)
  res1_mean_high <- apply(full[, response1], 1, mean) + 1.96*apply(full[, response1], 1, std.error)
  res2_mean <- apply(full[, response2], 1, mean)
  res2_mean_low <- apply(full[, response2], 1, mean) - 1.96*apply(full[, response2], 1, std.error)
  res2_mean_high <- apply(full[, response2], 1, mean) + 1.96*apply(full[, response2], 1, std.error)
  res3_mean <- apply(full[, response3], 1, mean)
  res3_mean_low <- apply(full[, response3], 1, mean) - 1.96*apply(full[, response3], 1, std.error)
  res3_mean_high <- apply(full[, response3], 1, mean) + 1.96*apply(full[, response3], 1, std.error)
  res4_mean <- apply(full[, response4], 1, mean)
  res4_mean_low <- apply(full[, response4], 1, mean) - 1.96*apply(full[, response4], 1, std.error)
  res4_mean_high <- apply(full[, response4], 1, mean) + 1.96*apply(full[, response4], 1, std.error)
  
  full_mat <- cbind(res1_mean, res1_mean_low, res1_mean_high, 
                    res2_mean, res2_mean_low, res2_mean_high, 
                    res3_mean, res3_mean_low, res3_mean_high, 
                    res4_mean, res4_mean_low, res4_mean_high)
  
  #initial
  res1_mean <- apply(initial[, response1], 1, mean)
  res1_mean_low <- apply(initial[, response1], 1, mean) - 1.96*apply(initial[, response1], 1, std.error)
  res1_mean_high <- apply(initial[, response1], 1, mean) + 1.96*apply(initial[, response1], 1, std.error)
  res2_mean <- apply(initial[, response2], 1, mean)
  res2_mean_low <- apply(initial[, response2], 1, mean) - 1.96*apply(initial[, response2], 1, std.error)
  res2_mean_high <- apply(initial[, response2], 1, mean) + 1.96*apply(initial[, response2], 1, std.error)
  res3_mean <- apply(initial[, response3], 1, mean)
  res3_mean_low <- apply(initial[, response3], 1, mean) - 1.96*apply(initial[, response3], 1, std.error)
  res3_mean_high <- apply(initial[, response3], 1, mean) + 1.96*apply(initial[, response3], 1, std.error)
  res4_mean <- apply(initial[, response4], 1, mean)
  res4_mean_low <- apply(initial[, response4], 1, mean) - 1.96*apply(initial[, response4], 1, std.error)
  res4_mean_high <- apply(initial[, response4], 1, mean) + 1.96*apply(initial[, response4], 1, std.error)
  
  initial_mat <- cbind(res1_mean, res1_mean_low, res1_mean_high, 
                       res2_mean, res2_mean_low, res2_mean_high, 
                       res3_mean, res3_mean_low, res3_mean_high, 
                       res4_mean, res4_mean_low, res4_mean_high)
  
  #final
  res1_mean <- apply(final[, response1], 1, mean)
  res1_mean_low <- apply(final[, response1], 1, mean) - 1.96*apply(final[, response1], 1, std.error)
  res1_mean_high <- apply(final[, response1], 1, mean) + 1.96*apply(final[, response1], 1, std.error)
  res2_mean <- apply(final[, response2], 1, mean)
  res2_mean_low <- apply(final[, response2], 1, mean) - 1.96*apply(final[, response2], 1, std.error)
  res2_mean_high <- apply(final[, response2], 1, mean) + 1.96*apply(final[, response2], 1, std.error)
  res3_mean <- apply(final[, response3], 1, mean)
  res3_mean_low <- apply(final[, response3], 1, mean) - 1.96*apply(final[, response3], 1, std.error)
  res3_mean_high <- apply(final[, response3], 1, mean) + 1.96*apply(final[, response3], 1, std.error)
  res4_mean <- apply(final[, response4], 1, mean)
  res4_mean_low <- apply(final[, response4], 1, mean) - 1.96*apply(final[, response4], 1, std.error)
  res4_mean_high <- apply(final[, response4], 1, mean) + 1.96*apply(final[, response4], 1, std.error)
  
  final_mat <-  cbind(res1_mean, res1_mean_low, res1_mean_high, 
                      res2_mean, res2_mean_low, res2_mean_high, 
                      res3_mean, res3_mean_low, res3_mean_high, 
                      res4_mean, res4_mean_low, res4_mean_high)
  
  a <- list()
  a[[1]] <- full_mat
  a[[2]] <- initial_mat
  a[[3]] <- final_mat
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("response1", "response2", "response3", "response4", "full", "inital", "final",
              "res1_mean", "res1_mean_low", "res1_mean_high", 
              "res2_mean", "res2_mean_low", "res2_mean_high", 
              "res3_mean", "res3_mean_low", "res3_mean_high", 
              "res4_mean", "res4_mean_low", "res4_mean_high",
              "full_mat", "intial_mat", "final_mat"))
}

########################## MODEL EXTRAPOLATIONS #####################################

boral_ras <- ext_fun(boral_ext)
brt_ras <- ext_fun(brt_ext)
glm_ras <- ext_fun(glm_ext)
hmsc_ras <- ext_fun(hmsc_ext)
mistnet_ras <- ext_fun(mistnet_ext)
sam_ras <- ext_fun(sam_ext)
raw_ras <- ext_fun(raw_ext)


save(boral_ras, brt_ras, glm_ras, hmsc_ras, mistnet_ras, sam_ras, raw_ras,
     file = "sc2_model_extrapolations.RData")

