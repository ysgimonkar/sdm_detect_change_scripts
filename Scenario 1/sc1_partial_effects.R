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

load("sc1_partial_effects.RData")

response1 <- c(3, 4, 8)
response2 <- c(5, 6, 9, 10, 11, 12, 13, 14, 18)
response3 <- c(2, 15, 16)
response4 <- c(1, 7, 17)

############################ SST ###########################

sam1m <- as.matrix(apply(sam_pp$sam_yy_sst_mean[, response1], 1, mean))
sam1c <- 1.96*as.matrix(apply(sam_pp$sam_yy_sst_mean[, response1], 1, std.error))
sam2m <- as.matrix(apply(sam_pp$sam_yy_sst_mean[, response2], 1, mean))
sam2c <- 1.96*as.matrix(apply(sam_pp$sam_yy_sst_mean[, response2], 1, std.error))
sam3m <- as.matrix(apply(sam_pp$sam_yy_sst_mean[, response3], 1, mean))
sam3c <- 1.96*as.matrix(apply(sam_pp$sam_yy_sst_mean[, response3], 1, std.error))
sam4m <- as.matrix(apply(sam_pp$sam_yy_sst_mean[, response4], 1, mean))
sam4c <- 1.96*as.matrix(apply(sam_pp$sam_yy_sst_mean[, response4], 1, std.error))

boral1m <- as.matrix(apply(boral_pp$boral_yy_sst_mean[, response1], 1, mean))
boral1c <- 1.96*as.matrix(apply(boral_pp$boral_yy_sst_mean[, response1], 1, std.error))
boral2m <- as.matrix(apply(boral_pp$boral_yy_sst_mean[, response2], 1, mean))
boral2c <- 1.96*as.matrix(apply(boral_pp$boral_yy_sst_mean[, response2], 1, std.error))
boral3m <- as.matrix(apply(boral_pp$boral_yy_sst_mean[, response3], 1, mean))
boral3c <- 1.96*as.matrix(apply(boral_pp$boral_yy_sst_mean[, response3], 1, std.error))
boral4m <- as.matrix(apply(boral_pp$boral_yy_sst_mean[, response4], 1, mean))
boral4c <- 1.96*as.matrix(apply(boral_pp$boral_yy_sst_mean[, response4], 1, std.error))

brt1m <- as.matrix(apply(brt_pp$brt_yy_sst_mean[, response1], 1, mean))
brt1c <- 1.96*as.matrix(apply(brt_pp$brt_yy_sst_mean[, response1], 1, std.error))
brt2m <- as.matrix(apply(brt_pp$brt_yy_sst_mean[, response2], 1, mean))
brt2c <- 1.96*as.matrix(apply(brt_pp$brt_yy_sst_mean[, response2], 1, std.error))
brt3m <- as.matrix(apply(brt_pp$brt_yy_sst_mean[, response3], 1, mean))
brt3c <- 1.96*as.matrix(apply(brt_pp$brt_yy_sst_mean[, response3], 1, std.error))
brt4m <- as.matrix(apply(brt_pp$brt_yy_sst_mean[, response4], 1, mean))
brt4c <- 1.96*as.matrix(apply(brt_pp$brt_yy_sst_mean[, response4], 1, std.error))

glm1m <- as.matrix(apply(glm_pp$glm_yy_sst_mean[, response1], 1, mean))
glm1c <- 1.96*as.matrix(apply(glm_pp$glm_yy_sst_mean[, response1], 1, std.error))
glm2m <- as.matrix(apply(glm_pp$glm_yy_sst_mean[, response2], 1, mean))
glm2c <- 1.96*as.matrix(apply(glm_pp$glm_yy_sst_mean[, response2], 1, std.error))
glm3m <- as.matrix(apply(glm_pp$glm_yy_sst_mean[, response3], 1, mean))
glm3c <- 1.96*as.matrix(apply(glm_pp$glm_yy_sst_mean[, response3], 1, std.error))
glm4m <- as.matrix(apply(glm_pp$glm_yy_sst_mean[, response4], 1, mean))
glm4c <- 1.96*as.matrix(apply(glm_pp$glm_yy_sst_mean[, response4], 1, std.error))

hmsc1m <- as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response1], 1, mean))
hmsc1c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response1], 1, std.error))
hmsc2m <- as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response2], 1, mean))
hmsc2c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response2], 1, std.error))
hmsc3m <- as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response3], 1, mean))
hmsc3c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response3], 1, std.error))
hmsc4m <- as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response4], 1, mean))
hmsc4c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_sst_mean[, response4], 1, std.error))

mistnet1m <- as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response1], 1, mean))
mistnet1c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response1], 1, std.error))
mistnet2m <- as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response2], 1, mean))
mistnet2c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response2], 1, std.error))
mistnet3m <- as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response3], 1, mean))
mistnet3c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response3], 1, std.error))
mistnet4m <- as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response4], 1, mean))
mistnet4c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_sst_mean[, response4], 1, std.error))

raw1m <- as.matrix(apply(raw_pp$raw_yy_sst_mean[, response1], 1, mean))
raw1c <- 1.96*as.matrix(apply(raw_pp$raw_yy_sst_mean[, response1], 1, std.error))
raw2m <- as.matrix(apply(raw_pp$raw_yy_sst_mean[, response2], 1, mean))
raw2c <- 1.96*as.matrix(apply(raw_pp$raw_yy_sst_mean[, response2], 1, std.error))
raw3m <- as.matrix(apply(raw_pp$raw_yy_sst_mean[, response3], 1, mean))
raw3c <- 1.96*as.matrix(apply(raw_pp$raw_yy_sst_mean[, response3], 1, std.error))
raw4m <- as.matrix(apply(raw_pp$raw_yy_sst_mean[, response4], 1, mean))
raw4c <- 1.96*as.matrix(apply(raw_pp$raw_yy_sst_mean[, response4], 1, std.error))

sresp1m <- cbind(boral1m, brt1m, glm1m, hmsc1m, mistnet1m, raw1m, sam1m)
colnames(sresp1m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
sresp1c <- cbind(boral1c, brt1c, glm1c, hmsc1c, mistnet1c, raw1c, sam1c)
colnames(sresp1c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

sresp2m <- cbind(boral2m, brt2m, glm2m, hmsc2m, mistnet2m, raw2m, sam2m)
colnames(sresp2m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
sresp2c <- cbind(boral2c, brt2c, glm2c, hmsc2c, mistnet2c, raw2c, sam2c)
colnames(sresp2c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

sresp3m <- cbind(boral3m, brt3m, glm3m, hmsc3m, mistnet3m, raw3m, sam3m)
colnames(sresp3m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
sresp3c <- cbind(boral3c, brt3c, glm3c, hmsc3c, mistnet3c, raw3c, sam3c)
colnames(sresp3c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

sresp4m <- cbind(boral4m, brt4m, glm4m, hmsc4m, mistnet4m, raw4m, sam4m)
colnames(sresp4m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
sresp4c <- cbind(boral4c, brt4c, glm4c, hmsc4c, mistnet4c, raw4c, sam4c)
colnames(sresp4c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

############################ DSS ###########################

sam1m <- as.matrix(apply(sam_pp$sam_yy_dss_mean[, response1], 1, mean))
sam1c <- 1.96*as.matrix(apply(sam_pp$sam_yy_dss_mean[, response1], 1, std.error))
sam2m <- as.matrix(apply(sam_pp$sam_yy_dss_mean[, response2], 1, mean))
sam2c <- 1.96*as.matrix(apply(sam_pp$sam_yy_dss_mean[, response2], 1, std.error))
sam3m <- as.matrix(apply(sam_pp$sam_yy_dss_mean[, response3], 1, mean))
sam3c <- 1.96*as.matrix(apply(sam_pp$sam_yy_dss_mean[, response3], 1, std.error))
sam4m <- as.matrix(apply(sam_pp$sam_yy_dss_mean[, response4], 1, mean))
sam4c <- 1.96*as.matrix(apply(sam_pp$sam_yy_dss_mean[, response4], 1, std.error))

boral1m <- as.matrix(apply(boral_pp$boral_yy_dss_mean[, response1], 1, mean))
boral1c <- 1.96*as.matrix(apply(boral_pp$boral_yy_dss_mean[, response1], 1, std.error))
boral2m <- as.matrix(apply(boral_pp$boral_yy_dss_mean[, response2], 1, mean))
boral2c <- 1.96*as.matrix(apply(boral_pp$boral_yy_dss_mean[, response2], 1, std.error))
boral3m <- as.matrix(apply(boral_pp$boral_yy_dss_mean[, response3], 1, mean))
boral3c <- 1.96*as.matrix(apply(boral_pp$boral_yy_dss_mean[, response3], 1, std.error))
boral4m <- as.matrix(apply(boral_pp$boral_yy_dss_mean[, response4], 1, mean))
boral4c <- 1.96*as.matrix(apply(boral_pp$boral_yy_dss_mean[, response4], 1, std.error))

brt1m <- as.matrix(apply(brt_pp$brt_yy_dss_mean[, response1], 1, mean))
brt1c <- 1.96*as.matrix(apply(brt_pp$brt_yy_dss_mean[, response1], 1, std.error))
brt2m <- as.matrix(apply(brt_pp$brt_yy_dss_mean[, response2], 1, mean))
brt2c <- 1.96*as.matrix(apply(brt_pp$brt_yy_dss_mean[, response2], 1, std.error))
brt3m <- as.matrix(apply(brt_pp$brt_yy_dss_mean[, response3], 1, mean))
brt3c <- 1.96*as.matrix(apply(brt_pp$brt_yy_dss_mean[, response3], 1, std.error))
brt4m <- as.matrix(apply(brt_pp$brt_yy_dss_mean[, response4], 1, mean))
brt4c <- 1.96*as.matrix(apply(brt_pp$brt_yy_dss_mean[, response4], 1, std.error))

glm1m <- as.matrix(apply(glm_pp$glm_yy_dss_mean[, response1], 1, mean))
glm1c <- 1.96*as.matrix(apply(glm_pp$glm_yy_dss_mean[, response1], 1, std.error))
glm2m <- as.matrix(apply(glm_pp$glm_yy_dss_mean[, response2], 1, mean))
glm2c <- 1.96*as.matrix(apply(glm_pp$glm_yy_dss_mean[, response2], 1, std.error))
glm3m <- as.matrix(apply(glm_pp$glm_yy_dss_mean[, response3], 1, mean))
glm3c <- 1.96*as.matrix(apply(glm_pp$glm_yy_dss_mean[, response3], 1, std.error))
glm4m <- as.matrix(apply(glm_pp$glm_yy_dss_mean[, response4], 1, mean))
glm4c <- 1.96*as.matrix(apply(glm_pp$glm_yy_dss_mean[, response4], 1, std.error))

hmsc1m <- as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response1], 1, mean))
hmsc1c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response1], 1, std.error))
hmsc2m <- as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response2], 1, mean))
hmsc2c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response2], 1, std.error))
hmsc3m <- as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response3], 1, mean))
hmsc3c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response3], 1, std.error))
hmsc4m <- as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response4], 1, mean))
hmsc4c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_dss_mean[, response4], 1, std.error))

mistnet1m <- as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response1], 1, mean))
mistnet1c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response1], 1, std.error))
mistnet2m <- as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response2], 1, mean))
mistnet2c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response2], 1, std.error))
mistnet3m <- as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response3], 1, mean))
mistnet3c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response3], 1, std.error))
mistnet4m <- as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response4], 1, mean))
mistnet4c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_dss_mean[, response4], 1, std.error))

raw1m <- as.matrix(apply(raw_pp$raw_yy_dss_mean[, response1], 1, mean))
raw1c <- 1.96*as.matrix(apply(raw_pp$raw_yy_dss_mean[, response1], 1, std.error))
raw2m <- as.matrix(apply(raw_pp$raw_yy_dss_mean[, response2], 1, mean))
raw2c <- 1.96*as.matrix(apply(raw_pp$raw_yy_dss_mean[, response2], 1, std.error))
raw3m <- as.matrix(apply(raw_pp$raw_yy_dss_mean[, response3], 1, mean))
raw3c <- 1.96*as.matrix(apply(raw_pp$raw_yy_dss_mean[, response3], 1, std.error))
raw4m <- as.matrix(apply(raw_pp$raw_yy_dss_mean[, response4], 1, mean))
raw4c <- 1.96*as.matrix(apply(raw_pp$raw_yy_dss_mean[, response4], 1, std.error))

dresp1m <- cbind(boral1m, brt1m, glm1m, hmsc1m, mistnet1m, raw1m, sam1m)
colnames(dresp1m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
dresp1c <- cbind(boral1c, brt1c, glm1c, hmsc1c, mistnet1c, raw1c, sam1c)
colnames(dresp1c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

dresp2m <- cbind(boral2m, brt2m, glm2m, hmsc2m, mistnet2m, raw2m, sam2m)
colnames(dresp2m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
dresp2c <- cbind(boral2c, brt2c, glm2c, hmsc2c, mistnet2c, raw2c, sam2c)
colnames(dresp2c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

dresp3m <- cbind(boral3m, brt3m, glm3m, hmsc3m, mistnet3m, raw3m, sam3m)
colnames(dresp3m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
dresp3c <- cbind(boral3c, brt3c, glm3c, hmsc3c, mistnet3c, raw3c, sam3c)
colnames(dresp3c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

dresp4m <- cbind(boral4m, brt4m, glm4m, hmsc4m, mistnet4m, raw4m, sam4m)
colnames(dresp4m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
dresp4c <- cbind(boral4c, brt4c, glm4c, hmsc4c, mistnet4c, raw4c, sam4c)
colnames(dresp4c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")


############################ CHL ###########################

sam1m <- as.matrix(apply(sam_pp$sam_yy_chl_mean[, response1], 1, mean))
sam1c <- 1.96*as.matrix(apply(sam_pp$sam_yy_chl_mean[, response1], 1, std.error))
sam2m <- as.matrix(apply(sam_pp$sam_yy_chl_mean[, response2], 1, mean))
sam2c <- 1.96*as.matrix(apply(sam_pp$sam_yy_chl_mean[, response2], 1, std.error))
sam3m <- as.matrix(apply(sam_pp$sam_yy_chl_mean[, response3], 1, mean))
sam3c <- 1.96*as.matrix(apply(sam_pp$sam_yy_chl_mean[, response3], 1, std.error))
sam4m <- as.matrix(apply(sam_pp$sam_yy_chl_mean[, response4], 1, mean))
sam4c <- 1.96*as.matrix(apply(sam_pp$sam_yy_chl_mean[, response4], 1, std.error))

boral1m <- as.matrix(apply(boral_pp$boral_yy_chl_mean[, response1], 1, mean))
boral1c <- 1.96*as.matrix(apply(boral_pp$boral_yy_chl_mean[, response1], 1, std.error))
boral2m <- as.matrix(apply(boral_pp$boral_yy_chl_mean[, response2], 1, mean))
boral2c <- 1.96*as.matrix(apply(boral_pp$boral_yy_chl_mean[, response2], 1, std.error))
boral3m <- as.matrix(apply(boral_pp$boral_yy_chl_mean[, response3], 1, mean))
boral3c <- 1.96*as.matrix(apply(boral_pp$boral_yy_chl_mean[, response3], 1, std.error))
boral4m <- as.matrix(apply(boral_pp$boral_yy_chl_mean[, response4], 1, mean))
boral4c <- 1.96*as.matrix(apply(boral_pp$boral_yy_chl_mean[, response4], 1, std.error))

brt1m <- as.matrix(apply(brt_pp$brt_yy_chl_mean[, response1], 1, mean))
brt1c <- 1.96*as.matrix(apply(brt_pp$brt_yy_chl_mean[, response1], 1, std.error))
brt2m <- as.matrix(apply(brt_pp$brt_yy_chl_mean[, response2], 1, mean))
brt2c <- 1.96*as.matrix(apply(brt_pp$brt_yy_chl_mean[, response2], 1, std.error))
brt3m <- as.matrix(apply(brt_pp$brt_yy_chl_mean[, response3], 1, mean))
brt3c <- 1.96*as.matrix(apply(brt_pp$brt_yy_chl_mean[, response3], 1, std.error))
brt4m <- as.matrix(apply(brt_pp$brt_yy_chl_mean[, response4], 1, mean))
brt4c <- 1.96*as.matrix(apply(brt_pp$brt_yy_chl_mean[, response4], 1, std.error))

glm1m <- as.matrix(apply(glm_pp$glm_yy_chl_mean[, response1], 1, mean))
glm1c <- 1.96*as.matrix(apply(glm_pp$glm_yy_chl_mean[, response1], 1, std.error))
glm2m <- as.matrix(apply(glm_pp$glm_yy_chl_mean[, response2], 1, mean))
glm2c <- 1.96*as.matrix(apply(glm_pp$glm_yy_chl_mean[, response2], 1, std.error))
glm3m <- as.matrix(apply(glm_pp$glm_yy_chl_mean[, response3], 1, mean))
glm3c <- 1.96*as.matrix(apply(glm_pp$glm_yy_chl_mean[, response3], 1, std.error))
glm4m <- as.matrix(apply(glm_pp$glm_yy_chl_mean[, response4], 1, mean))
glm4c <- 1.96*as.matrix(apply(glm_pp$glm_yy_chl_mean[, response4], 1, std.error))

hmsc1m <- as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response1], 1, mean))
hmsc1c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response1], 1, std.error))
hmsc2m <- as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response2], 1, mean))
hmsc2c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response2], 1, std.error))
hmsc3m <- as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response3], 1, mean))
hmsc3c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response3], 1, std.error))
hmsc4m <- as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response4], 1, mean))
hmsc4c <- 1.96*as.matrix(apply(hmsc_pp$hmsc_yy_chl_mean[, response4], 1, std.error))

mistnet1m <- as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response1], 1, mean))
mistnet1c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response1], 1, std.error))
mistnet2m <- as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response2], 1, mean))
mistnet2c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response2], 1, std.error))
mistnet3m <- as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response3], 1, mean))
mistnet3c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response3], 1, std.error))
mistnet4m <- as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response4], 1, mean))
mistnet4c <- 1.96*as.matrix(apply(mistnet_pp$mn_yy_chl_mean[, response4], 1, std.error))

raw1m <- as.matrix(apply(raw_pp$raw_yy_chl_mean[, response1], 1, mean))
raw1c <- 1.96*as.matrix(apply(raw_pp$raw_yy_chl_mean[, response1], 1, std.error))
raw2m <- as.matrix(apply(raw_pp$raw_yy_chl_mean[, response2], 1, mean))
raw2c <- 1.96*as.matrix(apply(raw_pp$raw_yy_chl_mean[, response2], 1, std.error))
raw3m <- as.matrix(apply(raw_pp$raw_yy_chl_mean[, response3], 1, mean))
raw3c <- 1.96*as.matrix(apply(raw_pp$raw_yy_chl_mean[, response3], 1, std.error))
raw4m <- as.matrix(apply(raw_pp$raw_yy_chl_mean[, response4], 1, mean))
raw4c <- 1.96*as.matrix(apply(raw_pp$raw_yy_chl_mean[, response4], 1, std.error))

cresp1m <- cbind(boral1m, brt1m, glm1m, hmsc1m, mistnet1m, raw1m, sam1m)
colnames(cresp1m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
cresp1c <- cbind(boral1c, brt1c, glm1c, hmsc1c, mistnet1c, raw1c, sam1c)
colnames(cresp1c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

cresp2m <- cbind(boral2m, brt2m, glm2m, hmsc2m, mistnet2m, raw2m, sam2m)
colnames(cresp2m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
cresp2c <- cbind(boral2c, brt2c, glm2c, hmsc2c, mistnet2c, raw2c, sam2c)
colnames(cresp2c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

cresp3m <- cbind(boral3m, brt3m, glm3m, hmsc3m, mistnet3m, raw3m, sam3m)
colnames(cresp3m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
cresp3c <- cbind(boral3c, brt3c, glm3c, hmsc3c, mistnet3c, raw3c, sam3c)
colnames(cresp3c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")

cresp4m <- cbind(boral4m, brt4m, glm4m, hmsc4m, mistnet4m, raw4m, sam4m)
colnames(cresp4m) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")
cresp4c <- cbind(boral4c, brt4c, glm4c, hmsc4c, mistnet4c, raw4c, sam4c)
colnames(cresp4c) <- c("boral", "brt", "glm", "hmsc", "mistnet", "raw", "sam")


######################################################################

#################################### Plotting RAW - BORAL ##############################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_sst, rev(boral_pp$boral_xx_sst)), c((sresp1m[, "boral"] + sresp1c[, "boral"]), rev(sresp1m[, "boral"] - sresp1c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_sst, sresp1m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_dss, rev(boral_pp$boral_xx_dss)), c((dresp1m[, "boral"] + dresp1c[, "boral"]), rev(dresp1m[, "boral"] - dresp1c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_dss, dresp1m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(boral_pp$boral_xx_chl, rev(boral_pp$boral_xx_chl)), c((cresp1m[, "boral"] + cresp1c[, "boral"]), rev(cresp1m[, "boral"] - cresp1c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_chl, cresp1m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_sst, rev(boral_pp$boral_xx_sst)), c((sresp2m[, "boral"] + sresp2c[, "boral"]), rev(sresp2m[, "boral"] - sresp2c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_sst, sresp2m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_dss, rev(boral_pp$boral_xx_dss)), c((dresp2m[, "boral"] + dresp2c[, "boral"]), rev(dresp2m[, "boral"] - dresp2c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_dss, dresp2m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_chl, rev(boral_pp$boral_xx_chl)), c((cresp2m[, "boral"] + cresp2c[, "boral"]), rev(cresp2m[, "boral"] - cresp2c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_chl, cresp2m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_sst, rev(boral_pp$boral_xx_sst)), c((sresp3m[, "boral"] + sresp3c[, "boral"]), rev(sresp3m[, "boral"] - sresp3c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_sst, sresp3m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_dss, rev(boral_pp$boral_xx_dss)), c((dresp3m[, "boral"] + dresp3c[, "boral"]), rev(dresp3m[, "boral"] - dresp3c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_dss, dresp3m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_chl, rev(boral_pp$boral_xx_chl)), c((cresp3m[, "boral"] + cresp3c[, "boral"]), rev(cresp3m[, "boral"] - cresp3c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_chl, cresp3m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_sst, rev(boral_pp$boral_xx_sst)), c((sresp4m[, "boral"] + sresp4c[, "boral"]), rev(sresp4m[, "boral"] - sresp4c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_sst, sresp4m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_dss, rev(boral_pp$boral_xx_dss)), c((dresp4m[, "boral"] + dresp4c[, "boral"]), rev(dresp4m[, "boral"] - dresp4c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_dss, dresp4m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(boral_pp$boral_xx_chl, rev(boral_pp$boral_xx_chl)), c((cresp4m[, "boral"] + cresp4c[, "boral"]), rev(cresp4m[, "boral"] - cresp4c[, "boral"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(boral_pp$boral_xx_chl, cresp4m[,"boral"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BORAL"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#################################### Plotting RAW - BRT ##############################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_sst, rev(brt_pp$brt_xx_sst)), c((sresp1m[, "brt"] + sresp1c[, "brt"]), rev(sresp1m[, "brt"] - sresp1c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_sst, sresp1m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_dss, rev(brt_pp$brt_xx_dss)), c((dresp1m[, "brt"] + dresp1c[, "brt"]), rev(dresp1m[, "brt"] - dresp1c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_dss, dresp1m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(brt_pp$brt_xx_chl, rev(brt_pp$brt_xx_chl)), c((cresp1m[, "brt"] + cresp1c[, "brt"]), rev(cresp1m[, "brt"] - cresp1c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_chl, cresp1m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_sst, rev(brt_pp$brt_xx_sst)), c((sresp2m[, "brt"] + sresp2c[, "brt"]), rev(sresp2m[, "brt"] - sresp2c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_sst, sresp2m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_dss, rev(brt_pp$brt_xx_dss)), c((dresp2m[, "brt"] + dresp2c[, "brt"]), rev(dresp2m[, "brt"] - dresp2c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_dss, dresp2m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_chl, rev(brt_pp$brt_xx_chl)), c((cresp2m[, "brt"] + cresp2c[, "brt"]), rev(cresp2m[, "brt"] - cresp2c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_chl, cresp2m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_sst, rev(brt_pp$brt_xx_sst)), c((sresp3m[, "brt"] + sresp3c[, "brt"]), rev(sresp3m[, "brt"] - sresp3c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_sst, sresp3m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_dss, rev(brt_pp$brt_xx_dss)), c((dresp3m[, "brt"] + dresp3c[, "brt"]), rev(dresp3m[, "brt"] - dresp3c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_dss, dresp3m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_chl, rev(brt_pp$brt_xx_chl)), c((cresp3m[, "brt"] + cresp3c[, "brt"]), rev(cresp3m[, "brt"] - cresp3c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_chl, cresp3m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_sst, rev(brt_pp$brt_xx_sst)), c((sresp4m[, "brt"] + sresp4c[, "brt"]), rev(sresp4m[, "brt"] - sresp4c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_sst, sresp4m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_dss, rev(brt_pp$brt_xx_dss)), c((dresp4m[, "brt"] + dresp4c[, "brt"]), rev(dresp4m[, "brt"] - dresp4c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_dss, dresp4m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(brt_pp$brt_xx_chl, rev(brt_pp$brt_xx_chl)), c((cresp4m[, "brt"] + cresp4c[, "brt"]), rev(cresp4m[, "brt"] - cresp4c[, "brt"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(brt_pp$brt_xx_chl, cresp4m[,"brt"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "BRT"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

############################## Plotting Raw - GLM ###################################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_sst, rev(glm_pp$glm_xx_sst)), c((sresp1m[, "glm"] + sresp1c[, "glm"]), rev(sresp1m[, "glm"] - sresp1c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_sst, sresp1m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_dss, rev(glm_pp$glm_xx_dss)), c((dresp1m[, "glm"] + dresp1c[, "glm"]), rev(dresp1m[, "glm"] - dresp1c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_dss, dresp1m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(glm_pp$glm_xx_chl, rev(glm_pp$glm_xx_chl)), c((cresp1m[, "glm"] + cresp1c[, "glm"]), rev(cresp1m[, "glm"] - cresp1c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_chl, cresp1m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_sst, rev(glm_pp$glm_xx_sst)), c((sresp2m[, "glm"] + sresp2c[, "glm"]), rev(sresp2m[, "glm"] - sresp2c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_sst, sresp2m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_dss, rev(glm_pp$glm_xx_dss)), c((dresp2m[, "glm"] + dresp2c[, "glm"]), rev(dresp2m[, "glm"] - dresp2c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_dss, dresp2m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_chl, rev(glm_pp$glm_xx_chl)), c((cresp2m[, "glm"] + cresp2c[, "glm"]), rev(cresp2m[, "glm"] - cresp2c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_chl, cresp2m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_sst, rev(glm_pp$glm_xx_sst)), c((sresp3m[, "glm"] + sresp3c[, "glm"]), rev(sresp3m[, "glm"] - sresp3c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_sst, sresp3m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_dss, rev(glm_pp$glm_xx_dss)), c((dresp3m[, "glm"] + dresp3c[, "glm"]), rev(dresp3m[, "glm"] - dresp3c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_dss, dresp3m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_chl, rev(glm_pp$glm_xx_chl)), c((cresp3m[, "glm"] + cresp3c[, "glm"]), rev(cresp3m[, "glm"] - cresp3c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_chl, cresp3m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_sst, rev(glm_pp$glm_xx_sst)), c((sresp4m[, "glm"] + sresp4c[, "glm"]), rev(sresp4m[, "glm"] - sresp4c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_sst, sresp4m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_dss, rev(glm_pp$glm_xx_dss)), c((dresp4m[, "glm"] + dresp4c[, "glm"]), rev(dresp4m[, "glm"] - dresp4c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_dss, dresp4m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(glm_pp$glm_xx_chl, rev(glm_pp$glm_xx_chl)), c((cresp4m[, "glm"] + cresp4c[, "glm"]), rev(cresp4m[, "glm"] - cresp4c[, "glm"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(glm_pp$glm_xx_chl, cresp4m[,"glm"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "GLM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

############################## Plotting Raw - HMSC ###################################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_sst, rev(hmsc_pp$hmsc_xx_sst)), c((sresp1m[, "hmsc"] + sresp1c[, "hmsc"]), rev(sresp1m[, "hmsc"] - sresp1c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_sst, sresp1m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_dss, rev(hmsc_pp$hmsc_xx_dss)), c((dresp1m[, "hmsc"] + dresp1c[, "hmsc"]), rev(dresp1m[, "hmsc"] - dresp1c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_dss, dresp1m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_chl, rev(hmsc_pp$hmsc_xx_chl)), c((cresp1m[, "hmsc"] + cresp1c[, "hmsc"]), rev(cresp1m[, "hmsc"] - cresp1c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_chl, cresp1m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_sst, rev(hmsc_pp$hmsc_xx_sst)), c((sresp2m[, "hmsc"] + sresp2c[, "hmsc"]), rev(sresp2m[, "hmsc"] - sresp2c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_sst, sresp2m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_dss, rev(hmsc_pp$hmsc_xx_dss)), c((dresp2m[, "hmsc"] + dresp2c[, "hmsc"]), rev(dresp2m[, "hmsc"] - dresp2c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_dss, dresp2m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_chl, rev(hmsc_pp$hmsc_xx_chl)), c((cresp2m[, "hmsc"] + cresp2c[, "hmsc"]), rev(cresp2m[, "hmsc"] - cresp2c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_chl, cresp2m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_sst, rev(hmsc_pp$hmsc_xx_sst)), c((sresp3m[, "hmsc"] + sresp3c[, "hmsc"]), rev(sresp3m[, "hmsc"] - sresp3c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_sst, sresp3m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_dss, rev(hmsc_pp$hmsc_xx_dss)), c((dresp3m[, "hmsc"] + dresp3c[, "hmsc"]), rev(dresp3m[, "hmsc"] - dresp3c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_dss, dresp3m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_chl, rev(hmsc_pp$hmsc_xx_chl)), c((cresp3m[, "hmsc"] + cresp3c[, "hmsc"]), rev(cresp3m[, "hmsc"] - cresp3c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_chl, cresp3m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_sst, rev(hmsc_pp$hmsc_xx_sst)), c((sresp4m[, "hmsc"] + sresp4c[, "hmsc"]), rev(sresp4m[, "hmsc"] - sresp4c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_sst, sresp4m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_dss, rev(hmsc_pp$hmsc_xx_dss)), c((dresp4m[, "hmsc"] + dresp4c[, "hmsc"]), rev(dresp4m[, "hmsc"] - dresp4c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_dss, dresp4m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(hmsc_pp$hmsc_xx_chl, rev(hmsc_pp$hmsc_xx_chl)), c((cresp4m[, "hmsc"] + cresp4c[, "hmsc"]), rev(cresp4m[, "hmsc"] - cresp4c[, "hmsc"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(hmsc_pp$hmsc_xx_chl, cresp4m[,"hmsc"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "HMSC"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

############################## Plotting Raw - MistNet ###################################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((sresp1m[, "mistnet"] + sresp1c[, "mistnet"]), rev(sresp1m[, "mistnet"] - sresp1c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, sresp1m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((dresp1m[, "mistnet"] + dresp1c[, "mistnet"]), rev(dresp1m[, "mistnet"] - dresp1c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, dresp1m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((cresp1m[, "mistnet"] + cresp1c[, "mistnet"]), rev(cresp1m[, "mistnet"] - cresp1c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, cresp1m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((sresp2m[, "mistnet"] + sresp2c[, "mistnet"]), rev(sresp2m[, "mistnet"] - sresp2c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, sresp2m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((dresp2m[, "mistnet"] + dresp2c[, "mistnet"]), rev(dresp2m[, "mistnet"] - dresp2c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, dresp2m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((cresp2m[, "mistnet"] + cresp2c[, "mistnet"]), rev(cresp2m[, "mistnet"] - cresp2c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, cresp2m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((sresp3m[, "mistnet"] + sresp3c[, "mistnet"]), rev(sresp3m[, "mistnet"] - sresp3c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, sresp3m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((dresp3m[, "mistnet"] + dresp3c[, "mistnet"]), rev(dresp3m[, "mistnet"] - dresp3c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, dresp3m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((cresp3m[, "mistnet"] + cresp3c[, "mistnet"]), rev(cresp3m[, "mistnet"] - cresp3c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, cresp3m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((sresp4m[, "mistnet"] + sresp4c[, "mistnet"]), rev(sresp4m[, "mistnet"] - sresp4c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, sresp4m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((dresp4m[, "mistnet"] + dresp4c[, "mistnet"]), rev(dresp4m[, "mistnet"] - dresp4c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, dresp4m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(mistnet_pp$mn_xx_sst, rev(mistnet_pp$mn_xx_sst)), c((cresp4m[, "mistnet"] + cresp4c[, "mistnet"]), rev(cresp4m[, "mistnet"] - cresp4c[, "mistnet"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(mistnet_pp$mn_xx_sst, cresp4m[,"mistnet"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "MISTNET"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

############################# Plotting Raw - SAM #####################################

par(mfrow = c(1,3))
#1
matplot(raw_pp$raw_xx_sst, sresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp1m[, "raw"] + sresp1c[, "raw"]), rev(sresp1m[, "raw"] - sresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_sst, rev(sam_pp$sam_xx_sst)), c((sresp1m[, "sam"] + sresp1c[, "sam"]), rev(sresp1m[, "sam"] - sresp1c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_sst, sresp1m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp1m[, "raw"] + dresp1c[, "raw"]), rev(dresp1m[, "raw"] - dresp1c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_dss, rev(sam_pp$sam_xx_dss)), c((dresp1m[, "sam"] + dresp1c[, "sam"]), rev(dresp1m[, "sam"] - dresp1c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_dss, dresp1m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp1m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp1m[, "raw"] + cresp1c[, "raw"]), rev(cresp1m[, "raw"] - cresp1c[, "raw"])), col = "grey70", border = FALSE)
polygon(c(sam_pp$sam_xx_chl, rev(sam_pp$sam_xx_chl)), c((cresp1m[, "sam"] + cresp1c[, "sam"]), rev(cresp1m[, "sam"] - cresp1c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp1m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_chl, cresp1m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#2
matplot(raw_pp$raw_xx_sst, sresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp2m[, "raw"] + sresp2c[, "raw"]), rev(sresp2m[, "raw"] - sresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_sst, rev(sam_pp$sam_xx_sst)), c((sresp2m[, "sam"] + sresp2c[, "sam"]), rev(sresp2m[, "sam"] - sresp2c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_sst, sresp2m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp2m[, "raw"] + dresp2c[, "raw"]), rev(dresp2m[, "raw"] - dresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_dss, rev(sam_pp$sam_xx_dss)), c((dresp2m[, "sam"] + dresp2c[, "sam"]), rev(dresp2m[, "sam"] - dresp2c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_dss, dresp2m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp2m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp2m[, "raw"] + cresp2c[, "raw"]), rev(cresp2m[, "raw"] - cresp2c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_chl, rev(sam_pp$sam_xx_chl)), c((cresp2m[, "sam"] + cresp2c[, "sam"]), rev(cresp2m[, "sam"] - cresp2c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp2m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_chl, cresp2m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#3
matplot(raw_pp$raw_xx_sst, sresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp3m[, "raw"] + sresp3c[, "raw"]), rev(sresp3m[, "raw"] - sresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_sst, rev(sam_pp$sam_xx_sst)), c((sresp3m[, "sam"] + sresp3c[, "sam"]), rev(sresp3m[, "sam"] - sresp3c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_sst, sresp3m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp3m[, "raw"] + dresp3c[, "raw"]), rev(dresp3m[, "raw"] - dresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_dss, rev(sam_pp$sam_xx_dss)), c((dresp3m[, "sam"] + dresp3c[, "sam"]), rev(dresp3m[, "sam"] - dresp3c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_dss, dresp3m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp3m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp3m[, "raw"] + cresp3c[, "raw"]), rev(cresp3m[, "raw"] - cresp3c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_chl, rev(sam_pp$sam_xx_chl)), c((cresp3m[, "sam"] + cresp3c[, "sam"]), rev(cresp3m[, "sam"] - cresp3c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp3m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_chl, cresp3m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

#4
matplot(raw_pp$raw_xx_sst, sresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Sea-Surface Temperature", ylab = "Probability")
polygon(c(raw_pp$raw_xx_sst, rev(raw_pp$raw_xx_sst)), c((sresp4m[, "raw"] + sresp4c[, "raw"]), rev(sresp4m[, "raw"] - sresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_sst, rev(sam_pp$sam_xx_sst)), c((sresp4m[, "sam"] + sresp4c[, "sam"]), rev(sresp4m[, "sam"] - sresp4c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_sst, sresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_sst, sresp4m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_dss, dresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Days since Start", ylab = "")
polygon(c(raw_pp$raw_xx_dss, rev(raw_pp$raw_xx_dss)), c((dresp4m[, "raw"] + dresp4c[, "raw"]), rev(dresp4m[, "raw"] - dresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_dss, rev(sam_pp$sam_xx_dss)), c((dresp4m[, "sam"] + dresp4c[, "sam"]), rev(dresp4m[, "sam"] - dresp4c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_dss, dresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_dss, dresp4m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

matplot(raw_pp$raw_xx_chl, cresp4m[, "raw"], type = "l", lwd = 2.5, pch = NULL,
        col = "black", ylim = c(0,1), xlab ="Chlorophyll a", ylab = "")
polygon(c(raw_pp$raw_xx_chl, rev(raw_pp$raw_xx_chl)), c((cresp4m[, "raw"] + cresp4c[, "raw"]), rev(cresp4m[, "raw"] - cresp4c[, "raw"])), col = "grey75", border = FALSE)
polygon(c(sam_pp$sam_xx_chl, rev(sam_pp$sam_xx_chl)), c((cresp4m[, "sam"] + cresp4c[, "sam"]), rev(cresp4m[, "sam"] - cresp4c[, "sam"])), col = adjustcolor("salmon", alpha = 0.4), border = FALSE)
matlines(raw_pp$raw_xx_chl, cresp4m[,"raw"], type = "l", lwd = 2.5, col = "black")
matlines(sam_pp$sam_xx_chl, cresp4m[,"sam"], type = "l", lwd = 2.5, col = "red")
# legend("top", legend = c("Actual", "SAM"), lty = 1, lwd = 2.5, col = c("black", "red"), cex = 0.8,horiz = TRUE)

###########################################################################






























