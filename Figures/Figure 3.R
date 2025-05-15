# Partial efects plots

library(tidyverse)
library(ggpattern)
library(plotrix)


######################### Scenario 1 ################################

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

######################### Plotting #################################

### Response 1 ###

cols <- c("blue", "deeppink", "#00ff00", "black", "orange", "dodgerblue")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp1m[, "brt"] - sresp1c[, "brt"], ymax = sresp1m[, "brt"] + sresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp1m[, "glm"] - sresp1c[, "glm"], ymax = sresp1m[,"glm"] + sresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp1m[, "mistnet"] - sresp1c[, "mistnet"], ymax = sresp1m[, "mistnet"] + sresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp1m[, "boral"] - sresp1c[, "boral"], ymax = sresp1m[, "boral"] + sresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp1m[, "hmsc"] - sresp1c[, "hmsc"], ymax = sresp1m[, "hmsc"] + sresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp1m[, "sam"] - sresp1c[, "sam"], ymax = sresp1m[, "sam"] + sresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp1m[, "raw"] - sresp1c[, "raw"], ymax = sresp1m[, "raw"] + sresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp1m[, "brt"] - dresp1c[, "brt"], ymax = dresp1m[, "brt"] + dresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp1m[, "glm"] - dresp1c[, "glm"], ymax = dresp1m[, "glm"] + dresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp1m[, "mistnet"] - dresp1c[, "mistnet"], ymax = dresp1m[, "mistnet"] + dresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp1m[, "boral"] - dresp1c[, "boral"], ymax = dresp1m[, "boral"] + dresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp1m[, "hmsc"] - dresp1c[, "hmsc"], ymax = dresp1m[, "hmsc"] + dresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp1m[, "sam"] - dresp1c[, "sam"], ymax = dresp1m[, "sam"] + dresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp1m[, "raw"] - dresp1c[, "raw"], ymax = dresp1m[, "raw"] + dresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp1m[, "brt"] - cresp1c[, "brt"], ymax = cresp1m[, "brt"] + cresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp1m[, "glm"] - cresp1c[, "glm"], ymax = cresp1m[, "glm"] + cresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp1m[, "mistnet"] - cresp1c[, "mistnet"], ymax = cresp1m[, "mistnet"] + cresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp1m[, "boral"] - cresp1c[, "boral"], ymax = cresp1m[, "boral"] + cresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp1m[, "hmsc"] - cresp1c[, "hmsc"], ymax = cresp1m[, "hmsc"] + cresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp1m[, "sam"] - cresp1c[, "sam"], ymax = cresp1m[, "sam"] + cresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp1m[, "raw"] - cresp1c[, "raw"], ymax = cresp1m[, "raw"] + cresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 2 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp2m[, "brt"] - sresp2c[, "brt"], ymax = sresp2m[, "brt"] + sresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp2m[, "glm"] - sresp2c[, "glm"], ymax = sresp2m[,"glm"] + sresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp2m[, "mistnet"] - sresp2c[, "mistnet"], ymax = sresp2m[, "mistnet"] + sresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp2m[, "boral"] - sresp2c[, "boral"], ymax = sresp2m[, "boral"] + sresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp2m[, "hmsc"] - sresp2c[, "hmsc"], ymax = sresp2m[, "hmsc"] + sresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp2m[, "sam"] - sresp2c[, "sam"], ymax = sresp2m[, "sam"] + sresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp2m[, "raw"] - sresp2c[, "raw"], ymax = sresp2m[, "raw"] + sresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp2m[, "brt"] - dresp2c[, "brt"], ymax = dresp2m[, "brt"] + dresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp2m[, "glm"] - dresp2c[, "glm"], ymax = dresp2m[, "glm"] + dresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp2m[, "mistnet"] - dresp2c[, "mistnet"], ymax = dresp2m[, "mistnet"] + dresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp2m[, "boral"] - dresp2c[, "boral"], ymax = dresp2m[, "boral"] + dresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp2m[, "hmsc"] - dresp2c[, "hmsc"], ymax = dresp2m[, "hmsc"] + dresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp2m[, "sam"] - dresp2c[, "sam"], ymax = dresp2m[, "sam"] + dresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp2m[, "raw"] - dresp2c[, "raw"], ymax = dresp2m[, "raw"] + dresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp2m[, "brt"] - cresp2c[, "brt"], ymax = cresp2m[, "brt"] + cresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp2m[, "glm"] - cresp2c[, "glm"], ymax = cresp2m[, "glm"] + cresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp2m[, "mistnet"] - cresp2c[, "mistnet"], ymax = cresp2m[, "mistnet"] + cresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp2m[, "boral"] - cresp2c[, "boral"], ymax = cresp2m[, "boral"] + cresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp2m[, "hmsc"] - cresp2c[, "hmsc"], ymax = cresp2m[, "hmsc"] + cresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp2m[, "sam"] - cresp2c[, "sam"], ymax = cresp2m[, "sam"] + cresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp2m[, "raw"] - cresp2c[, "raw"], ymax = cresp2m[, "raw"] + cresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 3 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp3m[, "brt"] - sresp3c[, "brt"], ymax = sresp3m[, "brt"] + sresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp3m[, "glm"] - sresp3c[, "glm"], ymax = sresp3m[,"glm"] + sresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp3m[, "mistnet"] - sresp3c[, "mistnet"], ymax = sresp3m[, "mistnet"] + sresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp3m[, "boral"] - sresp3c[, "boral"], ymax = sresp3m[, "boral"] + sresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp3m[, "hmsc"] - sresp3c[, "hmsc"], ymax = sresp3m[, "hmsc"] + sresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp3m[, "sam"] - sresp3c[, "sam"], ymax = sresp3m[, "sam"] + sresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp3m[, "raw"] - sresp3c[, "raw"], ymax = sresp3m[, "raw"] + sresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp3m[, "brt"] - dresp3c[, "brt"], ymax = dresp3m[, "brt"] + dresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp3m[, "glm"] - dresp3c[, "glm"], ymax = dresp3m[, "glm"] + dresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp3m[, "mistnet"] - dresp3c[, "mistnet"], ymax = dresp3m[, "mistnet"] + dresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp3m[, "boral"] - dresp3c[, "boral"], ymax = dresp3m[, "boral"] + dresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp3m[, "hmsc"] - dresp3c[, "hmsc"], ymax = dresp3m[, "hmsc"] + dresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp3m[, "sam"] - dresp3c[, "sam"], ymax = dresp3m[, "sam"] + dresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp3m[, "raw"] - dresp3c[, "raw"], ymax = dresp3m[, "raw"] + dresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp3m[, "brt"] - cresp3c[, "brt"], ymax = cresp3m[, "brt"] + cresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp3m[, "glm"] - cresp3c[, "glm"], ymax = cresp3m[, "glm"] + cresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp3m[, "mistnet"] - cresp3c[, "mistnet"], ymax = cresp3m[, "mistnet"] + cresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp3m[, "boral"] - cresp3c[, "boral"], ymax = cresp3m[, "boral"] + cresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp3m[, "hmsc"] - cresp3c[, "hmsc"], ymax = cresp3m[, "hmsc"] + cresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp3m[, "sam"] - cresp3c[, "sam"], ymax = cresp3m[, "sam"] + cresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp3m[, "raw"] - cresp3c[, "raw"], ymax = cresp3m[, "raw"] + cresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 4 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp4m[, "brt"] - sresp4c[, "brt"], ymax = sresp4m[, "brt"] + sresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp4m[, "glm"] - sresp4c[, "glm"], ymax = sresp4m[,"glm"] + sresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp4m[, "mistnet"] - sresp4c[, "mistnet"], ymax = sresp4m[, "mistnet"] + sresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp4m[, "boral"] - sresp4c[, "boral"], ymax = sresp4m[, "boral"] + sresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp4m[, "hmsc"] - sresp4c[, "hmsc"], ymax = sresp4m[, "hmsc"] + sresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp4m[, "sam"] - sresp4c[, "sam"], ymax = sresp4m[, "sam"] + sresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp4m[, "raw"] - sresp4c[, "raw"], ymax = sresp4m[, "raw"] + sresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp4m[, "brt"] - dresp4c[, "brt"], ymax = dresp4m[, "brt"] + dresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp4m[, "glm"] - dresp4c[, "glm"], ymax = dresp4m[, "glm"] + dresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp4m[, "mistnet"] - dresp4c[, "mistnet"], ymax = dresp4m[, "mistnet"] + dresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp4m[, "boral"] - dresp4c[, "boral"], ymax = dresp4m[, "boral"] + dresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp4m[, "hmsc"] - dresp4c[, "hmsc"], ymax = dresp4m[, "hmsc"] + dresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp4m[, "sam"] - dresp4c[, "sam"], ymax = dresp4m[, "sam"] + dresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp4m[, "raw"] - dresp4c[, "raw"], ymax = dresp4m[, "raw"] + dresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp4m[, "brt"] - cresp4c[, "brt"], ymax = cresp4m[, "brt"] + cresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp4m[, "glm"] - cresp4c[, "glm"], ymax = cresp4m[, "glm"] + cresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp4m[, "mistnet"] - cresp4c[, "mistnet"], ymax = cresp4m[, "mistnet"] + cresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp4m[, "boral"] - cresp4c[, "boral"], ymax = cresp4m[, "boral"] + cresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp4m[, "hmsc"] - cresp4c[, "hmsc"], ymax = cresp4m[, "hmsc"] + cresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp4m[, "sam"] - cresp4c[, "sam"], ymax = cresp4m[, "sam"] + cresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp4m[, "raw"] - cresp4c[, "raw"], ymax = cresp4m[, "raw"] + cresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

##################################################

######################### Scenario 2 ################################

load("sc2_partial_effects.RData")

response1 <- c(7, 9, 18)
response2 <- c(3, 4, 5, 8, 11, 14, 15, 17)
response3 <- c(2, 6, 10, 16)
response4 <- c(1, 12, 13)

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


######################### Plotting #################################

### Response 1 ###

cols <- c("blue", "deeppink", "#00ff00", "black", "orange", "dodgerblue")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp1m[, "brt"] - sresp1c[, "brt"], ymax = sresp1m[, "brt"] + sresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp1m[, "glm"] - sresp1c[, "glm"], ymax = sresp1m[,"glm"] + sresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp1m[, "mistnet"] - sresp1c[, "mistnet"], ymax = sresp1m[, "mistnet"] + sresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp1m[, "boral"] - sresp1c[, "boral"], ymax = sresp1m[, "boral"] + sresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp1m[, "hmsc"] - sresp1c[, "hmsc"], ymax = sresp1m[, "hmsc"] + sresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp1m[, "sam"] - sresp1c[, "sam"], ymax = sresp1m[, "sam"] + sresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp1m[, "raw"] - sresp1c[, "raw"], ymax = sresp1m[, "raw"] + sresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp1m[, "brt"] - dresp1c[, "brt"], ymax = dresp1m[, "brt"] + dresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp1m[, "glm"] - dresp1c[, "glm"], ymax = dresp1m[, "glm"] + dresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp1m[, "mistnet"] - dresp1c[, "mistnet"], ymax = dresp1m[, "mistnet"] + dresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp1m[, "boral"] - dresp1c[, "boral"], ymax = dresp1m[, "boral"] + dresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp1m[, "hmsc"] - dresp1c[, "hmsc"], ymax = dresp1m[, "hmsc"] + dresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp1m[, "sam"] - dresp1c[, "sam"], ymax = dresp1m[, "sam"] + dresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp1m[, "raw"] - dresp1c[, "raw"], ymax = dresp1m[, "raw"] + dresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp1m[, "brt"] - cresp1c[, "brt"], ymax = cresp1m[, "brt"] + cresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp1m[, "glm"] - cresp1c[, "glm"], ymax = cresp1m[, "glm"] + cresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp1m[, "mistnet"] - cresp1c[, "mistnet"], ymax = cresp1m[, "mistnet"] + cresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp1m[, "boral"] - cresp1c[, "boral"], ymax = cresp1m[, "boral"] + cresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp1m[, "hmsc"] - cresp1c[, "hmsc"], ymax = cresp1m[, "hmsc"] + cresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp1m[, "sam"] - cresp1c[, "sam"], ymax = cresp1m[, "sam"] + cresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp1m[, "raw"] - cresp1c[, "raw"], ymax = cresp1m[, "raw"] + cresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 2 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp2m[, "brt"] - sresp2c[, "brt"], ymax = sresp2m[, "brt"] + sresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp2m[, "glm"] - sresp2c[, "glm"], ymax = sresp2m[,"glm"] + sresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp2m[, "mistnet"] - sresp2c[, "mistnet"], ymax = sresp2m[, "mistnet"] + sresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp2m[, "boral"] - sresp2c[, "boral"], ymax = sresp2m[, "boral"] + sresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp2m[, "hmsc"] - sresp2c[, "hmsc"], ymax = sresp2m[, "hmsc"] + sresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp2m[, "sam"] - sresp2c[, "sam"], ymax = sresp2m[, "sam"] + sresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp2m[, "raw"] - sresp2c[, "raw"], ymax = sresp2m[, "raw"] + sresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp2m[, "brt"] - dresp2c[, "brt"], ymax = dresp2m[, "brt"] + dresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp2m[, "glm"] - dresp2c[, "glm"], ymax = dresp2m[, "glm"] + dresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp2m[, "mistnet"] - dresp2c[, "mistnet"], ymax = dresp2m[, "mistnet"] + dresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp2m[, "boral"] - dresp2c[, "boral"], ymax = dresp2m[, "boral"] + dresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp2m[, "hmsc"] - dresp2c[, "hmsc"], ymax = dresp2m[, "hmsc"] + dresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp2m[, "sam"] - dresp2c[, "sam"], ymax = dresp2m[, "sam"] + dresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp2m[, "raw"] - dresp2c[, "raw"], ymax = dresp2m[, "raw"] + dresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp2m[, "brt"] - cresp2c[, "brt"], ymax = cresp2m[, "brt"] + cresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp2m[, "glm"] - cresp2c[, "glm"], ymax = cresp2m[, "glm"] + cresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp2m[, "mistnet"] - cresp2c[, "mistnet"], ymax = cresp2m[, "mistnet"] + cresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp2m[, "boral"] - cresp2c[, "boral"], ymax = cresp2m[, "boral"] + cresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp2m[, "hmsc"] - cresp2c[, "hmsc"], ymax = cresp2m[, "hmsc"] + cresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp2m[, "sam"] - cresp2c[, "sam"], ymax = cresp2m[, "sam"] + cresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp2m[, "raw"] - cresp2c[, "raw"], ymax = cresp2m[, "raw"] + cresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 3 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp3m[, "brt"] - sresp3c[, "brt"], ymax = sresp3m[, "brt"] + sresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp3m[, "glm"] - sresp3c[, "glm"], ymax = sresp3m[,"glm"] + sresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp3m[, "mistnet"] - sresp3c[, "mistnet"], ymax = sresp3m[, "mistnet"] + sresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp3m[, "boral"] - sresp3c[, "boral"], ymax = sresp3m[, "boral"] + sresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp3m[, "hmsc"] - sresp3c[, "hmsc"], ymax = sresp3m[, "hmsc"] + sresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp3m[, "sam"] - sresp3c[, "sam"], ymax = sresp3m[, "sam"] + sresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp3m[, "raw"] - sresp3c[, "raw"], ymax = sresp3m[, "raw"] + sresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp3m[, "brt"] - dresp3c[, "brt"], ymax = dresp3m[, "brt"] + dresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp3m[, "glm"] - dresp3c[, "glm"], ymax = dresp3m[, "glm"] + dresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp3m[, "mistnet"] - dresp3c[, "mistnet"], ymax = dresp3m[, "mistnet"] + dresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp3m[, "boral"] - dresp3c[, "boral"], ymax = dresp3m[, "boral"] + dresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp3m[, "hmsc"] - dresp3c[, "hmsc"], ymax = dresp3m[, "hmsc"] + dresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp3m[, "sam"] - dresp3c[, "sam"], ymax = dresp3m[, "sam"] + dresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp3m[, "raw"] - dresp3c[, "raw"], ymax = dresp3m[, "raw"] + dresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp3m[, "brt"] - cresp3c[, "brt"], ymax = cresp3m[, "brt"] + cresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp3m[, "glm"] - cresp3c[, "glm"], ymax = cresp3m[, "glm"] + cresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp3m[, "mistnet"] - cresp3c[, "mistnet"], ymax = cresp3m[, "mistnet"] + cresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp3m[, "boral"] - cresp3c[, "boral"], ymax = cresp3m[, "boral"] + cresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp3m[, "hmsc"] - cresp3c[, "hmsc"], ymax = cresp3m[, "hmsc"] + cresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp3m[, "sam"] - cresp3c[, "sam"], ymax = cresp3m[, "sam"] + cresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp3m[, "raw"] - cresp3c[, "raw"], ymax = cresp3m[, "raw"] + cresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 4 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp4m[, "brt"] - sresp4c[, "brt"], ymax = sresp4m[, "brt"] + sresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp4m[, "glm"] - sresp4c[, "glm"], ymax = sresp4m[,"glm"] + sresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp4m[, "mistnet"] - sresp4c[, "mistnet"], ymax = sresp4m[, "mistnet"] + sresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp4m[, "boral"] - sresp4c[, "boral"], ymax = sresp4m[, "boral"] + sresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp4m[, "hmsc"] - sresp4c[, "hmsc"], ymax = sresp4m[, "hmsc"] + sresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp4m[, "sam"] - sresp4c[, "sam"], ymax = sresp4m[, "sam"] + sresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp4m[, "raw"] - sresp4c[, "raw"], ymax = sresp4m[, "raw"] + sresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp4m[, "brt"] - dresp4c[, "brt"], ymax = dresp4m[, "brt"] + dresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp4m[, "glm"] - dresp4c[, "glm"], ymax = dresp4m[, "glm"] + dresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp4m[, "mistnet"] - dresp4c[, "mistnet"], ymax = dresp4m[, "mistnet"] + dresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp4m[, "boral"] - dresp4c[, "boral"], ymax = dresp4m[, "boral"] + dresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp4m[, "hmsc"] - dresp4c[, "hmsc"], ymax = dresp4m[, "hmsc"] + dresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp4m[, "sam"] - dresp4c[, "sam"], ymax = dresp4m[, "sam"] + dresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp4m[, "raw"] - dresp4c[, "raw"], ymax = dresp4m[, "raw"] + dresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp4m[, "brt"] - cresp4c[, "brt"], ymax = cresp4m[, "brt"] + cresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp4m[, "glm"] - cresp4c[, "glm"], ymax = cresp4m[, "glm"] + cresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp4m[, "mistnet"] - cresp4c[, "mistnet"], ymax = cresp4m[, "mistnet"] + cresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp4m[, "boral"] - cresp4c[, "boral"], ymax = cresp4m[, "boral"] + cresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp4m[, "hmsc"] - cresp4c[, "hmsc"], ymax = cresp4m[, "hmsc"] + cresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp4m[, "sam"] - cresp4c[, "sam"], ymax = cresp4m[, "sam"] + cresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp4m[, "raw"] - cresp4c[, "raw"], ymax = cresp4m[, "raw"] + cresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

#########################################

######################### Scenario 3 ################################

load("sc3_partial_effects.RData")

response1 <- c(5, 12, 14, 16)
response2 <- c(1, 3, 18)
response3 <- c(2, 4, 6, 8, 11, 13)
response4 <- c(7, 9, 10, 15, 17)

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


######################### Plotting #################################

### Response 1 ###

cols <- c("blue", "deeppink", "#00ff00", "black", "orange", "dodgerblue")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp1m[, "brt"] - sresp1c[, "brt"], ymax = sresp1m[, "brt"] + sresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp1m[, "glm"] - sresp1c[, "glm"], ymax = sresp1m[,"glm"] + sresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp1m[, "mistnet"] - sresp1c[, "mistnet"], ymax = sresp1m[, "mistnet"] + sresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp1m[, "boral"] - sresp1c[, "boral"], ymax = sresp1m[, "boral"] + sresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp1m[, "hmsc"] - sresp1c[, "hmsc"], ymax = sresp1m[, "hmsc"] + sresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp1m[, "sam"] - sresp1c[, "sam"], ymax = sresp1m[, "sam"] + sresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp1m[, "raw"] - sresp1c[, "raw"], ymax = sresp1m[, "raw"] + sresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp1m[, "brt"] - dresp1c[, "brt"], ymax = dresp1m[, "brt"] + dresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp1m[, "glm"] - dresp1c[, "glm"], ymax = dresp1m[, "glm"] + dresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp1m[, "mistnet"] - dresp1c[, "mistnet"], ymax = dresp1m[, "mistnet"] + dresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp1m[, "boral"] - dresp1c[, "boral"], ymax = dresp1m[, "boral"] + dresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp1m[, "hmsc"] - dresp1c[, "hmsc"], ymax = dresp1m[, "hmsc"] + dresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp1m[, "sam"] - dresp1c[, "sam"], ymax = dresp1m[, "sam"] + dresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp1m[, "raw"] - dresp1c[, "raw"], ymax = dresp1m[, "raw"] + dresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp1m[, "brt"] - cresp1c[, "brt"], ymax = cresp1m[, "brt"] + cresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp1m[, "glm"] - cresp1c[, "glm"], ymax = cresp1m[, "glm"] + cresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp1m[, "mistnet"] - cresp1c[, "mistnet"], ymax = cresp1m[, "mistnet"] + cresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp1m[, "boral"] - cresp1c[, "boral"], ymax = cresp1m[, "boral"] + cresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp1m[, "hmsc"] - cresp1c[, "hmsc"], ymax = cresp1m[, "hmsc"] + cresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp1m[, "sam"] - cresp1c[, "sam"], ymax = cresp1m[, "sam"] + cresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp1m[, "raw"] - cresp1c[, "raw"], ymax = cresp1m[, "raw"] + cresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 2 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp2m[, "brt"] - sresp2c[, "brt"], ymax = sresp2m[, "brt"] + sresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp2m[, "glm"] - sresp2c[, "glm"], ymax = sresp2m[,"glm"] + sresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp2m[, "mistnet"] - sresp2c[, "mistnet"], ymax = sresp2m[, "mistnet"] + sresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp2m[, "boral"] - sresp2c[, "boral"], ymax = sresp2m[, "boral"] + sresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp2m[, "hmsc"] - sresp2c[, "hmsc"], ymax = sresp2m[, "hmsc"] + sresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp2m[, "sam"] - sresp2c[, "sam"], ymax = sresp2m[, "sam"] + sresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp2m[, "raw"] - sresp2c[, "raw"], ymax = sresp2m[, "raw"] + sresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp2m[, "brt"] - dresp2c[, "brt"], ymax = dresp2m[, "brt"] + dresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp2m[, "glm"] - dresp2c[, "glm"], ymax = dresp2m[, "glm"] + dresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp2m[, "mistnet"] - dresp2c[, "mistnet"], ymax = dresp2m[, "mistnet"] + dresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp2m[, "boral"] - dresp2c[, "boral"], ymax = dresp2m[, "boral"] + dresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp2m[, "hmsc"] - dresp2c[, "hmsc"], ymax = dresp2m[, "hmsc"] + dresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp2m[, "sam"] - dresp2c[, "sam"], ymax = dresp2m[, "sam"] + dresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp2m[, "raw"] - dresp2c[, "raw"], ymax = dresp2m[, "raw"] + dresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp2m[, "brt"] - cresp2c[, "brt"], ymax = cresp2m[, "brt"] + cresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp2m[, "glm"] - cresp2c[, "glm"], ymax = cresp2m[, "glm"] + cresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp2m[, "mistnet"] - cresp2c[, "mistnet"], ymax = cresp2m[, "mistnet"] + cresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp2m[, "boral"] - cresp2c[, "boral"], ymax = cresp2m[, "boral"] + cresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp2m[, "hmsc"] - cresp2c[, "hmsc"], ymax = cresp2m[, "hmsc"] + cresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp2m[, "sam"] - cresp2c[, "sam"], ymax = cresp2m[, "sam"] + cresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp2m[, "raw"] - cresp2c[, "raw"], ymax = cresp2m[, "raw"] + cresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 3 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp3m[, "brt"] - sresp3c[, "brt"], ymax = sresp3m[, "brt"] + sresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp3m[, "glm"] - sresp3c[, "glm"], ymax = sresp3m[,"glm"] + sresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp3m[, "mistnet"] - sresp3c[, "mistnet"], ymax = sresp3m[, "mistnet"] + sresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp3m[, "boral"] - sresp3c[, "boral"], ymax = sresp3m[, "boral"] + sresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp3m[, "hmsc"] - sresp3c[, "hmsc"], ymax = sresp3m[, "hmsc"] + sresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp3m[, "sam"] - sresp3c[, "sam"], ymax = sresp3m[, "sam"] + sresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp3m[, "raw"] - sresp3c[, "raw"], ymax = sresp3m[, "raw"] + sresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp3m[, "brt"] - dresp3c[, "brt"], ymax = dresp3m[, "brt"] + dresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp3m[, "glm"] - dresp3c[, "glm"], ymax = dresp3m[, "glm"] + dresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp3m[, "mistnet"] - dresp3c[, "mistnet"], ymax = dresp3m[, "mistnet"] + dresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp3m[, "boral"] - dresp3c[, "boral"], ymax = dresp3m[, "boral"] + dresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp3m[, "hmsc"] - dresp3c[, "hmsc"], ymax = dresp3m[, "hmsc"] + dresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp3m[, "sam"] - dresp3c[, "sam"], ymax = dresp3m[, "sam"] + dresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp3m[, "raw"] - dresp3c[, "raw"], ymax = dresp3m[, "raw"] + dresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp3m[, "brt"] - cresp3c[, "brt"], ymax = cresp3m[, "brt"] + cresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp3m[, "glm"] - cresp3c[, "glm"], ymax = cresp3m[, "glm"] + cresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp3m[, "mistnet"] - cresp3c[, "mistnet"], ymax = cresp3m[, "mistnet"] + cresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp3m[, "boral"] - cresp3c[, "boral"], ymax = cresp3m[, "boral"] + cresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp3m[, "hmsc"] - cresp3c[, "hmsc"], ymax = cresp3m[, "hmsc"] + cresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp3m[, "sam"] - cresp3c[, "sam"], ymax = cresp3m[, "sam"] + cresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp3m[, "raw"] - cresp3c[, "raw"], ymax = cresp3m[, "raw"] + cresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 4 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp4m[, "brt"] - sresp4c[, "brt"], ymax = sresp4m[, "brt"] + sresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp4m[, "glm"] - sresp4c[, "glm"], ymax = sresp4m[,"glm"] + sresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp4m[, "mistnet"] - sresp4c[, "mistnet"], ymax = sresp4m[, "mistnet"] + sresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp4m[, "boral"] - sresp4c[, "boral"], ymax = sresp4m[, "boral"] + sresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp4m[, "hmsc"] - sresp4c[, "hmsc"], ymax = sresp4m[, "hmsc"] + sresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp4m[, "sam"] - sresp4c[, "sam"], ymax = sresp4m[, "sam"] + sresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp4m[, "raw"] - sresp4c[, "raw"], ymax = sresp4m[, "raw"] + sresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp4m[, "brt"] - dresp4c[, "brt"], ymax = dresp4m[, "brt"] + dresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp4m[, "glm"] - dresp4c[, "glm"], ymax = dresp4m[, "glm"] + dresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp4m[, "mistnet"] - dresp4c[, "mistnet"], ymax = dresp4m[, "mistnet"] + dresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp4m[, "boral"] - dresp4c[, "boral"], ymax = dresp4m[, "boral"] + dresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp4m[, "hmsc"] - dresp4c[, "hmsc"], ymax = dresp4m[, "hmsc"] + dresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp4m[, "sam"] - dresp4c[, "sam"], ymax = dresp4m[, "sam"] + dresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp4m[, "raw"] - dresp4c[, "raw"], ymax = dresp4m[, "raw"] + dresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp4m[, "brt"] - cresp4c[, "brt"], ymax = cresp4m[, "brt"] + cresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp4m[, "glm"] - cresp4c[, "glm"], ymax = cresp4m[, "glm"] + cresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp4m[, "mistnet"] - cresp4c[, "mistnet"], ymax = cresp4m[, "mistnet"] + cresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp4m[, "boral"] - cresp4c[, "boral"], ymax = cresp4m[, "boral"] + cresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp4m[, "hmsc"] - cresp4c[, "hmsc"], ymax = cresp4m[, "hmsc"] + cresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp4m[, "sam"] - cresp4c[, "sam"], ymax = cresp4m[, "sam"] + cresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp4m[, "raw"] - cresp4c[, "raw"], ymax = cresp4m[, "raw"] + cresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

#########################################

######################### Scenario 4 ################################

load("sc4_partial_effects.RData")

response1 <- c(4,7,11,13,15)
response2 <- c(1,5,8,9,10,12)
response3 <- c(2,6,16)
response4 <- c(3,14,17,18)

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


######################### Plotting #################################

### Response 1 ###

cols <- c("blue", "deeppink", "#00ff00", "black", "orange", "dodgerblue")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp1m[, "brt"] - sresp1c[, "brt"], ymax = sresp1m[, "brt"] + sresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp1m[, "glm"] - sresp1c[, "glm"], ymax = sresp1m[,"glm"] + sresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp1m[, "mistnet"] - sresp1c[, "mistnet"], ymax = sresp1m[, "mistnet"] + sresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp1m[, "boral"] - sresp1c[, "boral"], ymax = sresp1m[, "boral"] + sresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp1m[, "hmsc"] - sresp1c[, "hmsc"], ymax = sresp1m[, "hmsc"] + sresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp1m[, "sam"] - sresp1c[, "sam"], ymax = sresp1m[, "sam"] + sresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp1m[, "raw"] - sresp1c[, "raw"], ymax = sresp1m[, "raw"] + sresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp1m[, "brt"] - dresp1c[, "brt"], ymax = dresp1m[, "brt"] + dresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp1m[, "glm"] - dresp1c[, "glm"], ymax = dresp1m[, "glm"] + dresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp1m[, "mistnet"] - dresp1c[, "mistnet"], ymax = dresp1m[, "mistnet"] + dresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp1m[, "boral"] - dresp1c[, "boral"], ymax = dresp1m[, "boral"] + dresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp1m[, "hmsc"] - dresp1c[, "hmsc"], ymax = dresp1m[, "hmsc"] + dresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp1m[, "sam"] - dresp1c[, "sam"], ymax = dresp1m[, "sam"] + dresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp1m[, "raw"] - dresp1c[, "raw"], ymax = dresp1m[, "raw"] + dresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp1m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp1m[, "brt"] - cresp1c[, "brt"], ymax = cresp1m[, "brt"] + cresp1c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp1m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp1m[, "glm"] - cresp1c[, "glm"], ymax = cresp1m[, "glm"] + cresp1c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp1m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp1m[, "mistnet"] - cresp1c[, "mistnet"], ymax = cresp1m[, "mistnet"] + cresp1c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp1m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp1m[, "boral"] - cresp1c[, "boral"], ymax = cresp1m[, "boral"] + cresp1c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp1m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp1m[, "hmsc"] - cresp1c[, "hmsc"], ymax = cresp1m[, "hmsc"] + cresp1c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp1m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp1m[, "sam"] - cresp1c[, "sam"], ymax = cresp1m[, "sam"] + cresp1c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp1m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp1m[, "raw"] - cresp1c[, "raw"], ymax = cresp1m[, "raw"] + cresp1c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 2 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp2m[, "brt"] - sresp2c[, "brt"], ymax = sresp2m[, "brt"] + sresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp2m[, "glm"] - sresp2c[, "glm"], ymax = sresp2m[,"glm"] + sresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp2m[, "mistnet"] - sresp2c[, "mistnet"], ymax = sresp2m[, "mistnet"] + sresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp2m[, "boral"] - sresp2c[, "boral"], ymax = sresp2m[, "boral"] + sresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp2m[, "hmsc"] - sresp2c[, "hmsc"], ymax = sresp2m[, "hmsc"] + sresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp2m[, "sam"] - sresp2c[, "sam"], ymax = sresp2m[, "sam"] + sresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp2m[, "raw"] - sresp2c[, "raw"], ymax = sresp2m[, "raw"] + sresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp2m[, "brt"] - dresp2c[, "brt"], ymax = dresp2m[, "brt"] + dresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp2m[, "glm"] - dresp2c[, "glm"], ymax = dresp2m[, "glm"] + dresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp2m[, "mistnet"] - dresp2c[, "mistnet"], ymax = dresp2m[, "mistnet"] + dresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp2m[, "boral"] - dresp2c[, "boral"], ymax = dresp2m[, "boral"] + dresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp2m[, "hmsc"] - dresp2c[, "hmsc"], ymax = dresp2m[, "hmsc"] + dresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp2m[, "sam"] - dresp2c[, "sam"], ymax = dresp2m[, "sam"] + dresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp2m[, "raw"] - dresp2c[, "raw"], ymax = dresp2m[, "raw"] + dresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp2m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp2m[, "brt"] - cresp2c[, "brt"], ymax = cresp2m[, "brt"] + cresp2c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp2m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp2m[, "glm"] - cresp2c[, "glm"], ymax = cresp2m[, "glm"] + cresp2c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp2m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp2m[, "mistnet"] - cresp2c[, "mistnet"], ymax = cresp2m[, "mistnet"] + cresp2c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp2m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp2m[, "boral"] - cresp2c[, "boral"], ymax = cresp2m[, "boral"] + cresp2c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp2m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp2m[, "hmsc"] - cresp2c[, "hmsc"], ymax = cresp2m[, "hmsc"] + cresp2c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp2m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp2m[, "sam"] - cresp2c[, "sam"], ymax = cresp2m[, "sam"] + cresp2c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp2m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp2m[, "raw"] - cresp2c[, "raw"], ymax = cresp2m[, "raw"] + cresp2c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 3 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp3m[, "brt"] - sresp3c[, "brt"], ymax = sresp3m[, "brt"] + sresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp3m[, "glm"] - sresp3c[, "glm"], ymax = sresp3m[,"glm"] + sresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp3m[, "mistnet"] - sresp3c[, "mistnet"], ymax = sresp3m[, "mistnet"] + sresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp3m[, "boral"] - sresp3c[, "boral"], ymax = sresp3m[, "boral"] + sresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp3m[, "hmsc"] - sresp3c[, "hmsc"], ymax = sresp3m[, "hmsc"] + sresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp3m[, "sam"] - sresp3c[, "sam"], ymax = sresp3m[, "sam"] + sresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp3m[, "raw"] - sresp3c[, "raw"], ymax = sresp3m[, "raw"] + sresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp3m[, "brt"] - dresp3c[, "brt"], ymax = dresp3m[, "brt"] + dresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp3m[, "glm"] - dresp3c[, "glm"], ymax = dresp3m[, "glm"] + dresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp3m[, "mistnet"] - dresp3c[, "mistnet"], ymax = dresp3m[, "mistnet"] + dresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp3m[, "boral"] - dresp3c[, "boral"], ymax = dresp3m[, "boral"] + dresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp3m[, "hmsc"] - dresp3c[, "hmsc"], ymax = dresp3m[, "hmsc"] + dresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp3m[, "sam"] - dresp3c[, "sam"], ymax = dresp3m[, "sam"] + dresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp3m[, "raw"] - dresp3c[, "raw"], ymax = dresp3m[, "raw"] + dresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp3m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp3m[, "brt"] - cresp3c[, "brt"], ymax = cresp3m[, "brt"] + cresp3c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp3m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp3m[, "glm"] - cresp3c[, "glm"], ymax = cresp3m[, "glm"] + cresp3c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp3m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp3m[, "mistnet"] - cresp3c[, "mistnet"], ymax = cresp3m[, "mistnet"] + cresp3c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp3m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp3m[, "boral"] - cresp3c[, "boral"], ymax = cresp3m[, "boral"] + cresp3c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp3m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp3m[, "hmsc"] - cresp3c[, "hmsc"], ymax = cresp3m[, "hmsc"] + cresp3c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp3m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp3m[, "sam"] - cresp3c[, "sam"], ymax = cresp3m[, "sam"] + cresp3c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp3m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp3m[, "raw"] - cresp3c[, "raw"], ymax = cresp3m[, "raw"] + cresp3c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

### Response 4 ###

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_sst, y = sresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_sst, ymin = sresp4m[, "brt"] - sresp4c[, "brt"], ymax = sresp4m[, "brt"] + sresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_sst, y = sresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_sst, ymin = sresp4m[, "glm"] - sresp4c[, "glm"], ymax = sresp4m[,"glm"] + sresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_sst, y = sresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_sst, ymin = sresp4m[, "mistnet"] - sresp4c[, "mistnet"], ymax = sresp4m[, "mistnet"] + sresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_sst, y = sresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_sst, ymin = sresp4m[, "boral"] - sresp4c[, "boral"], ymax = sresp4m[, "boral"] + sresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_sst, y = sresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_sst, ymin = sresp4m[, "hmsc"] - sresp4c[, "hmsc"], ymax = sresp4m[, "hmsc"] + sresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_sst, y = sresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_sst, ymin = sresp4m[, "sam"] - sresp4c[, "sam"], ymax = sresp4m[, "sam"] + sresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_sst, y = sresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_sst, ymin = sresp4m[, "raw"] - sresp4c[, "raw"], ymax = sresp4m[, "raw"] + sresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Sea-Surface Temperature") +
  ylab("Probability")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_dss, y = dresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_dss, ymin = dresp4m[, "brt"] - dresp4c[, "brt"], ymax = dresp4m[, "brt"] + dresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_dss, y = dresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_dss, ymin = dresp4m[, "glm"] - dresp4c[, "glm"], ymax = dresp4m[, "glm"] + dresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_dss, y = dresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_dss, ymin = dresp4m[, "mistnet"] - dresp4c[, "mistnet"], ymax = dresp4m[, "mistnet"] + dresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_dss, y = dresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_dss, ymin = dresp4m[, "boral"] - dresp4c[, "boral"], ymax = dresp4m[, "boral"] + dresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_dss, y = dresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_dss, ymin = dresp4m[, "hmsc"] - dresp4c[, "hmsc"], ymax = dresp4m[, "hmsc"] + dresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_dss, y = dresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_dss, ymin = dresp4m[, "sam"] - dresp4c[, "sam"], ymax = dresp4m[, "sam"] + dresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_dss, y = dresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_dss, ymin = dresp4m[, "raw"] - dresp4c[, "raw"], ymax = dresp4m[, "raw"] + dresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Days Since Start") +
  ylab("")

ggplot() +
  geom_line(mapping = aes(x = brt_pp$brt_xx_chl, y = cresp4m[, "brt"]), col = "deeppink", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = brt_pp$brt_xx_chl, ymin = cresp4m[, "brt"] - cresp4c[, "brt"], ymax = cresp4m[, "brt"] + cresp4c[, "brt"]), fill = "deeppink", alpha = 0.2) +
  geom_line(mapping = aes(x = glm_pp$glm_xx_chl, y = cresp4m[, "glm"]), col = "#00ff00", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = glm_pp$glm_xx_chl, ymin = cresp4m[, "glm"] - cresp4c[, "glm"], ymax = cresp4m[, "glm"] + cresp4c[, "glm"]), fill = "#00ff00", alpha = 0.2) +
  geom_line(mapping = aes(x = mistnet_pp$mn_xx_chl, y = cresp4m[, "mistnet"]), col = "orange", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = mistnet_pp$mn_xx_chl, ymin = cresp4m[, "mistnet"] - cresp4c[, "mistnet"], ymax = cresp4m[, "mistnet"] + cresp4c[, "mistnet"]), fill = "orange", alpha = 0.2) +
  geom_line(mapping = aes(x = boral_pp$boral_xx_chl, y = cresp4m[, "boral"]), col = "blue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = boral_pp$boral_xx_chl, ymin = cresp4m[, "boral"] - cresp4c[, "boral"], ymax = cresp4m[, "boral"] + cresp4c[, "boral"]), fill = "blue", alpha = 0.2) +
  geom_line(mapping = aes(x = hmsc_pp$hmsc_xx_chl, y = cresp4m[, "hmsc"]), col = "seagreen", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = hmsc_pp$hmsc_xx_chl, ymin = cresp4m[, "hmsc"] - cresp4c[, "hmsc"], ymax = cresp4m[, "hmsc"] + cresp4c[, "hmsc"]), fill = "seagreen", alpha = 0.2) +
  geom_line(mapping = aes(x = sam_pp$sam_xx_chl, y = cresp4m[, "sam"]), col = "dodgerblue", linewidth = 1.5, linetype = 2) +
  geom_ribbon(mapping = aes(x = sam_pp$sam_xx_chl, ymin = cresp4m[, "sam"] - cresp4c[, "sam"], ymax = cresp4m[, "sam"] + cresp4c[, "sam"]), fill = "dodgerblue", alpha = 0.2) +
  geom_line(mapping = aes(x = raw_pp$raw_xx_chl, y = cresp4m[, "raw"]), col = "black", linewidth = 1.5, linetype = 1) +
  geom_ribbon(mapping = aes(x = raw_pp$raw_xx_chl, ymin = cresp4m[, "raw"] - cresp4c[, "raw"], ymax = cresp4m[, "raw"] + cresp4c[, "raw"]), fill = "black", alpha = 0.3) +
  theme_bw(base_line_size = 0.1) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        axis.text=element_text(size=20), axis.title=element_text(size=25)) +
  ylim(0,1) +
  xlab("Chlorophyll a") +
  ylab("")

#########################################


