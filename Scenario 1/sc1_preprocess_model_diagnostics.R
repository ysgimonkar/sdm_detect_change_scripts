# This can be used for all four scenarios

# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ecomix)
library(raster)
library(pROC)
library(boral)
library(terra)
library(rgdal)
library(rasterVis)
library(tibble)
library(sdm)
library(mistnet)
library(Hmsc)
library(plotrix)

# Load data from respective scenario

load("sam.RData")
load("glm.RData")
load("brt.RData")
load("hmsc.RData")
load("boral.RData")
load("mistnet.RData")

### RMSE ---------------------------------------------------------------------------------

boral_rmse <- list()
brt_rmse <- list()
glm_rmse <- list()
hmsc_rmse <- list()
mistnet_rmse <- list()
sam_rmse <- list()

for(i in 1:100){
  boral_rmse[[i]] <- boral_par[[i]]$boral_RMSE
  brt_rmse[[i]] <- brt_par[[i]]$brt_RMSE
  glm_rmse[[i]] <- glm_par[[i]]$glm_RMSE
  hmsc_rmse[[i]] <- hmsc_par[[i]]$hmsc_RMSE
  mistnet_rmse[[i]] <- mistnet_par[[i]]$mistnet_RMSE
  sam_rmse[[i]] <- sam_par[[i]]$sam_RMSE
}

rmse <- list()

for(i in 1:100){
  rmse[[i]] <- as.data.frame(cbind(BORAL = boral_rmse[[i]], 
                                   BRT = brt_rmse[[i]], 
                                   GLM = glm_rmse[[i]], 
                                   HMSC = hmsc_rmse[[i]],
                                   MistNet = mistnet_rmse[[i]],
                                   SAM = sam_rmse[[i]]))
}

rm(list = c("boral_rmse", "brt_rmse", "glm_rmse", "hmsc_rmse", "mistnet_rmse", "sam_rmse"))

### Tjur's R2 ---------------------------------------------------------------------------------

boral_tjur <- list()
brt_tjur <- list()
glm_tjur <- list()
hmsc_tjur <- list()
mistnet_tjur <- list()
sam_tjur <- list()

for(i in 1:100){
  boral_tjur[[i]] <- boral_par[[i]]$boral_TjurR2
  brt_tjur[[i]] <- brt_par[[i]]$brt_TjurR2
  glm_tjur[[i]] <- glm_par[[i]]$glm_TjurR2
  hmsc_tjur[[i]] <- hmsc_par[[i]]$hmsc_TjurR2
  mistnet_tjur[[i]] <- mistnet_par[[i]]$mistnet_TjurR2
  sam_tjur[[i]] <- sam_par[[i]]$sam_TjurR2
}

tjur <- list()

for(i in 1:100){
  tjur[[i]] <- as.data.frame(cbind(BORAL = boral_tjur[[i]], 
                                   BRT = brt_tjur[[i]], 
                                   GLM = glm_tjur[[i]], 
                                   HMSC = hmsc_tjur[[i]],
                                   MistNet = mistnet_tjur[[i]],
                                   SAM = sam_tjur[[i]]))
}

rm(list = c("boral_tjur", "brt_tjur", "glm_tjur", "hmsc_tjur", "mistnet_tjur", "sam_tjur"))

### AUC ---------------------------------------------------------------------------------

boral_auc <- list()
brt_auc <- list()
glm_auc <- list()
hmsc_auc <- list()
mistnet_auc <- list()
sam_auc <- list()

for(i in 1:100){
  boral_auc[[i]] <- boral_par[[i]]$boral_AUC
  brt_auc[[i]] <- brt_par[[i]]$brt_AUC
  glm_auc[[i]] <- glm_par[[i]]$glm_AUC
  hmsc_auc[[i]] <- hmsc_par[[i]]$hmsc_AUC
  mistnet_auc[[i]] <- mistnet_par[[i]]$mistnet_AUC
  sam_auc[[i]] <- sam_par[[i]]$sam_AUC
}

au <- list()

for(i in 1:100){
  au[[i]] <- as.data.frame(cbind(BORAL = boral_auc[[i]], 
                                 BRT = brt_auc[[i]], 
                                 GLM = glm_auc[[i]], 
                                 HMSC = hmsc_auc[[i]], 
                                 MistNet = mistnet_auc[[i]],
                                 SAM = sam_auc[[i]]))
}

rm(list = c("boral_auc", "brt_auc", "glm_auc", "hmsc_auc", "mistnet_auc", "sam_auc", "i"))

rm(list = c("boral_par", "brt_par", "glm_par", "hmsc_par", "mistnet_par", "sam_par"))

################### ANALYSIS SECTION ############################

### RMSE ---------------------------------------------------------------------------------
n_rows <- nrow(rmse[[1]])
n_cols <- ncol(rmse[[1]])

mean_rmse <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(mean_rmse) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(rmse, function(mat) mat[i, j])
    mean_rmse[i, j] <- mean(values)
  }
}

mrmse <- as.data.frame(mean_rmse)
mrmse <- add_column(mrmse, spp = c(paste0("spp", 1:18)), .before = "BORAL")
mrmse <- pivot_longer(mrmse, cols = -1, names_to = "models", values_to = "mean")

se_rmse <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(se_rmse) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(rmse, function(mat) mat[i, j])
    se_rmse[i, j] <- std.error(values)
  }
}

srmse <- as.data.frame(se_rmse)
srmse <- add_column(srmse, spp = c(paste0("spp",1:18)), .before = "BORAL")
srmse <- pivot_longer(srmse, cols = -1, names_to = "models", values_to = "se")

frmse <- merge(mrmse,srmse, by = c("spp", "models"))
frmse$spp <- factor(frmse$spp, levels = c(paste0("spp", 1:18)))

min_rmse <- frmse %>%
  filter(models != "SAM") %>%
  group_by(spp)  %>%
  slice(which.min(mean)) %>%
  dplyr::select(spp, models, mean, se)

sam_rmse <- frmse %>%
  filter(models == "SAM") %>%
  group_by(spp)  %>%
  slice(which.min(mean)) %>%
  dplyr::select(spp, models, mean, se)

rm(list = c("rmse", "n_rows", "n_cols", "mean_rmse", "mrmse", "se_rmse", "srmse"))

### Tjur's R2 ---------------------------------------------------------------------------------

n_rows <- nrow(tjur[[1]])
n_cols <- ncol(tjur[[1]])

mean_tjur <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(mean_tjur) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(tjur, function(mat) mat[i, j])
    mean_tjur[i, j] <- mean(values)
  }
}

mtjur <- as.data.frame(mean_tjur)
mtjur <- add_column(mtjur, spp = c(paste0("spp",1:18)), .before = "BORAL")
mtjur <- pivot_longer(mtjur, cols = -1, names_to = "models", values_to = "mean")

se_tjur <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(se_tjur) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(tjur, function(mat) mat[i, j])
    se_tjur[i, j] <- std.error(values)
  }
}

stjur <- as.data.frame(se_tjur)
stjur <- add_column(stjur, spp = c(paste0("spp",1:18)), .before = "BORAL")
stjur <- pivot_longer(stjur, cols = -1, names_to = "models", values_to = "se")

ftjur <- merge(mtjur,stjur, by = c("spp", "models"))
ftjur$spp <- factor(ftjur$spp, levels = c(paste0("spp", 1:18)))

max_tjur <- ftjur %>%
  filter(models != "SAM") %>%
  group_by(spp)  %>%
  slice(which.max(mean)) %>%
  dplyr::select(spp, models, mean, se)

sam_tjur <- ftjur %>%
  filter(models == "SAM") %>%
  group_by(spp)  %>%
  slice(which.max(mean)) %>%
  dplyr::select(spp, models, mean, se)


rm(list = c("tjur", "n_rows", "n_cols", "mean_tjur", "mtjur", "se_tjur", "stjur"))

### AUC ---------------------------------------------------------------------------------

n_rows <- nrow(au[[1]])
n_cols <- ncol(au[[1]])

mean_auc <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(mean_auc) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(au, function(mat) mat[i, j])
    mean_auc[i, j] <- mean(values)
  }
}

mauc <- as.data.frame(mean_auc)
mauc <- add_column(mauc, spp = c(paste0("spp",1:18)), .before = "BORAL")
mauc <- pivot_longer(mauc, cols = -1, names_to = "models", values_to = "mean")

se_auc <- matrix(NA, nrow = n_rows, ncol = n_cols)
colnames(se_auc) <- c("BORAL", "BRT", "GLM", "HMSC", "MistNet", "SAM")

for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    values <- sapply(au, function(mat) mat[i, j])
    se_auc[i, j] <- std.error(values)
  }
}

sauc <- as.data.frame(se_auc)
sauc <- add_column(sauc, spp = c(paste0("spp",1:18)), .before = "BORAL")
sauc <- pivot_longer(sauc, cols = -1, names_to = "models", values_to = "se")

fauc <- merge(mauc,sauc, by = c("spp", "models"))
fauc$spp <- factor(fauc$spp, levels = c(paste0("spp", 1:18)))

max_auc <- fauc %>%
  filter(models != "SAM") %>%
  group_by(spp)  %>%
  slice(which.max(mean)) %>%
  dplyr::select(spp, models, mean, se)

sam_auc <- fauc %>%
  filter(models == "SAM") %>%
  group_by(spp)  %>%
  slice(which.max(mean)) %>%
  dplyr::select(spp, models, mean, se)

rm(list = c("au", "n_rows", "n_cols", "mean_auc", "mauc", "se_auc", "sauc"))

rm(list = c("i", "j", "values"))

#########################################################################

# save(frmse,
#      ftjur,
#      fauc,
#      min_rmse,
#      max_tjur,
#      max_auc,
#      sam_rmse,
#      sam_tjur,
#      sam_auc, file = "model_diagnostics.RData")
