# For Scenario 1 only

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
library(patchwork)

# Load data

load("sc1_model_diagnostics.RData")

################### PLOTTING SECTION ############################

### RMSE ---------------------------------------------------------------------------------

res1 <- frmse %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1")

res2 <- frmse %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2")

res3 <- frmse %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3")

res4 <- frmse %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4")

res <- rbind(res1, res2, res3, res4)

min_rmse <- res %>%
  filter(models != "SAM") %>%
  group_by(res)  %>%
  slice(which.min(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

sam_rmse <- res %>%
  filter(models == "SAM") %>%
  group_by(res)  %>%
  slice(which.min(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

rmse <- ggplot() +
  geom_point(data = res, mapping = aes(x = meanx, y = models)) +
  geom_errorbar(data = res, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), width = 0.25) +
  geom_point(data = min_rmse, mapping = aes(x = meanx, y = models), col = "red") +
  geom_errorbar(data = min_rmse, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "red", width = 0.25) +
  geom_point(data = sam_rmse, mapping = aes(x = meanx, y = models), col = "grey") +
  geom_errorbar(data = sam_rmse, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "grey", width = 0.25) +
  facet_wrap(~res, nrow = 1) +
  theme_bw(base_line_size = 0.1)+ 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  xlab("Root Mean Square Error (RMSE)") +
  ylab("")

### Tjur R2 ---------------------------------------------------------------------------------

res1 <- ftjur %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1")

res2 <- ftjur %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2")

res3 <- ftjur %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3")

res4 <- ftjur %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4")

res <- rbind(res1, res2, res3, res4)

max_tjur <- res %>%
  filter(models != "SAM") %>%
  group_by(res)  %>%
  slice(which.max(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

sam_tjur <- res %>%
  filter(models == "SAM") %>%
  group_by(res)  %>%
  slice(which.max(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

tjur <- ggplot() +
  geom_point(data = res, mapping = aes(x = meanx, y = models)) +
  geom_errorbar(data = res, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), width = 0.25) +
  geom_point(data = max_tjur, mapping = aes(x = meanx, y = models), col = "red") +
  geom_errorbar(data = max_tjur, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "red", width = 0.25) +
  geom_point(data = sam_tjur, mapping = aes(x = meanx, y = models), col = "grey") +
  geom_errorbar(data = sam_tjur, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "grey", width = 0.25) +
  facet_wrap(~res, nrow = 1) +
  theme_bw(base_line_size = 0.1)+ 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  xlab(expression(Tjur~R^{"2"})) +
  ylab("")

### AUC ---------------------------------------------------------------------------------

res1 <- fauc %>%
  filter(spp == "spp3" | spp == "spp4" | spp == "spp8") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 1")

res2 <- fauc %>%
  filter(spp == "spp5" | spp == "spp6" | spp == "spp9" | spp == "spp10" | spp == "spp11" | spp == "spp12" | spp == "spp13" | spp == "spp14" | spp == "spp18") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 2")

res3 <- fauc %>%
  filter(spp == "spp2" | spp == "spp15" | spp == "spp16") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 3")

res4 <- fauc %>%
  filter(spp == "spp1" | spp == "spp7" | spp == "spp17") %>%
  group_by(models) %>%
  summarise(meanx = mean(mean),
            ci = 1.96*std.error(mean)) %>%
  add_column(res = "Response 4")

res <- rbind(res1, res2, res3, res4)

max_auc <- res %>%
  filter(models != "SAM") %>%
  group_by(res)  %>%
  slice(which.max(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

sam_auc <- res %>%
  filter(models == "SAM") %>%
  group_by(res)  %>%
  slice(which.max(meanx)) %>%
  dplyr::select(models, meanx, ci, res)

auc <- ggplot() +
  geom_point(data = res, mapping = aes(x = meanx, y = models)) +
  geom_errorbar(data = res, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), width = 0.25) +
  geom_point(data = max_auc, mapping = aes(x = meanx, y = models), col = "red") +
  geom_errorbar(data = max_auc, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "red", width = 0.25) +
  geom_point(data = sam_auc, mapping = aes(x = meanx, y = models), col = "grey") +
  geom_errorbar(data = sam_auc, mapping = aes(xmin = meanx - ci, xmax = meanx + ci, y = models), col = "grey", width = 0.25) +
  facet_wrap(~res, nrow = 1) +
  theme_bw(base_line_size = 0.1)+ 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  xlab("AUC") +
  ylab("")

#---------------------------------------------------------------------------------

# Plot

rmse / tjur / auc
