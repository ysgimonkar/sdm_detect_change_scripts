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
library(rnaturalearth)
library(lattice)
library(maps)
library(mapdata)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(ggplotify)

################################# RASTER #############################
ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

r_obj <- rast(xmin=53, xmax=160, ymin= -69, ymax= -38) 
dim(r_obj) <- c(124, 428)
crs(r_obj) <- crs(ras_stack)

load("extrapolation_data.RData")

full <- ext_data$full

location <- as.matrix(full[,c(1,2)])

rm(list = c("ras_stack", "full", "ext_data"))

# Load world map as SpatialPolygons
world_sf <- ne_countries(scale = "medium", returnclass = "sf")
world_sp <- as(world_sf, "Spatial")
extent_crop <- extent(53, 160, -69, -38)
world_crop <- crop(world_sp, extent_crop)

########################### SCENARIO 1 ####################################

load("sc1_model_extrapolations.RData")
load("cells.RData")

## full difference plots

diff_fun <- function(model1, model2, model3){

  # Load world map as SpatialPolygons
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  world_sp <- as(world_sf, "Spatial")
  extent_crop <- extent(53, 160, -69, -38)
  world_crop <- crop(world_sp, extent_crop)
  
  model1_full <- rasterize(x = location,
                           y = r_obj,
                           values = model1$full)
  
  model2_full <- rasterize(x = location,
                           y = r_obj,
                           values = model2$full)
  
  #create empty raster
  r2 <- r_obj
  empty.ra <- r2
  empty.ra[] <- NA
  
  res1 <- c(empty.ra, empty.ra, empty.ra)
  res1[[1]][all.sel.ra]  <- model3$full[,1]
  res1[[2]][all.sel.ra]  <- model3$full[,2]
  res1[[3]][all.sel.ra]  <- model3$full[,3]
  
  res2 <- c(empty.ra, empty.ra, empty.ra)
  res2[[1]][all.sel.ra]  <- model3$full[,4]
  res2[[2]][all.sel.ra]  <- model3$full[,5]
  res2[[3]][all.sel.ra]  <- model3$full[,6]
  
  res3 <- c(empty.ra, empty.ra, empty.ra)
  res3[[1]][all.sel.ra]  <- model3$full[,7]
  res3[[2]][all.sel.ra]  <- model3$full[,8]
  res3[[3]][all.sel.ra]  <- model3$full[,9]
  
  res4 <- c(empty.ra, empty.ra, empty.ra)
  res4[[1]][all.sel.ra]  <- model3$full[,10]
  res4[[2]][all.sel.ra]  <- model3$full[,11]
  res4[[3]][all.sel.ra]  <- model3$full[,12]
  
  model3_full <- c(res1,res2,res3,res4)
  
  rm(list = c("res1", "res2", "res3", "res4"))
  
  # difference
  brt <- model2_full - model1_full
  hmsc <- model3_full - model1_full
  
  # creating a manual colour palette
  pal <- rev(paletteer_c("grDevices::Spectral", 1000))
  nlev <- 1000
  brt.at <- seq(from = min(minmax(brt[[c(7:9)]])),
                to = max(minmax(brt[[c(7:9)]])),
                length.out = nlev+1)
  
  # creating a manual colour palette
  negative.length <- round(abs(range(brt.at)[1]) /
                             diff(range(brt.at)) *
                             nlev)
  positive.length <- nlev - negative.length
  
  brt_diff1 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 1,
                         margin = FALSE, ) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_diff2 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 4,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_diff3 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 7,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_diff4 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 10,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  brt_low1 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 2,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_low2 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 5,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_low3 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 8,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_low4 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 11,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  brt_up1 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 3,
                       margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_up2 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 6,
                       margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_up3 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 9,
                       margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  brt_up4 <- levelplot(brt,at = brt.at, col.regions=pal,layers = 12,
                       margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  hmsc.at <- seq(from = min(minmax(hmsc[[c(7:9)]])),
                 to = max(minmax(hmsc[[c(7:9)]])),
                 length.out = nlev+1)
  
  negative.length2 <- round(abs(range(hmsc.at)[1]) / 
                              diff(range(hmsc.at)) * 
                              nlev)
  positive.length2 <- nlev - negative.length2
  
  hmsc_diff1 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 1,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_diff2 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 4,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_diff3 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 7,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_diff4 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 10,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  hmsc_low1 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 2,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_low2 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 5,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_low3 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 8,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_low4 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 11,
                         margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  hmsc_up1 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 3,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_up2 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 6,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_up3 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 9,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  hmsc_up4 <- levelplot(hmsc,at = brt.at, col.regions=pal,layers = 12,
                        margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  a <- list()
  
  a[[1]] <-brt_low1
  a[[2]] <- brt_diff1
  a[[3]] <- brt_up1
  a[[4]] <- brt_low2
  a[[5]] <- brt_diff2
  a[[6]] <-brt_up2
  a[[7]] <- brt_low3
  a[[8]] <- brt_diff3
  a[[9]] <- brt_up3
  a[[10]] <- brt_low4
  a[[11]] <- brt_diff4
  a[[12]] <- brt_up4
  a[[13]] <- hmsc_low1
  a[[14]] <- hmsc_diff1 
  a[[15]] <- hmsc_up1
  a[[16]] <- hmsc_low2
  a[[17]] <- hmsc_diff2
  a[[18]] <- hmsc_up2
  a[[19]] <- hmsc_low3
  a[[20]] <- hmsc_diff3
  a[[21]] <- hmsc_up3
  a[[22]] <- hmsc_low4 
  a[[23]] <- hmsc_diff4
  a[[24]] <- hmsc_up4
  
  names(a) <- c("brt_low1", "brt_diff1", "brt_up1",
                "brt_low2", "brt_diff2", "brt_up2",
                "brt_low3", "brt_diff3", "brt_up3",
                "brt_low4", "brt_diff4", "brt_up4",
                "hmsc_low1", "hmsc_diff1", "hmsc_up1",
                "hmsc_low2", "hmsc_diff2", "hmsc_up2",
                "hmsc_low3", "hmsc_diff3", "hmsc_up3",
                "hmsc_low4", "hmsc_diff4", "hmsc_up4")
  
  return(a)
  
  
}
mod_diff <- diff_fun(raw_ras, brt_ras, hmsc_ras)

## Plotting difference between BRT and Actual Response
ggplot_list <- lapply(mod_diff[c(7:9)], function(p) as.ggplot((p)))
plot_grid(plotlist = ggplot_list, nrow = 1)

## Plotting difference between HMSC and Actual Response
ggplot_list <- lapply(mod_diff[c(19:21)], function(p) as.ggplot((p)))
plot_grid(plotlist = ggplot_list, nrow = 1)




