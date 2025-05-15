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
ras_stack <- stack("~/figures/SO_CPR_yr_month_sst_stack.grd")

r_obj <- rast(xmin=53, xmax=160, ymin= -69, ymax= -38) 
dim(r_obj) <- c(124, 428)
crs(r_obj) <- crs(ras_stack)

load("~/figures/extrapolation_data.RData")

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

#### difference between final and initial days ####

diff_fun <- function(ras1, r_obj){
  
  # Load world map as SpatialPolygons
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  world_sp <- as(world_sf, "Spatial")
  extent_crop <- extent(53, 160, -69, -38)
  world_crop <- crop(world_sp, extent_crop)
  
  ini <- rasterize(x = location,
                         y = r_obj,
                         values = ras1$initial)
  
  fin <- rasterize(x = location,
                         y = r_obj,
                         values = ras1$final)
  
  # difference
  mod <- fin - ini
  
  # creating a manual colour palette
  pal <- rev(paletteer_c("grDevices::Spectral", 1000))
  nlev <- 1000
  res1.at <- seq(from = min(minmax(mod[[c(1:3)]])),
                 to = max(minmax(mod[[c(1:3)]])),
                 length.out = nlev+1)
  res2.at <- seq(from = min(minmax(mod[[c(4:6)]])),
                 to = max(minmax(mod[[c(4:6)]])),
                 length.out = nlev+1)
  res3.at <- seq(from = min(minmax(mod[[c(7:9)]])),
                 to = max(minmax(mod[[c(7:9)]])),
                 length.out = nlev+1)
  res4.at <- seq(from = min(minmax(mod[[c(10:12)]])),
                 to = max(minmax(mod[[c(10:12)]])),
                 length.out = nlev+1)
  
  # diff
  diff1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 1,
                     margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 4,
                     margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 7,
                     margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 10,
                     margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  low1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 2,
                    margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 5,
                    margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 8,
                    margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 11,
                    margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  up1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 3,
                   margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 6,
                   margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 9,
                   margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 12,
                   margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  a <- list()
  
  a[[1]] <- low1
  a[[2]] <- diff1
  a[[3]] <- up1
  a[[4]] <- low2
  a[[5]] <- diff2
  a[[6]] <- up2
  a[[7]] <- low3
  a[[8]] <- diff3
  a[[9]] <- up3
  a[[10]] <- low4
  a[[11]] <- diff4
  a[[12]] <- up4
  
  names(a) <- c("low1", "diff1", "up1",
                "low2", "diff2", "up2",
                "low3", "diff3", "up3",
                "low4", "diff4", "up4")
  
  return(a)
}

br_hm_diff_fun2 <- function(ras1, r_obj){
  
  # Load world map as SpatialPolygons
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  world_sp <- as(world_sf, "Spatial")
  extent_crop <- extent(53, 160, -69, -38)
  world_crop <- crop(world_sp, extent_crop)
  
  #create empty raster
  r2 <- r_obj
  empty.ra <- r2
  empty.ra[] <- NA
  
  res1 <- c(empty.ra, empty.ra, empty.ra)
  res1[[1]][all.sel.ra]  <- ras1$initial[,1]
  res1[[2]][all.sel.ra]  <- ras1$initial[,2]
  res1[[3]][all.sel.ra]  <- ras1$initial[,3]
  
  res2 <- c(empty.ra, empty.ra, empty.ra)
  res2[[1]][all.sel.ra]  <- ras1$initial[,4]
  res2[[2]][all.sel.ra]  <- ras1$initial[,5]
  res2[[3]][all.sel.ra]  <- ras1$initial[,6]
  
  res3 <- c(empty.ra, empty.ra, empty.ra)
  res3[[1]][all.sel.ra]  <- ras1$initial[,7]
  res3[[2]][all.sel.ra]  <- ras1$initial[,8]
  res3[[3]][all.sel.ra]  <- ras1$initial[,9]
  
  res4 <- c(empty.ra, empty.ra, empty.ra)
  res4[[1]][all.sel.ra]  <- ras1$initial[,10]
  res4[[2]][all.sel.ra]  <- ras1$initial[,11]
  res4[[3]][all.sel.ra]  <- ras1$initial[,12]
  
  ini <- c(res1,res2,res3,res4)
  
  rm(list = c("res1", "res2", "res3", "res4"))
  
  #create empty raster
  r2 <- r_obj
  empty.ra <- r2
  empty.ra[] <- NA
  
  res1 <- c(empty.ra, empty.ra, empty.ra)
  res1[[1]][all.sel.ra]  <- ras1$final[,1]
  res1[[2]][all.sel.ra]  <- ras1$final[,2]
  res1[[3]][all.sel.ra]  <- ras1$final[,3]
  
  res2 <- c(empty.ra, empty.ra, empty.ra)
  res2[[1]][all.sel.ra]  <- ras1$final[,4]
  res2[[2]][all.sel.ra]  <- ras1$final[,5]
  res2[[3]][all.sel.ra]  <- ras1$final[,6]
  
  res3 <- c(empty.ra, empty.ra, empty.ra)
  res3[[1]][all.sel.ra]  <- ras1$final[,7]
  res3[[2]][all.sel.ra]  <- ras1$final[,8]
  res3[[3]][all.sel.ra]  <- ras1$final[,9]
  
  res4 <- c(empty.ra, empty.ra, empty.ra)
  res4[[1]][all.sel.ra]  <- ras1$final[,10]
  res4[[2]][all.sel.ra]  <- ras1$final[,11]
  res4[[3]][all.sel.ra]  <- ras1$final[,12]
  
  fin <- c(res1,res2,res3,res4)
  
  rm(list = c("res1", "res2", "res3", "res4"))
  
  # difference
  mod <- fin - ini
  
  # creating a manual colour palette
  pal <- rev(paletteer_c("grDevices::Spectral", 1000))
  nlev <- 1000
  res1.at <- seq(from = min(minmax(mod[[c(1:3)]])),
                   to = max(minmax(mod[[c(1:3)]])),
                   length.out = nlev+1)
  res2.at <- seq(from = min(minmax(mod[[c(4:6)]])),
                 to = max(minmax(mod[[c(4:6)]])),
                 length.out = nlev+1)
  res3.at <- seq(from = min(minmax(mod[[c(7:9)]])),
                 to = max(minmax(mod[[c(7:9)]])),
                 length.out = nlev+1)
  res4.at <- seq(from = min(minmax(mod[[c(10:12)]])),
                 to = max(minmax(mod[[c(10:12)]])),
                 length.out = nlev+1)

  # diff
  diff1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 1,
                            margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 4,
                            margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 7,
                            margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  diff4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 10,
                            margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  low1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 2,
                           margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 5,
                           margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 8,
                           margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  low4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 11,
                           margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  up1 <- levelplot(mod,at = res1.at, col.regions=pal,layers = 3,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up2 <- levelplot(mod,at = res2.at, col.regions=pal,layers = 6,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up3 <- levelplot(mod,at = res3.at, col.regions=pal,layers = 9,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  up4 <- levelplot(mod,at = res4.at, col.regions=pal,layers = 12,
                          margin = FALSE) +
    latticeExtra::layer(sp.polygons(world_crop, fill='gray90', border='black'))
  
  a <- list()
  
  a[[1]] <- low1
  a[[2]] <- diff1
  a[[3]] <- up1
  a[[4]] <- low2
  a[[5]] <- diff2
  a[[6]] <- up2
  a[[7]] <- low3
  a[[8]] <- diff3
  a[[9]] <- up3
  a[[10]] <- low4
  a[[11]] <- diff4
  a[[12]] <- up4
  
  names(a) <- c("low1", "diff1", "up1",
                "low2", "diff2", "up2",
                "low3", "diff3", "up3",
                "low4", "diff4", "up4")
  
  return(a)
}

raw_diff <- diff_fun(raw_ras, r_obj)
brt_diff <- diff_fun(brt_ras, r_obj)
hmsc_diff <- br_hm_diff_fun(hmsc_ras, r_obj)

## Plotting difference between Final and Initial period for Actual Response
ggplot_list <- lapply(raw_diff[c(7:9)], function(p) as.ggplot((p)))
plot_grid(plotlist = ggplot_list, nrow = 1)

## Plotting difference between Final and Initial period for BRT Response
ggplot_list <- lapply(brt_diff[c(7:9)], function(p) as.ggplot((p)))
plot_grid(plotlist = ggplot_list, nrow = 1)

## Plotting difference between Final and Initial period for HMSC Response
ggplot_list <- lapply(hmsc_diff[c(7:9)], function(p) as.ggplot((p)))
plot_grid(plotlist = ggplot_list, nrow = 1)



















