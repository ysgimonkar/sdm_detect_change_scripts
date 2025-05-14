# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(raster)
library(pROC)
library(boral)
library(terra)
# library(rgdal)
library(rasterVis)
library(tibble)
library(plotrix)
library(parallel)
library(future.apply)

##### Full stack #####
ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

ras_stack <- ras_stack[[c("Oct.2001", "Oct.2002", "Oct.2003", "Oct.2004", "Oct.2005", "Oct.2006", "Oct.2007", "Oct.2008", "Oct.2009", "Oct.2010", "Oct.2011", "Oct.2012", "Oct.2013", "Oct.2014", "Oct.2015",
                          "Nov.2001", "Nov.2002", "Nov.2003", "Nov.2004", "Nov.2005", "Nov.2006", "Nov.2007", "Nov.2008", "Nov.2009", "Nov.2010", "Nov.2011", "Nov.2012", "Nov.2013", "Nov.2014", "Nov.2015",
                          "Dec.2001", "Dec.2002", "Dec.2003", "Dec.2004", "Dec.2005", "Dec.2006", "Dec.2007", "Dec.2008", "Dec.2009", "Dec.2010", "Dec.2011", "Dec.2012", "Dec.2013", "Dec.2014", "Dec.2015",
                          "Jan.2002", "Jan.2003", "Jan.2004", "Jan.2005", "Jan.2006", "Jan.2007", "Jan.2008", "Jan.2009", "Jan.2010", "Jan.2011", "Jan.2012", "Jan.2013", "Jan.2014", "Jan.2015", "Jan.2016",
                          "Feb.2002", "Feb.2003", "Feb.2004", "Feb.2005", "Feb.2006", "Feb.2007", "Feb.2008", "Feb.2009", "Feb.2010", "Feb.2011", "Feb.2012", "Feb.2013", "Feb.2014", "Feb.2015", "Feb.2016",
                          "Mar.2002", "Mar.2003", "Mar.2004", "Mar.2005", "Mar.2006", "Mar.2007", "Mar.2008", "Mar.2009", "Mar.2010", "Mar.2011", "Mar.2012", "Mar.2013", "Mar.2014", "Mar.2015", "Mar.2016")]]

sst_full <- rast(calc(ras_stack, fun = mean, na.rm = TRUE))
values(sst_full) <- scale(values(sst_full))
chl_full <- rast("simulated_chlorophyll_raster.tif")
values(chl_full) <- scale(values(chl_full))
dsStart_full <- sst_full
values(dsStart_full) <- 0.0000331176 

# Add layer names
names(sst_full) <- "sst"
names(dsStart_full) <- "dsStart"
names(chl_full) <- "chla"

# Merge all the layers into a single stack
full_stack <- c(sst_full, dsStart_full, chl_full)

rm(list = c("chl_full", "dsStart_full", "sst_full", "ras_stack"))

#############################

# Load data from respective scenario
load("boral.RData")

for(i in 1:length(boral_par)){
  
  dir.create(paste0("~/full/pred", i))
  
  fitSepTF <- boral_par[[i]]$boral_fit
  
  #assign species names
  sp.v <- colnames(fitSepTF$y)
  
  #create empty raster
  r2 <- full_stack$dsStart
  empty.ra <- r2
  empty.ra[] <- NA
  
  # Identify which cells we have ignored before
  sel.not.na <- which(!is.na(r2[]))
  
  # Relevant env data
  grid <- full_stack[]
  
  # Spatial information
  xy.grid.raw <- crds(r2)
  
  # Remove NAs
  sel <- which(complete.cases(grid))
  XData.grid <- grid[sel,]
  xy.grid <- data.frame(xy.grid.raw[sel,])
  
  # Size of prediction boxes
  xmin <- seq(ext(full_stack)[1]-1.0, ext(full_stack)[2]-1.0, by = 1.0)
  xmax <- seq(ext(full_stack)[1], ext(full_stack)[2], by = 1.0)
  ymin <- seq(ext(full_stack)[3]-1.0, ext(full_stack)[4]-1.0, by = 1.0)
  ymax <- seq(ext(full_stack)[3], ext(full_stack)[4], by = 1.0)
  
  #create a matrix of cells with and without data
  cells_with_data <- matrix(NA, nrow=length(ymin), ncol=length(xmin))
  for(j in 1:length(xmin)){
    # print(j)
    for(k in 1:length(ymin)){
      sel.loop <- which(xy.grid[,1]>xmin[j] & xy.grid[,1]<xmax[j] &
                          xy.grid[,2]>ymin[k] & xy.grid[,2]<ymax[k])
      print(sel.loop)
      if(length(sel.loop>0)){
        cells_with_data[k,j] <- 1
      }
    }}
  
  print(paste0("model fit:", i))
  
  ## parallel processing: PER CELL that contains values
  library(doParallel)
  library(foreach)
  parallel::detectCores()
  UseCores = parallel::detectCores()
  c1<-makeCluster(UseCores, outfile="", type="FORK") ## "FORK" is faster than "PSOCK", but only works on linux/mac
  registerDoParallel(c1)
  getDoParWorkers()
  
  cell.sel.v <- which(!is.na(cells_with_data))
  cell.sel.df <- which(!is.na(cells_with_data), arr.ind = TRUE)
  
  ptm = proc.time()
  foreach(l=1:length(cell.sel.v)) %dopar%{
    m <- cell.sel.df[l,2]
    n <- cell.sel.df[l,1]
    sel.loop <- which(xy.grid[,1]>xmin[m] & xy.grid[,1]<xmax[m] &
                        xy.grid[,2]>ymin[n] & xy.grid[,2]<ymax[n])
    print(l)
    
    if(length(sel.loop) == 1){
      XData.grid.loop <- as.data.frame(t(XData.grid[sel.loop,]))
    } else {
      XData.grid.loop <- as.data.frame(XData.grid[sel.loop,])
    }
    
    # XData.grid.loop$dn <- as.factor("night")
    XData.grid.loop <- add_column(XData.grid.loop, sst2 = XData.grid.loop$sst^2, .after = "sst")
    xy.grid.loop <- xy.grid[sel.loop,]
    
    full_p <- boral::predict.boral(fitSepTF, newX = XData.grid.loop, scale = "response",
                                   est = "mean", predict.type = "marginal", lv.mc = 100)
    
    bp <- full_p$linpred
    colnames(bp) <- sp.v
    
    ## save-string for 10km cell tiles
    run.name <- sprintf("%06d",cell.sel.v[l])
    
    save(
      bp, #predY.pa.up, predY.pa.low,
      sel.loop, XData.grid.loop, xy.grid.loop,
      file=paste0("~/full/pred",i,"/", run.name,".Rdata"))
    rm(bp)#predY.loop.pa, predY.pa.up, predY.pa.low)
  }
  computational.time = proc.time() - ptm
  parallel::stopCluster(cl = c1)
  
}

full_pred_mean <- list()

for (i in 1:length(boral_par)) {
  
  
  ## identify prediction files that we need to load
  pred.dir <- paste0("~/full/pred", i, "/")
  pred.files.raw <- list.files(pred.dir)
  pred.files.pa <- pred.files.raw
  
  # Set up PSOCK cluster (works in RStudio)
  num_cores <- parallel::detectCores() - 6  # Use all but one core
  cl <- parallel::makeCluster(num_cores)    # Create PSOCK cluster
  plan(cluster, workers = cl)               # Use PSOCK cluster
  
  # Function to process each file in parallel
  process_file <- function(file) {
    load(paste0(pred.dir, file))
    row.numbers <- as.numeric(rownames(xy.grid[sel.loop,]))
    list(
      sel.ra = sel.not.na[sel[row.numbers]],
      predY.mean = bp
    )
  }
  
  # Run in parallel
  results <- future_lapply(pred.files.pa, process_file)
  
  # Combine results
  all.sel.ra <- unlist(lapply(results, `[[`, "sel.ra"))
  full_pred_mean[[i]] <- do.call(rbind, lapply(results, `[[`, "predY.mean"))
  
  # Stop the cluster
  parallel::stopCluster(cl)
  plan(sequential)
}

full <- Reduce(`+`, full_pred_mean) / length(full_pred_mean)

##########

##### Initial stack #####
ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

ras_stack <- ras_stack[[c("Oct.2001", "Oct.2002", "Oct.2003", "Oct.2004", 
                          "Nov.2001", "Nov.2002", "Nov.2003", "Nov.2004", 
                          "Dec.2001", "Dec.2002", "Dec.2003", "Dec.2004",  
                          "Jan.2002", "Jan.2003", "Jan.2004", "Jan.2005", 
                          "Feb.2002", "Feb.2003", "Feb.2004", "Feb.2005", 
                          "Mar.2002", "Mar.2003", "Mar.2004", "Mar.2005")]]

sst_initial <- rast(calc(ras_stack, fun = mean, na.rm = TRUE))
values(sst_initial) <- scale(values(sst_initial))
chl_initial <- rast("simulated_chlorophyll_raster.tif")
values(chl_initial) <- scale(values(chl_initial))
dsStart_initial <- sst_initial
values(dsStart_initial) <- -1.224757 #  -1.224757/0.0000331176/1.224724 

# Add layer names
names(sst_initial) <- "sst"
names(dsStart_initial) <- "dsStart"
names(chl_initial) <- "chla"

# Merge all the layers into a single stack
initial_stack <- c(sst_initial, dsStart_initial, chl_initial)

rm(list = c("chl_initial", "dsStart_initial", "sst_initial", "ras_stack"))

#############################

for(i in 1:length(boral_par)){
  i = 1
  
  dir.create(paste0("~/initial/pred", i))
  
  fitSepTF <- boral_par[[i]]$boral_fit
  
  #assign species names
  sp.v <- colnames(fitSepTF$y)
  
  #create empty raster
  r2 <- initial_stack$dsStart
  empty.ra <- r2
  empty.ra[] <- NA
  
  # Identify which cells we have ignored before
  sel.not.na <- which(!is.na(r2[]))
  
  # Relevant env data
  grid <- initial_stack[]
  
  # Spatial information
  xy.grid.raw <- crds(r2)
  
  # Remove NAs
  sel <- which(complete.cases(grid))
  XData.grid <- grid[sel,]
  xy.grid <- data.frame(xy.grid.raw[sel,])
  
  # Size of prediction boxes
  xmin <- seq(ext(initial_stack)[1]-1.0, ext(initial_stack)[2]-1.0, by = 1.0)
  xmax <- seq(ext(initial_stack)[1], ext(initial_stack)[2], by = 1.0)
  ymin <- seq(ext(initial_stack)[3]-1.0, ext(initial_stack)[4]-1.0, by = 1.0)
  ymax <- seq(ext(initial_stack)[3], ext(initial_stack)[4], by = 1.0)
  
  #create a matrix of cells with and without data
  cells_with_data <- matrix(NA, nrow=length(ymin), ncol=length(xmin))
  for(j in 1:length(xmin)){
    # print(j)
    for(k in 1:length(ymin)){
      sel.loop <- which(xy.grid[,1]>xmin[j] & xy.grid[,1]<xmax[j] &
                          xy.grid[,2]>ymin[k] & xy.grid[,2]<ymax[k])
      print(sel.loop)
      if(length(sel.loop>0)){
        cells_with_data[k,j] <- 1
      }
    }}
  
  print(paste0("model fit:", i))
  
  ## parallel processing: PER CELL that contains values
  library(doParallel)
  library(foreach)
  parallel::detectCores()
  UseCores = parallel::detectCores()
  c1<-makeCluster(UseCores, outfile="", type="FORK") ## "FORK" is faster than "PSOCK", but only works on linux/mac
  registerDoParallel(c1)
  getDoParWorkers()
  
  cell.sel.v <- which(!is.na(cells_with_data))
  cell.sel.df <- which(!is.na(cells_with_data), arr.ind = TRUE)
  
  ptm = proc.time()
  foreach(l=1:length(cell.sel.v)) %dopar%{
    m <- cell.sel.df[l,2]
    n <- cell.sel.df[l,1]
    sel.loop <- which(xy.grid[,1]>xmin[m] & xy.grid[,1]<xmax[m] &
                        xy.grid[,2]>ymin[n] & xy.grid[,2]<ymax[n])
    print(l)
    
    if(length(sel.loop) == 1){
      XData.grid.loop <- as.data.frame(t(XData.grid[sel.loop,]))
    } else {
      XData.grid.loop <- as.data.frame(XData.grid[sel.loop,])
    }
    
    # XData.grid.loop$dn <- as.factor("night")
    XData.grid.loop <- add_column(XData.grid.loop, sst2 = XData.grid.loop$sst^2, .after = "sst")
    xy.grid.loop <- xy.grid[sel.loop,]
    
    initial_p <- boral::predict.boral(fitSepTF, newX = XData.grid.loop, scale = "response",
                                   est = "mean", predict.type = "marginal", lv.mc = 100)
    
    bp <- initial_p$linpred
    colnames(bp) <- sp.v
    
    ## save-string for 10km cell tiles
    run.name <- sprintf("%06d",cell.sel.v[l])
    
    save(
      bp, #predY.pa.up, predY.pa.low,
      sel.loop, XData.grid.loop, xy.grid.loop,
      file=paste0("~/initial/pred",i,"/", run.name,".Rdata"))
    rm(bp)#predY.loop.pa, predY.pa.up, predY.pa.low)
  }
  computational.time = proc.time() - ptm
  parallel::stopCluster(cl = c1)
  
}

initial_pred_mean <- list()

for (i in 1:length(boral_par)) {

  ## identify prediction files that we need to load
  pred.dir <- paste0("~/initial/pred", i, "/")
  pred.files.raw <- list.files(pred.dir)
  pred.files.pa <- pred.files.raw
  
  # Set up PSOCK cluster (works in RStudio)
  num_cores <- parallel::detectCores() - 6  # Use all but one core
  cl <- parallel::makeCluster(num_cores)    # Create PSOCK cluster
  plan(cluster, workers = cl)               # Use PSOCK cluster
  
  # Function to process each file in parallel
  process_file <- function(file) {
    load(paste0(pred.dir, file))
    row.numbers <- as.numeric(rownames(xy.grid[sel.loop,]))
    list(
      sel.ra = sel.not.na[sel[row.numbers]],
      predY.mean = bp
    )
  }
  
  # Run in parallel
  results <- future_lapply(pred.files.pa, process_file)
  
  # Combine results
  all.sel.ra <- unlist(lapply(results, `[[`, "sel.ra"))
  initial_pred_mean[[i]] <- do.call(rbind, lapply(results, `[[`, "predY.mean"))
  
  # Stop the cluster
  parallel::stopCluster(cl)
  plan(sequential)
}

initial <- Reduce(`+`, initial_pred_mean) / length(initial_pred_mean)

#########

##### Final stack #####
ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

ras_stack <- ras_stack[[c("Oct.2012", "Oct.2013", "Oct.2014", "Oct.2015", 
                          "Nov.2012", "Nov.2013", "Nov.2014", "Nov.2015",
                          "Dec.2012", "Dec.2013", "Dec.2014", "Dec.2015", 
                          "Jan.2013", "Jan.2014", "Jan.2015", "Jan.2016", 
                          "Feb.2013", "Feb.2014", "Feb.2015", "Feb.2016", 
                          "Mar.2013", "Mar.2014", "Mar.2015", "Mar.2016")]]

sst_final <- rast(calc(ras_stack, fun = mean, na.rm = TRUE))
values(sst_final) <- scale(values(sst_final))
chl_final <- rast("correlated_dummy_raster.tif")
values(chl_final) <- scale(values(chl_final))
dsStart_final <- sst_final
values(dsStart_final) <- 1.224724  

# Add layer names
names(sst_final) <- "sst"
names(dsStart_final) <- "dsStart"
names(chl_final) <- "chla"

# Merge all the layers into a single stack
final_stack <- c(sst_final, dsStart_final, chl_final)

rm(list = c("chl_final", "dsStart_final", "sst_final", "ras_stack"))































#############################

for(i in 1:length(boral_par)){
  
  i = 1
  
  dir.create(paste0("~/ch1_sc2/final/pred", i))
  
  fitSepTF <- boral_par[[i]]$boral_fit
  
  #assign species names
  sp.v <- colnames(fitSepTF$y)
  
  #create empty raster
  r2 <- final_stack$dsStart
  empty.ra <- r2
  empty.ra[] <- NA
  
  # Identify which cells we have ignored before
  sel.not.na <- which(!is.na(r2[]))
  
  # Relevant env data
  grid <- final_stack[]
  
  # Spatial information
  xy.grid.raw <- crds(r2)
  
  # Remove NAs
  sel <- which(complete.cases(grid))
  XData.grid <- grid[sel,]
  xy.grid <- data.frame(xy.grid.raw[sel,])
  
  # Size of prediction boxes
  xmin <- seq(ext(final_stack)[1]-1.0, ext(final_stack)[2]-1.0, by = 1.0)
  xmax <- seq(ext(final_stack)[1], ext(final_stack)[2], by = 1.0)
  ymin <- seq(ext(final_stack)[3]-1.0, ext(final_stack)[4]-1.0, by = 1.0)
  ymax <- seq(ext(final_stack)[3], ext(final_stack)[4], by = 1.0)
  
  #create a matrix of cells with and without data
  cells_with_data <- matrix(NA, nrow=length(ymin), ncol=length(xmin))
  for(j in 1:length(xmin)){
    # print(j)
    for(k in 1:length(ymin)){
      sel.loop <- which(xy.grid[,1]>xmin[j] & xy.grid[,1]<xmax[j] &
                          xy.grid[,2]>ymin[k] & xy.grid[,2]<ymax[k])
      print(sel.loop)
      if(length(sel.loop>0)){
        cells_with_data[k,j] <- 1
      }
    }}
  
  print(paste0("model fit:", i))
  
  ## parallel processing: PER CELL that contains values
  library(doParallel)
  library(foreach)
  parallel::detectCores()
  UseCores = parallel::detectCores()
  c1<-makeCluster(UseCores, outfile="", type="FORK") ## "FORK" is faster than "PSOCK", but only works on linux/mac
  registerDoParallel(c1)
  getDoParWorkers()
  
  cell.sel.v <- which(!is.na(cells_with_data))
  cell.sel.df <- which(!is.na(cells_with_data), arr.ind = TRUE)
  
  ptm = proc.time()
  foreach(l=1:length(cell.sel.v)) %dopar%{
    m <- cell.sel.df[l,2]
    n <- cell.sel.df[l,1]
    sel.loop <- which(xy.grid[,1]>xmin[m] & xy.grid[,1]<xmax[m] &
                        xy.grid[,2]>ymin[n] & xy.grid[,2]<ymax[n])
    print(l)
    
    if(length(sel.loop) == 1){
      XData.grid.loop <- as.data.frame(t(XData.grid[sel.loop,]))
    } else {
      XData.grid.loop <- as.data.frame(XData.grid[sel.loop,])
    }
    
    # XData.grid.loop$dn <- as.factor("night")
    XData.grid.loop <- add_column(XData.grid.loop, sst2 = XData.grid.loop$sst^2, .after = "sst")
    xy.grid.loop <- xy.grid[sel.loop,]
    
    full_p <- boral::predict.boral(fitSepTF, newX = XData.grid.loop, scale = "response",
                                   est = "mean", predict.type = "marginal", lv.mc = 100)
    
    bp <- full_p$linpred
    colnames(bp) <- sp.v
    
    ## save-string for 10km cell tiles
    run.name <- sprintf("%06d",cell.sel.v[l])
    
    save(
      bp, #predY.pa.up, predY.pa.low,
      sel.loop, XData.grid.loop, xy.grid.loop,
      file=paste0("~/ch1_sc2/final/pred",i,"/", run.name,".Rdata"))
    rm(bp)#predY.loop.pa, predY.pa.up, predY.pa.low)
  }
  computational.time = proc.time() - ptm
  parallel::stopCluster(cl = c1)
  
}

final_pred_mean <- list()

for (i in 1:length(boral_par)) {
  
  ## identify prediction files that we need to load
  pred.dir <- paste0("~/ch1_sc2/final/pred", i, "/")
  pred.files.raw <- list.files(pred.dir)
  pred.files.pa <- pred.files.raw
  
  # Set up PSOCK cluster (works in RStudio)
  num_cores <- parallel::detectCores() - 6  # Use all but one core
  cl <- parallel::makeCluster(num_cores)    # Create PSOCK cluster
  plan(cluster, workers = cl)               # Use PSOCK cluster
  
  # Function to process each file in parallel
  process_file <- function(file) {
    load(paste0(pred.dir, file))
    row.numbers <- as.numeric(rownames(xy.grid[sel.loop,]))
    list(
      sel.ra = sel.not.na[sel[row.numbers]],
      predY.mean = bp
    )
  }
  
  # Run in parallel
  results <- future_lapply(pred.files.pa, process_file)
  
  # Combine results
  all.sel.ra <- unlist(lapply(results, `[[`, "sel.ra"))
  final_pred_mean[[i]] <- do.call(rbind, lapply(results, `[[`, "predY.mean"))
  
  # Stop the cluster
  parallel::stopCluster(cl)
  plan(sequential)
}

final <- Reduce(`+`, final_pred_mean) / length(final_pred_mean)

#########










