# This is for scenario 4 only

# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ecomix)
library(lubridate)

# Loading data

load("krill_data.RData")

sim_dat <- function(data){
  
  env <- data[, c(22:26)]
  
  env_num <- data.matrix(env) 
  
  env_data <-data.frame(intercept=1,env_num[,1:ncol(env_num)]) 
  
  env_data[,c(4:6)] <-scale(env_data[,c(4:6)])
  
  sps <- data[,1:18]  
  
  prev <- colSums(sps)/nrow(sps) 
  
  prev_num <- unname(prev) 
  
  prev.sf <- rnorm(18, mean = -2.8, sd = 1.7) 
  
  alpha.sf <- exp(prev.sf) / (1+exp(prev.sf))
  
  betas <- as.matrix(data.frame(sst = c(0, 1.5, -1.5, 1),
                                sst2 = c(0, 0, 0, -1),
                                dsStart = c(1.5, -0.05, 0.25, -0.25),
                                chla = c(0, 0, 0, 0)))
  
  rownames(betas) <- paste0("Archetype",1:4)
  
  sites <- sample(1:nrow(env_data), nrow(data), replace=FALSE)
  
  env_region <- env_data[sites,]
  
  sam_form <- stats::as.formula(paste0('cbind(',paste(paste0('spp',1:18), collapse =','),")~ poly(sst, degree=2, raw=TRUE) + dsStart + chla")) 
  
  sp_form <- ~1
  
  set.seed(420)
  sim_data <- species_mix.simulate(archetype_formula = sam_form,
                                   species_formula = sp_form,
                                   alpha = alpha.sf,
                                   beta = betas,
                                   data = env_region,
                                   nArchetypes = 4,
                                   family = "bernoulli")
  
  spdata <- sim_data[,1:18]
  sim <- cbind(spdata,env_region)
  
  a <- list()
  for(i in 1:100){
    a[[i]] <- sim[sample(nrow(sim), 250),]
  }
  
  b <- list()
  b[[1]] <- attributes(sim_data)
  
  simulation <- list()
  simulation[[1]] <- a
  simulation[[2]] <- b
  
  names(simulation) <- c("datasets", "info")
  
  return(simulation)
}

simulation4 <- sim_dat(processed)

# save(simulation4, file = "sc4_simulations.RData")


