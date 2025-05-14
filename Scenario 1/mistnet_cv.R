# Loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mistnet)
library(lubridate)
library(pROC)

# Loading simulation data from respective scenario

load("simulations.RData")

fit <- function(x, y, hyperparams, i){
  MNet_mod = mistnet(
    x = x,
    y = y,
    layer.definitions = list(
      defineLayer(
        nonlinearity = rectify.nonlinearity(),
        size = hyperparams$n.layer1[i],
        prior = gaussian.prior(mean = 0, sd = .1)
      ),
      
      defineLayer(
        nonlinearity = sigmoid.nonlinearity(),
        size = ncol(y),
        prior = gaussian.prior(mean = 0, sd = .1)
      )
    ),
    loss=bernoulliLoss(),
    updater = adagrad.updater(learning.rate = hyperparams$learning.rate[i]),
    sampler = gaussian.sampler(ncol = hyperparams$sampler.size[i], sd = 1),
    n.importance.samples = hyperparams$n.importance.samples[i],
    n.minibatch = hyperparams$n.minibatch[i],
    training.iterations = 0,
    initialize.biases = TRUE,
    initialize.weights = TRUE
  )
  MNet_mod$layers[[1]]$biases[] = 1 # First layer biases equal 1
  
  start.time = Sys.time()
  while(
    difftime(Sys.time(), start.time, units = "secs") < hyperparams$fit.seconds[i]
  ){
    MNet_mod$fit(100)
    cat(".")
    # Update prior variance
    for(layer in MNet_mod$layers){
      layer$prior$update(
        layer$weights, 
        update.mean = FALSE, 
        update.sd = TRUE,
        min.sd = .01
      )
    }
    # Update mean for final layer
    MNet_mod$layers[[2]]$prior$update(
      layer$weights, 
      update.mean = TRUE, 
      update.sd = FALSE,
      min.sd = .01
    )
  } # End while
  
  MNet_mod
}

data <- simulation1[[1]]

x <- as.matrix(data[,22:24])
y <- as.matrix(data[,1:18])

env_vars <- colnames(x)
species <- colnames(y)

hyperparams = data.frame(
  n.minibatch = 25, #default value
  sampler.size = rep(2:5, 9),
  n.importance.samples = 25, #default value
  n.layer1 = rep(c(8,10,12,14,16,18,20,22,24),each=4),
  learning.rate = 0.1,
  fit.seconds = 90)
  
# Cross-validation --------------------------------------------------------
fold.ids <- sample(x = 1:5, size = nrow(data),replace = TRUE)
  
out <- list()
  
for(i in 1:dim(hyperparams)[[1]]){
  T1 <- Sys.time()
  cat(paste0("Starting iteration ", i, "\n"))
  for(fold.id in 1:max(fold.ids)){
    cat(paste0(" Starting fold ", fold.id, "\n  "))
    in.val = fold.ids == fold.id   #added this line
    in.train = fold.ids != fold.id
    mnmod = fit(
      x=as.matrix(data[in.train,env_vars]),
      y=as.matrix(data[in.train,species]),
      hyperparams = hyperparams,
      i = i)
      
    cat("\n evaluating")
      
    #calculate log likelihood of predictions (replaced with my code)
      
    log.lik = sum(
      dbinom(
        as.matrix(data[in.val,species]),
        size = 1, 
        prob = predict(mnmod, as.matrix(data[in.val,env_vars]),n.importance.samples=dim(data[in.val,])[[1]]),
        log = TRUE))
    cat("\n")
      
    out[[length(out) + 1]] = c(iteration = i, 
                               fold = fold.id, 
                               seconds = hyperparams$fit.seconds[i],
                               loglik = mean(log.lik))
    
    T2 <- Sys.time()
    tm <- difftime(T2,T1) 
  }
}

mistnet.results = merge(
  x = as.data.frame(do.call(rbind, out)),
  y = cbind(iteration = 1:nrow(hyperparams), hyperparams)
)

logliks <- aggregate(mistnet.results, by=list(mistnet.results$iteration), mean)

# save(mistnet.results,
#      logliks, 
#      file = "mistnet_cv.RData")
