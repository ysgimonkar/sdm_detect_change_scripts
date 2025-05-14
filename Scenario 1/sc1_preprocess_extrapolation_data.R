# Loading libraries
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

################## Extrapolation file ######################

ras_stack <- stack("SO_CPR_yr_month_sst_stack.grd")

ext_fun <- function(stack){
  
  # stack <- aggregate(stack, fact = 2)
  
  stack <- stack[[c("Oct.2001", "Oct.2002", "Oct.2003", "Oct.2004", "Oct.2005", "Oct.2006", "Oct.2007", "Oct.2008", "Oct.2009", "Oct.2010", "Oct.2011", "Oct.2012", "Oct.2013", "Oct.2014", "Oct.2015", 
                    "Nov.2001", "Nov.2002", "Nov.2003", "Nov.2004", "Nov.2005", "Nov.2006", "Nov.2007", "Nov.2008", "Nov.2009", "Nov.2010", "Nov.2011", "Nov.2012", "Nov.2013", "Nov.2014", "Nov.2015",
                    "Dec.2001", "Dec.2002", "Dec.2003", "Dec.2004", "Dec.2005", "Dec.2006", "Dec.2007", "Dec.2008", "Dec.2009", "Dec.2010", "Dec.2011", "Dec.2012", "Dec.2013", "Dec.2014", "Dec.2015", 
                    "Jan.2002", "Jan.2003", "Jan.2004", "Jan.2005", "Jan.2006", "Jan.2007", "Jan.2008", "Jan.2009", "Jan.2010", "Jan.2011", "Jan.2012", "Jan.2013", "Jan.2014", "Jan.2015", "Jan.2016", 
                    "Feb.2002", "Feb.2003", "Feb.2004", "Feb.2005", "Feb.2006", "Feb.2007", "Feb.2008", "Feb.2009", "Feb.2010", "Feb.2011", "Feb.2012", "Feb.2013", "Feb.2014", "Feb.2015", "Feb.2016", 
                    "Mar.2002", "Mar.2003", "Mar.2004", "Mar.2005", "Mar.2006", "Mar.2007", "Mar.2008", "Mar.2009", "Mar.2010", "Mar.2011", "Mar.2012", "Mar.2013", "Mar.2014", "Mar.2015", "Mar.2016")]]
  
  ras_df <- as.data.frame(stack, xy = TRUE)
  
  ras_df <- ras_df %>%
    rename(lon = x, lat = y) %>%
    pivot_longer(cols = c("Oct.2001", "Oct.2002", "Oct.2003", "Oct.2004", "Oct.2005", "Oct.2006", "Oct.2007", "Oct.2008", "Oct.2009", "Oct.2010", "Oct.2011", "Oct.2012", "Oct.2013", "Oct.2014", "Oct.2015", 
                          "Nov.2001", "Nov.2002", "Nov.2003", "Nov.2004", "Nov.2005", "Nov.2006", "Nov.2007", "Nov.2008", "Nov.2009", "Nov.2010", "Nov.2011", "Nov.2012", "Nov.2013", "Nov.2014", "Nov.2015",
                          "Dec.2001", "Dec.2002", "Dec.2003", "Dec.2004", "Dec.2005", "Dec.2006", "Dec.2007", "Dec.2008", "Dec.2009", "Dec.2010", "Dec.2011", "Dec.2012", "Dec.2013", "Dec.2014", "Dec.2015", 
                          "Jan.2002", "Jan.2003", "Jan.2004", "Jan.2005", "Jan.2006", "Jan.2007", "Jan.2008", "Jan.2009", "Jan.2010", "Jan.2011", "Jan.2012", "Jan.2013", "Jan.2014", "Jan.2015", "Jan.2016", 
                          "Feb.2002", "Feb.2003", "Feb.2004", "Feb.2005", "Feb.2006", "Feb.2007", "Feb.2008", "Feb.2009", "Feb.2010", "Feb.2011", "Feb.2012", "Feb.2013", "Feb.2014", "Feb.2015", "Feb.2016", 
                          "Mar.2002", "Mar.2003", "Mar.2004", "Mar.2005", "Mar.2006", "Mar.2007", "Mar.2008", "Mar.2009", "Mar.2010", "Mar.2011", "Mar.2012", "Mar.2013", "Mar.2014", "Mar.2015", "Mar.2016"), 
                 values_to = "sst", names_to = "month.year")%>%
    mutate(start_date = as.Date("2001-10-01"))
  
  ras_df <- ras_df %>%
    mutate(date = case_when(ras_df$month.year == "Oct.2001" ~  as.Date("2001-10-15"),
                            ras_df$month.year == "Oct.2002" ~  as.Date("2002-10-15"),
                            ras_df$month.year == "Oct.2003" ~  as.Date("2003-10-15"),
                            ras_df$month.year == "Oct.2004" ~  as.Date("2004-10-15"),
                            ras_df$month.year == "Oct.2005" ~  as.Date("2005-10-15"),
                            ras_df$month.year == "Oct.2006" ~  as.Date("2006-10-15"),
                            ras_df$month.year == "Oct.2007" ~  as.Date("2007-10-15"),
                            ras_df$month.year == "Oct.2008" ~  as.Date("2008-10-15"),
                            ras_df$month.year == "Oct.2009" ~  as.Date("2009-10-15"),
                            ras_df$month.year == "Oct.2010" ~  as.Date("2010-10-15"),
                            ras_df$month.year == "Oct.2011" ~  as.Date("2011-10-15"),
                            ras_df$month.year == "Oct.2012" ~  as.Date("2012-10-15"),
                            ras_df$month.year == "Oct.2013" ~  as.Date("2013-10-15"),
                            ras_df$month.year == "Oct.2014" ~  as.Date("2014-10-15"),
                            ras_df$month.year == "Oct.2015" ~  as.Date("2015-10-15"),
                            ras_df$month.year == "Nov.2001" ~  as.Date("2001-11-15"),
                            ras_df$month.year == "Nov.2002" ~  as.Date("2002-11-15"),
                            ras_df$month.year == "Nov.2003" ~  as.Date("2003-11-15"),
                            ras_df$month.year == "Nov.2004" ~  as.Date("2004-11-15"),
                            ras_df$month.year == "Nov.2005" ~  as.Date("2005-11-15"),
                            ras_df$month.year == "Nov.2006" ~  as.Date("2006-11-15"),
                            ras_df$month.year == "Nov.2007" ~  as.Date("2007-11-15"),
                            ras_df$month.year == "Nov.2008" ~  as.Date("2008-11-15"),
                            ras_df$month.year == "Nov.2009" ~  as.Date("2009-11-15"),
                            ras_df$month.year == "Nov.2010" ~  as.Date("2010-11-15"),
                            ras_df$month.year == "Nov.2011" ~  as.Date("2011-11-15"),
                            ras_df$month.year == "Nov.2012" ~  as.Date("2012-11-15"),
                            ras_df$month.year == "Nov.2013" ~  as.Date("2013-11-15"),
                            ras_df$month.year == "Nov.2014" ~  as.Date("2014-11-15"),
                            ras_df$month.year == "Nov.2015" ~  as.Date("2015-11-15"),
                            ras_df$month.year == "Dec.2001" ~  as.Date("2001-12-15"),
                            ras_df$month.year == "Dec.2002" ~  as.Date("2002-12-15"),
                            ras_df$month.year == "Dec.2003" ~  as.Date("2003-12-15"),
                            ras_df$month.year == "Dec.2004" ~  as.Date("2004-12-15"),
                            ras_df$month.year == "Dec.2005" ~  as.Date("2005-12-15"),
                            ras_df$month.year == "Dec.2006" ~  as.Date("2006-12-15"),
                            ras_df$month.year == "Dec.2007" ~  as.Date("2007-12-15"),
                            ras_df$month.year == "Dec.2008" ~  as.Date("2008-12-15"),
                            ras_df$month.year == "Dec.2009" ~  as.Date("2009-12-15"),
                            ras_df$month.year == "Dec.2010" ~  as.Date("2010-12-15"),
                            ras_df$month.year == "Dec.2011" ~  as.Date("2011-12-15"),
                            ras_df$month.year == "Dec.2012" ~  as.Date("2012-12-15"),
                            ras_df$month.year == "Dec.2013" ~  as.Date("2013-12-15"),
                            ras_df$month.year == "Dec.2014" ~  as.Date("2014-12-15"),
                            ras_df$month.year == "Dec.2015" ~  as.Date("2015-12-15"),
                            ras_df$month.year == "Jan.2002" ~  as.Date("2002-01-15"),
                            ras_df$month.year == "Jan.2003" ~  as.Date("2003-01-15"),
                            ras_df$month.year == "Jan.2004" ~  as.Date("2004-01-15"),
                            ras_df$month.year == "Jan.2005" ~  as.Date("2005-01-15"),
                            ras_df$month.year == "Jan.2006" ~  as.Date("2006-01-15"),
                            ras_df$month.year == "Jan.2007" ~  as.Date("2007-01-15"),
                            ras_df$month.year == "Jan.2008" ~  as.Date("2008-01-15"),
                            ras_df$month.year == "Jan.2009" ~  as.Date("2009-01-15"),
                            ras_df$month.year == "Jan.2010" ~  as.Date("2010-01-15"),
                            ras_df$month.year == "Jan.2011" ~  as.Date("2011-01-15"),
                            ras_df$month.year == "Jan.2012" ~  as.Date("2012-01-15"),
                            ras_df$month.year == "Jan.2013" ~  as.Date("2013-01-15"),
                            ras_df$month.year == "Jan.2014" ~  as.Date("2014-01-15"),
                            ras_df$month.year == "Jan.2015" ~  as.Date("2015-01-15"),
                            ras_df$month.year == "Jan.2016" ~  as.Date("2016-01-15"),
                            ras_df$month.year == "Feb.2002" ~  as.Date("2002-02-15"),
                            ras_df$month.year == "Feb.2003" ~  as.Date("2003-02-15"),
                            ras_df$month.year == "Feb.2004" ~  as.Date("2004-02-15"),
                            ras_df$month.year == "Feb.2005" ~  as.Date("2005-02-15"),
                            ras_df$month.year == "Feb.2006" ~  as.Date("2006-02-15"),
                            ras_df$month.year == "Feb.2007" ~  as.Date("2007-02-15"),
                            ras_df$month.year == "Feb.2008" ~  as.Date("2008-02-15"),
                            ras_df$month.year == "Feb.2009" ~  as.Date("2009-02-15"),
                            ras_df$month.year == "Feb.2010" ~  as.Date("2010-02-15"),
                            ras_df$month.year == "Feb.2011" ~  as.Date("2011-02-15"),
                            ras_df$month.year == "Feb.2012" ~  as.Date("2012-02-15"),
                            ras_df$month.year == "Feb.2013" ~  as.Date("2013-02-15"),
                            ras_df$month.year == "Feb.2014" ~  as.Date("2014-02-15"),
                            ras_df$month.year == "Feb.2015" ~  as.Date("2015-02-15"),
                            ras_df$month.year == "Feb.2016" ~  as.Date("2016-02-15"),
                            ras_df$month.year == "Mar.2002" ~  as.Date("2002-03-15"),
                            ras_df$month.year == "Mar.2003" ~  as.Date("2003-03-15"),
                            ras_df$month.year == "Mar.2004" ~  as.Date("2004-03-15"),
                            ras_df$month.year == "Mar.2005" ~  as.Date("2005-03-15"),
                            ras_df$month.year == "Mar.2006" ~  as.Date("2006-03-15"),
                            ras_df$month.year == "Mar.2007" ~  as.Date("2007-03-15"),
                            ras_df$month.year == "Mar.2008" ~  as.Date("2008-03-15"),
                            ras_df$month.year == "Mar.2009" ~  as.Date("2009-03-15"),
                            ras_df$month.year == "Mar.2010" ~  as.Date("2010-03-15"),
                            ras_df$month.year == "Mar.2011" ~  as.Date("2011-03-15"),
                            ras_df$month.year == "Mar.2012" ~  as.Date("2012-03-15"),
                            ras_df$month.year == "Mar.2013" ~  as.Date("2013-03-15"),
                            ras_df$month.year == "Mar.2014" ~  as.Date("2014-03-15"),
                            ras_df$month.year == "Mar.2015" ~  as.Date("2015-03-15"),
                            ras_df$month.year == "Mar.2016" ~  as.Date("2016-03-15"))) %>%
    mutate(dsStart = date - start_date) %>%
    dplyr::select(c(lon, lat, month.year, date, sst, dsStart))
  
  ras_df$dsStart <- as.integer(ras_df$dsStart)
  
  ras_df <-data.frame(intercept=1,ras_df[,1:ncol(ras_df)])
  
  ras_df <- ras_df %>%
    drop_na()
  
  full <- ras_df %>%
    group_by(lon, lat) %>%
    summarise(sst = mean(sst),
              dsStart = mean(dsStart)) %>%
    ungroup()
  
  initial <- ras_df %>%
    filter(date <= "2004-03-15") %>%
    group_by(lon, lat) %>%
    summarise(sst = mean(sst),
              dsStart = mean(dsStart)) %>%
    ungroup()
  
  final <- ras_df %>%
    filter(date >= "2013-10-15") %>%
    group_by(lon, lat) %>%
    summarise(sst = mean(sst),
              dsStart = mean(dsStart)) %>%
    ungroup()
  
  chl <- rast("correlated_dummy_raster.tif")
  
  
  chl_df <- as.data.frame(chl, xy = TRUE)
  
  chl_df <- chl_df %>%
    rename(lon = x,
           lat = y,
           chla = correlated_dummy_raster) %>%
    drop_na()
  
  full <- merge(full, chl_df, by = c("lon", "lat"))
  initial <- merge(initial, chl_df, by = c("lon", "lat"))
  final <- merge(final, chl_df, by = c("lon", "lat"))
  
  full <- full %>%
    drop_na() %>%
    add_column(data = "full")
  
  initial <- initial %>%
    drop_na() %>%
    add_column(data = "initial")
  
  final <- final %>%
    drop_na() %>%
    add_column(data = "final")
  
  temp <- rbind(initial, full, final)
  
  temp[,c(3:5)] <- scale(temp[,c(3:5)])
  
  a <- list()
  
  a[[1]] <- temp %>%
    filter(data == "full") %>%
    dplyr::select(-data)
  
  a[[2]] <- temp %>%
    filter(data == "initial") %>%
    dplyr::select(-data)
  
  a[[3]] <- temp %>%
    filter(data == "final") %>%
    dplyr::select(-data)
  
  names(a) <- c("full", "initial", "final")
  
  return(a)
  
  rm(list = c("stack", "ras_df", "full", "initial", "final", "chl", "temp"))
}

ext_data <- ext_fun(ras_stack)

rm(list = c("ext_fun", "ras_stack"))

# save the object
save(ext_data, file = "extrapolation_data.RData")
