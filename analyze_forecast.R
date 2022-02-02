rm(list = ls())
# ROMS example:
# Explore ROMS outputs:

setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
require(raster)
library(gapminder)
library(ggplot2)
library(wesanderson)
library(magick)
require(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
require(data.table)
library(mapdata)
library(marmap)
library(tidyverse)
require(mapproj)
require(reshape2)
library(gganimate)
require(lubridate)
require(scales)
require(rnaturalearth)
require(rnaturalearthdata)
library(gridExtra)
require(sf)
source('aux_functions.R')
#load('BathyData.RData')
bathy1 = read.csv('main_files/bathy1.csv')
bathy2 = read.csv('main_files/bathy2.csv')

# Read bathymetry information ---------------------------------------------

ak = map_data('worldHires','USA:Alaska')
world = ne_countries(scale = "medium", returnclass = "sf")
ini_number = 8E+6 # check this number in initial data file csv. 
#surv_Ndays = 100
#surv_Len = 25

# Read output files -------------------------------------------------------
# Here I will read every results file and get the appropiate format to make every figure.
# This approach is better since files will be super large for the forcast period and 1 h time step

thLow1 = 0.025
thHigh1 = 0.975
thLow2 = 0.25
thHigh2 = 0.75
alphaLevel = 0.2
main_folder = 'save_outputs' # read DisMELS outputs from
save_folder = 'output_data/GFDL_main' # save lists created here to plot
n_releases = 3
min_depth = -1000
n_grids = 269

# Select folders to read:
scenarios = list.files(main_folder)[1:2]
scenarioLevels = c("GFDL_rcp85_all_exp", "GFDL_rcp45_all_exp")
#mainPal = 'Set1'
mainCols = c(brewer.pal(4,"Set1")[c(4,3)], '#000000', '#000000', '#000000')

plot_data_0a = list()
plot_data_0b = list()
plot_data_0c = list()
plot_data_0d = list()
plot_data_0e = list()
plot_data_0f = list()
plot_data_0g = list()
plot_data_0h = list()
plot_data_1a = list()
plot_data_1b = list()
plot_data_1c = list()
plot_data_2a = list()
plot_data_2b = list()
plot_data_2c = list()
plot_data_3a = list()
plot_data_3b = list()
plot_data_3c = list()
plot_data_4a = list()
plot_data_4b = list()
plot_data_4c = list()
plot_data_5a = list()
plot_data_5b = list()
plot_data_5c = list()
plot_data_6 = list()
plot_data_7 = list()
plot_data_8a = list()
plot_data_8b = list()
plot_data_8c = list()
plot_data_9a = list()
plot_data_9b = list()
plot_data_9c = list()
plot_data_10a = list()
plot_data_10b = list()
plot_data_11a = list()
plot_data_11b = list()
plot_data_12 = list()
plot_data_13 = list()
plot_data_14 = list()

indList = 1

for(k in seq_along(scenarios)) {

  mod_year = list.files(path = file.path(main_folder, scenarios[k]))
  
    for(j in seq_along(mod_year)) {
      
      # Read all results CSV:
      tmpData = read_data_in(eggInclude = FALSE, 
                             path = file.path(main_folder, scenarios[k], mod_year[j]))
      tmpData$ageYSLround = round(tmpData$ageYSL)
      tmpData$LenRound = round(tmpData$SL)
      tmpData$md_inx = lubridate::month(x = tmpData$time) + (lubridate::day(x = tmpData$time)/31)
      tmpData = tmpData[tmpData$md_inx < 10.04, ] # Max date: Oct 1st
      
      # Base initial points: (all fish) Just do it once
      if(k & j == 1) {
        base_locs = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
        base_locs$horizPos1 = ifelse(test = base_locs$horizPos1 > 0, yes = base_locs$horizPos1 - 360, 
                                       no = base_locs$horizPos1)
        base_locs$horizPos1 = base_locs$horizPos1 + 360
        base_locs = base_locs[order(base_locs$id), ]
      }
      
      # Section 0 ----------
      # Subset only fish passed to FDL
      fdl_ind = unique(tmpData[tmpData$typeName == 'FDL', 'id'])
      fdata = data.frame(year = unique(tmpData$year), id = fdl_ind$id, scenario = scenarios[k])
      plot_data_0a[[indList]] = fdata
      tmpData2 = tmpData[tmpData$id %in% fdata$id, ] # exclude them
      
      # Exclude only fish DW < 0.75 DWmax:
      dw_ind = tmpData[which(tmpData$mortstarv >= 1000),]
      dw_in = base_locs$id[!(base_locs$id %in% unique(dw_ind$id))]
      dw_data = data.frame(year = unique(tmpData$year), id = dw_in, scenario = scenarios[k])
      plot_data_0b[[indList]] = dw_data
      tmpData2 = tmpData2[tmpData2$id %in% dw_data$id, ] # exclude them
      tmpData3 = tmpData2 # save this for spatial distribution analysis
      
      # Subset only fish that end up within the EBS:
      in_ind = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
      selIn = in_ind$id[which(in_ind$horizPos1 < 0)]
      in_data = data.frame(year = unique(tmpData$year), id = selIn, scenario = scenarios[k])
      plot_data_0c[[indList]] = in_data
      tmpData2 = tmpData2[tmpData2$id %in% selIn, ] # exclude them
                  
      # Number of fish alive by Oct 1st:
      alive_id = unique(tmpData2$id)
      alive_data = data.frame(year = unique(tmpData2$year), id = alive_id, scenario = scenarios[k])
      plot_data_0d[[indList]] = alive_data
      
      # Save data to plot maps dead individuals:
      plot_data_0e[[indList]] = fdata
      plot_data_0f[[indList]] = dw_data
      plot_data_0g[[indList]] = in_data
      plot_data_0h[[indList]] = alive_data
      
      # Find initial and final points (only alive id + out of EBS)
      init_points = tmpData3[tmpData3[ , .I[which.min(time)], by = id]$V1]
      end_points = tmpData3[tmpData3[ , .I[which.max(time)], by = id]$V1]
      
      # Section 1 ----------
      # Hatching success
      num_data = aggregate(x = list(number = tmpData2$number), list(year = tmpData2$year, 
                                                                   stage = tmpData2$typeName, id = tmpData2$id), 
                           FUN = function(x) mean(x))
      num_dataYSL = num_data[num_data$stage == 'YSL', ]
      num_dataYSL$hatsuc = num_dataYSL$number/ini_number
      # Prepare data to save:
      sel_var = 'hatsuc'
      toPlotData = num_dataYSL
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      colnames(prevAnom)[3] = 'value'
      prevAnom$scenario = scenarios[k]
      plot_data_1a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - mean(prevAnom$value)
      plot_data_1b[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_1c[[indList]] = fdata
      
      # Section 2 ----------
      # Survival 100 dph (only alive ID)
      survData = tmpData2[tmpData2$ageYSLround == 100, ]
      survData$psurvival = survData$psurvival*1e-6
      surv_data = aggregate(x = list(psurv = survData$psurvival), list(year = survData$year, id = survData$id),
                            FUN = mean, na.rm = TRUE)
      # Prepare data to save:
      sel_var = 'psurv'
      toPlotData = surv_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      prevAnom$scenario = scenarios[k]
      colnames(prevAnom)[3] = 'value'
      plot_data_2a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - mean(prevAnom$value)
      plot_data_2b[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_2c[[indList]] = fdata
      
      # Section 3 ----------
      # Survival 30 dph (only alive ID)
      survData = tmpData2[tmpData2$ageYSLround == 30, ]
      survData$psurvival = survData$psurvival*1e-6
      surv_data = aggregate(x = list(psurv = survData$psurvival), list(year = survData$year, id = survData$id),
                            FUN = mean, na.rm = TRUE)
      # Prepare data to save:
      sel_var = 'psurv'
      toPlotData = surv_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      prevAnom$scenario = scenarios[k]
      colnames(prevAnom)[3] = 'value'
      plot_data_3a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - mean(prevAnom$value)
      plot_data_3b[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_3c[[indList]] = fdata
      
      # Section 4 ----------
      # Standard length
      sl_data = aggregate(x = list(sl = tmpData2$SL), list(year = tmpData2$year, id = tmpData2$id), 
                          FUN = max, na.rm=TRUE)
      # Prepare data to save:
      sel_var = 'sl'
      toPlotData = sl_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      prevAnom$scenario = scenarios[k]
      colnames(prevAnom)[3] = 'value'
      plot_data_4a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - mean(prevAnom$value)
      plot_data_4b[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_4c[[indList]] = fdata
      
      # Section 5 ----------
      #  DW
      wgt_data = aggregate(x = list(dw = tmpData2$DW), list(year = tmpData2$year, id = tmpData2$id), 
                           FUN = max, na.rm=TRUE)
      # Prepare data to save:
      sel_var = 'dw'
      toPlotData = wgt_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', sel_var)])
      prevAnom$scenario = scenarios[k]
      colnames(prevAnom)[3] = 'value'
      plot_data_5a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - mean(prevAnom$value)
      plot_data_5b[[indList]] = prevAnom
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_5c[[indList]] = fdata
      
      # Section 6 ----------
      # Mortality: 100 dph
      myData = tmpData2[(tmpData2$ageYSLround <= 100), ]
      myData2 = myData[(myData$progYSA >= 1 | is.na(myData$progYSA)), ]
      myData = myData[which(myData$mortstarv < 10), ]
      myData2 = myData2[which(myData2$mortstarv < 10), ]
      #Calculate fish predator + invertebrate mortality
      myData3 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv), 
                          list(year = myData$year, id = myData$id), FUN = mean)
      myData4 = myData3[,-2]
      myData5 = myData4 %>% 
        gather('type', 'value', -year)
      #Find prop starvation:
      myData6 = aggregate(list(value = myData2$mortstarv), 
                          list(year = myData2$year, id = myData2$id), FUN = function(x) sum(x>0)/length(x))
      myData6$id = 'mortstarv'
      colnames(myData6)[2] = 'type'
      # Merge both data:
      myData7 = rbind(myData5, myData6)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = myData7
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, type = p50$type, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_6[[indList]] = fdata
      
      # Section 7 ----------
      # Mortality: 30 dph
      myData = tmpData2[(tmpData2$ageYSLround <= 30), ]
      myData2 = myData[(myData$progYSA >= 1 | is.na(myData$progYSA)), ]
      myData = myData[which(myData$mortstarv < 10), ]
      myData2 = myData2[which(myData2$mortstarv < 10), ]
      #Calculate fish predator + invertebrate mortality
      myData3 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv), 
                          list(year = myData$year, id = myData$id), FUN = mean)
      myData4 = myData3[,-2]
      myData5 = myData4 %>% 
        gather('type', 'value', -year)
      #Find prop starvation:
      myData6 = aggregate(list(value = myData2$mortstarv), 
                          list(year = myData2$year, id = myData2$id), FUN = function(x) sum(x>0)/length(x))
      myData6$id = 'mortstarv'
      colnames(myData6)[2] = 'type'
      # Merge both data:
      myData7 = rbind(myData5, myData6)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = myData7
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, type = toPlotData$type), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, type = p50$type, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_7[[indList]] = fdata
      
      # Section 8 ----------
      # Environmental variables (alive + dead)
      stageData = tmpData2[tmpData2$typeName %in% c('YSL', 'FDL', 'FDLpf', 'Epijuv'), ] 
      env_data = aggregate(x = list(temperature = stageData$temp, pCO2 = stageData$pCO2val), 
                           list(year = stageData$year, id = stageData$id), 
                           FUN = mean, na.rm=TRUE)
      int_data = gather(env_data, key = "variable", value = "value", temperature, pCO2)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = int_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', 'variable', sel_var)])
      prevAnom$scenario = scenarios[k]
      plot_data_8a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - ave(prevAnom$value, prevAnom$variable)
      plot_data_8b[[indList]] = prevAnom      
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, variable = p50$variable, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_8c[[indList]] = fdata
      
      # Section 9 ----------
      # Prey density field
      prey_data = aggregate(x = list(copepods = stageData$copepod, microzoo = stageData$microzoo, 
                                     neocalanus = stageData$neocalanus, neocalanusShelf = stageData$neocalanusShelf, 
                                     euphausiids = stageData$euphausiid, euphausiidsShelf = stageData$euphausiidShelf), 
                            list(year = stageData$year, id = stageData$id), 
                            FUN = mean, na.rm=TRUE)
      int_data = gather(prey_data, key = "variable", value = "value",
                        copepods, microzoo, neocalanus, neocalanusShelf, euphausiids, euphausiidsShelf)
      # Prepare data to save:
      sel_var = 'value'
      toPlotData = int_data
      prevAnom = data.frame(toPlotData[,c('year', 'id', 'variable', sel_var)])
      prevAnom$scenario = scenarios[k]
      plot_data_9a[[indList]] = prevAnom
      prevAnom$value2 = prevAnom$value - ave(prevAnom$value, prevAnom$variable)
      plot_data_9b[[indList]] = prevAnom          
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, variable = toPlotData$variable), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, variable = p50$variable, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_9c[[indList]] = fdata
      
      # Section 10 ----------
      # Diet species (only alive individuals)
      rank_data = aggregate(x = list(rank = tmpData2$avgRank), list(year = tmpData2$year, 
                                                                   stage = tmpData2$typeName, id = tmpData2$id), 
                            FUN = mean, na.rm=TRUE)
      rank_data = rank_data[!(rank_data$stage %in% c('YSL')), ] # exclude YSL
      # Prepare data to save:
      sel_var = 'rank'
      toPlotData = rank_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_10a[[indList]] = fdata
  
      # Diet size
      size_data = aggregate(x = list(sized = tmpData2$avgSize), list(year = tmpData2$year, stage = 
                                                                      tmpData2$typeName, id = tmpData2$id), 
                            FUN = mean, na.rm=TRUE)
      size_data = size_data[!(size_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
      # Prepare data to save:
      sel_var = 'sized'
      toPlotData = size_data
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_10b[[indList]] = fdata

      # Section 11 -----------
      # Calculate distance per id:
      mergePoints = rbind(init_points[,c('horizPos1', 'horizPos2', 'id')],
                          end_points[,c('horizPos1', 'horizPos2', 'id')])
      distMat = mergePoints %>%
        group_by(id)%>%
        group_map(~raster::pointDistance(.x[,c('horizPos1', 'horizPos2')], lonlat=TRUE)) %>%
        setNames(unique(sort(mergePoints$id)))
      distVals = unlist(distMat)
      distVals = distVals[distVals > 0 & !is.na(distVals)]
      distValsNm = (distVals/111000)*60 # units: nm
      preDist = data.frame(dist = distValsNm, year = unique(tmpData$year)[1])
      preDist$id = as.numeric(names(distMat))
      # Prepare data to save:
      sel_var = 'dist'
      toPlotData = preDist
      p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = median)
      p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                       FUN = quantile, probs = thLow1, na.rm = TRUE)
      p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                        FUN = quantile, probs = thHigh1, na.rm = TRUE)
      p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thLow2, na.rm = TRUE)
      p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year), 
                      FUN = quantile, probs = thHigh2, na.rm = TRUE)
      fdata = data.frame(year = p50$year, q50 = p50$var, q25 = p25$var, q75 = p75$var,
                         q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
      plot_data_11a[[indList]] = fdata
      preDist$scenario = scenarios[k]
      plot_data_11b[[indList]] = preDist

      # Section 12 -----------
      # Calculate direction
      direcMat = mergePoints %>%
        group_by(id)%>%
        group_map(~geosphere::bearing(.x[,c('horizPos1', 'horizPos2')])) %>%
        setNames(unique(sort(mergePoints$id)))
      direcVals = unlist(direcMat)
      direcVals = direcVals[!is.na(direcVals)]
      direcValsNm = ifelse(test = direcVals < 0, yes = 360+direcVals, no = direcVals)
      preDirec = data.frame(dist = direcValsNm, year = unique(tmpData$year)[1])
      preDirec$id = as.numeric(names(direcMat))
      preDirec$scenario = scenarios[k]
      plot_data_12[[indList]] = preDirec
      
      # Section 13 -----------
      # Calculate CG and Inertia:
      init_points$horizPos1 = ifelse(test = init_points$horizPos1 > 0, yes = init_points$horizPos1 - 360, 
                                 no = init_points$horizPos1)
      init_points$horizPos1 = init_points$horizPos1 + 360
      end_points$horizPos1 = ifelse(test = end_points$horizPos1 > 0, yes = end_points$horizPos1 - 360, 
                                     no = end_points$horizPos1)
      end_points$horizPos1 = end_points$horizPos1 + 360

      spatInfoIni = init_points %>%
        group_by(year)%>%
        group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
      spatInfoEnd = end_points %>%
        group_by(year)%>%
        group_map(~cgi(x = .x$horizPos1, y = .x$horizPos2))
      plot_data_13[[indList]] = data.frame(CG_x_ini = sapply(spatInfoIni, "[[", 1), 
                                      CG_x_end = sapply(spatInfoEnd, "[[", 1),
                                      CG_y_ini = sapply(spatInfoIni, "[[", 2),
                                      CG_y_end = sapply(spatInfoEnd, "[[", 2),
                                      I_ini = sapply(spatInfoIni, "[[", 3), 
                                      I_end = sapply(spatInfoEnd, "[[", 3),
                                      year = unique(tmpData$year)[1],
                                      scenario = scenarios[k])
      # Section 14 -----------
      # final locations to plot density
      endLocs_df = end_points[,c('id', 'horizPos1', 'horizPos2', 'year')] 
      endLocs_df$scenario = scenarios[k]
      plot_data_14[[indList]] = endLocs_df  
      
      # Get to next indicator:
      print(indList)
      indList = indList + 1
      
    }
    
}

# Info required to map:
baseLocs = base_locs[,c('id', 'horizPos1', 'horizPos2', 'vertPos')]
baseLocs$id_grid = rep(x = 1:n_grids, times = n_releases)
baseLocs2 = aggregate(list(lon = baseLocs$horizPos1, lat = baseLocs$horizPos2, depth = baseLocs$vertPos), 
                      list(id = baseLocs$id_grid), unique)
baseLocs3 = purrr::map_dfr(1:2, ~baseLocs2[,c('id', 'lon', 'lat')])
baseLocs3$scenario = rep(x = scenarios, each = n_grids)

# Save DF created:
save(plot_data_0a, file = file.path(save_folder, 'plot_data_0a.RData'))
save(plot_data_0b, file = file.path(save_folder, 'plot_data_0b.RData'))
save(plot_data_0c, file = file.path(save_folder, 'plot_data_0c.RData'))
save(plot_data_0d, file = file.path(save_folder, 'plot_data_0d.RData'))
save(plot_data_0e, file = file.path(save_folder, 'plot_data_0e.RData'))
save(plot_data_0f, file = file.path(save_folder, 'plot_data_0f.RData'))
save(plot_data_0g, file = file.path(save_folder, 'plot_data_0g.RData'))
save(plot_data_0h, file = file.path(save_folder, 'plot_data_0h.RData'))
save(plot_data_1a, file = file.path(save_folder, 'plot_data_1a.RData'))
save(plot_data_1b, file = file.path(save_folder, 'plot_data_1b.RData'))
save(plot_data_1c, file = file.path(save_folder, 'plot_data_1c.RData'))
save(plot_data_2a, file = file.path(save_folder, 'plot_data_2a.RData'))
save(plot_data_2b, file = file.path(save_folder, 'plot_data_2b.RData'))
save(plot_data_2c, file = file.path(save_folder, 'plot_data_2c.RData'))
save(plot_data_3a, file = file.path(save_folder, 'plot_data_3a.RData'))
save(plot_data_3b, file = file.path(save_folder, 'plot_data_3b.RData'))
save(plot_data_3c, file = file.path(save_folder, 'plot_data_3c.RData'))
save(plot_data_4a, file = file.path(save_folder, 'plot_data_4a.RData'))
save(plot_data_4b, file = file.path(save_folder, 'plot_data_4b.RData'))
save(plot_data_4c, file = file.path(save_folder, 'plot_data_4c.RData'))
save(plot_data_5a, file = file.path(save_folder, 'plot_data_5a.RData'))
save(plot_data_5b, file = file.path(save_folder, 'plot_data_5b.RData'))
save(plot_data_5c, file = file.path(save_folder, 'plot_data_5c.RData'))
save(plot_data_6, file = file.path(save_folder, 'plot_data_6.RData'))
save(plot_data_7, file = file.path(save_folder, 'plot_data_7.RData'))
save(plot_data_8a, file = file.path(save_folder, 'plot_data_8a.RData'))
save(plot_data_8b, file = file.path(save_folder, 'plot_data_8b.RData'))
save(plot_data_8c, file = file.path(save_folder, 'plot_data_8c.RData'))
save(plot_data_9a, file = file.path(save_folder, 'plot_data_9a.RData'))
save(plot_data_9b, file = file.path(save_folder, 'plot_data_9b.RData'))
save(plot_data_9c, file = file.path(save_folder, 'plot_data_9c.RData'))
save(plot_data_10a, file = file.path(save_folder, 'plot_data_10a.RData'))
save(plot_data_10b, file = file.path(save_folder, 'plot_data_10b.RData'))
save(plot_data_11a, file = file.path(save_folder, 'plot_data_11a.RData'))
save(plot_data_11b, file = file.path(save_folder, 'plot_data_11b.RData'))
save(plot_data_12, file = file.path(save_folder, 'plot_data_12.RData'))
save(plot_data_13, file = file.path(save_folder, 'plot_data_13.RData'))
save(plot_data_14, file = file.path(save_folder, 'plot_data_14.RData'))
save(baseLocs, file = file.path(save_folder, 'baseLocs.RData'))


# Analyze results: temporal series ---------------------------------------------------------
# -------------------------------------------------------------------------

# Hatching success --------------------------------------------------------

plot_data = bind_rows(plot_data_1c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

bio1 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')


# Plot 2: Survival 100 days --------------------------------------------------------

plot_data = bind_rows(plot_data_2c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$q50 = log(plot_data$q50)
plot_data$q25 = log(plot_data$q25)
plot_data$q75 = log(plot_data$q75)
plot_data$q5 = log(plot_data$q5)
plot_data$q95 = log(plot_data$q95)

bio2 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability (100 dph)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')


# Plot 3: standard length --------------------------------------------------------

plot_data = bind_rows(plot_data_4c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

bio3 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')


# Make biological variables plot ------------------------------------------

png(filename = 'figures/fore_biovar.png', width = 190, height = 60, 
    units = 'mm', res = 500)
grid.arrange(bio1, bio2, bio3, nrow = 1)
dev.off()


# Plot: Compare standard length with obs --------------------------------------------------------

plot_data = bind_rows(plot_data_4c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data = plot_data[plot_data$year <= 2020, ]

slcomp = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2011, to = 2019, by = 2)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')


# Read obs data (ML):
sl_obs = read.csv('Compare_ML_len.csv')
sl_obs$value = sl_obs$value * 10
sl_obs = sl_obs[sl_obs$year >= 2010 & sl_obs$year <= 2020, ]
sl_obs$type = factor(sl_obs$type, levels = c('ML_0', 'ML_1'))
sl_obs$type2 = factor(sl_obs$type, labels = c("Mean~length~age-0~(mm)", 'Mean~length~age-1~(mm)'))

# Plot:
slobs = ggplot(sl_obs, aes(x = year)) + 
  geom_line(aes(y = value), colour = 'black') +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2011, to = 2019, by = 2)) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ type2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, nrow = 2) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")


# Merge plots:
lay = matrix(c(1,2,2), nrow = 3)
png(filename = 'figures/fore_SL_compare.png', width = 95, height = 190, 
    units = 'mm', res = 500)
grid.arrange(slcomp, slobs, layout_matrix = lay)
dev.off()


# Plot 4: Survival 30 days --------------------------------------------------------

plot_data = bind_rows(plot_data_3c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$q50 = log(plot_data$q50)
plot_data$q25 = log(plot_data$q25)
plot_data$q75 = log(plot_data$q75)
plot_data$q5 = log(plot_data$q5)
plot_data$q95 = log(plot_data$q95)

sur30 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability (30 dph)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')


# Make plot
png(filename = 'figures/fore_surv.png', width = 95, height = 190, 
    units = 'mm', res = 500)
grid.arrange(bio2, sur30, nrow = 2)
dev.off()


# Plot 5: Mortality by category: 100 dph -------------------------------------
plot_data = bind_rows(plot_data_6, .id = "column_label")
plot_data_a = plot_data[plot_data$type %in% c('mortfish', 'mortinv'), ]
plot_data_a$scenario = factor(plot_data_a$scenario, levels = scenarioLevels)
plot_data_a$type = factor(plot_data_a$type, levels = c("mortfish", "mortinv"))
plot_data_a$type = factor(plot_data_a$type, labels = c('Fish predation', 'Invertebrate predation'))

mor100 = ggplot(plot_data_a, aes(x = year)) + 
        geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
        #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
        geom_line(aes(y = q50, colour = scenario)) +
        theme_bw() +
        xlab(NULL) +
        ylab(expression(paste('Mortality 100 dph (', s^{-1}, ', ', 10^{-6}, ')'))) +
        scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
        scale_color_manual(values = mainCols) +
        scale_fill_manual(values = mainCols) +
        theme(legend.position = 'none') +
        facet_wrap(~type, ncol = 2, scales = 'free_y')


# % time steps with starvation:
plot_data_b = plot_data[plot_data$type %in% 'mortstarv', ]
plot_data_b$scenario = factor(plot_data_b$scenario, levels = scenarioLevels)

strv100 = ggplot(plot_data_b, aes(x = year)) + 
        geom_ribbon(aes(ymin = q5*100, ymax = q95*100, fill = scenario), alpha=alphaLevel) +
        #geom_ribbon(aes(ymin = q25*100, ymax = q75*100, fill = scenario), alpha=0.2) +
        geom_line(aes(y = q50*100, colour = scenario)) +
        theme_bw() +
        xlab(NULL) +
        ylab('% time steps with starvation') +
        scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
        scale_color_manual(values = mainCols) +
        scale_fill_manual(values = mainCols) +
        theme(legend.position = 'none') 


# Plot 6: Mortality by category: 30 dph -------------------------------------
plot_data = bind_rows(plot_data_7, .id = "column_label")
plot_data_a = plot_data[plot_data$type %in% c('mortfish', 'mortinv'), ]
plot_data_a$scenario = factor(plot_data_a$scenario, levels = scenarioLevels)
plot_data_a$type = factor(plot_data_a$type, levels = c("mortfish", "mortinv"))
plot_data_a$type = factor(plot_data_a$type, labels = c('Fish predation', 'Invertebrate predation'))

mor30 = ggplot(plot_data_a, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab(expression(paste('Mortality 30 dph (', s^{-1}, ', ', 10^{-6}, ')'))) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(~type, ncol = 2, scales = 'free_y')

# % time steps with starvation:
plot_data_b = plot_data[plot_data$type %in% 'mortstarv', ]
plot_data_b$scenario = factor(plot_data_b$scenario, levels = scenarioLevels)

strv30 = ggplot(plot_data_b, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5*100, ymax = q95*100, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25*100, ymax = q75*100, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50*100, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('% time steps with starvation') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') 


# Make mortality plot ------------------------------------------

lay = rbind(c(1,1,2),
            c(3,3,4))
png(filename = 'figures/fore_mort_types.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(mor100, strv100, mor30, strv30, layout_matrix = lay)
dev.off()



# Plot 7: Dead individuals -------------------------------------
n_id = 807

# reached PNR:
plot_data = bind_rows(plot_data_0a, .id = "column_label")
plot_data = aggregate(list(porcsurv = plot_data$id), list(year = plot_data$year, scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

dead1 = ggplot(plot_data, aes(x = year, y = 1-porcsurv, color = scenario)) + 
        geom_line() +
        theme_bw() +
        xlab(NULL) +
        ylab('Proportion reached PNR') +
        scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
        scale_color_manual(values = mainCols) +
        theme(legend.position = 'none') 

# Num DW < 0.75*DWmax: dead from starvation
plot_data = bind_rows(plot_data_0b, .id = "column_label")
plot_data = aggregate(list(in_id = plot_data$id), list(year = plot_data$year, scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

dead2 = ggplot(plot_data, aes(x = year, y = 1-in_id, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab('Proportion starved') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') 


# Num ind out of EBS
plot_data = bind_rows(plot_data_0c, .id = "column_label")
plot_data = aggregate(list(in_id = plot_data$id), list(year = plot_data$year, scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

dead3 = ggplot(plot_data, aes(x = year, y = 1-in_id, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab('Proportion out of EBS') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') 


# Prop individuals dead
plot_data = bind_rows(plot_data_0d, .id = "column_label")
plot_data = aggregate(list(alive_id = plot_data$id), list(year = plot_data$year, scenario = plot_data$scenario),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

dead4 = ggplot(plot_data, aes(x = year, y = 1-alive_id, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab('Proportion dead') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') 


# Make plot dead ind ------------------------------------------------------

png(filename = 'figures/fore_dead_ind.png', width = 190, height = 150, 
    units = 'mm', res = 500)
grid.arrange(dead1, dead2, dead3, dead4, nrow = 2)
dev.off()


# Plot 8: Index of survival ---------------------------------------------
plot_data = NULL
hatch_data = bind_rows(plot_data_1a)
psurv100_data = bind_rows(plot_data_2a)
psurv30_data = bind_rows(plot_data_3a)
colnames(hatch_data)[3] = 'hatch'
colnames(psurv100_data)[3] = 'psurv'
colnames(psurv30_data)[3] = 'psurv'

# Index 1: hatch
tmp_data = hatch_data
tmp_data$rec_index = tmp_data$hatch
temp = aggregate(list(p_index = tmp_data$rec_index), 
                         list(year = tmp_data$year, scenario = tmp_data$scenario),
                         FUN = sum)
temp$type = 'hatch'
plot_data = rbind(plot_data, temp)

# Index 2: psurv100
tmp_data = psurv100_data
tmp_data$rec_index = tmp_data$psurv 
temp = aggregate(list(p_index = tmp_data$rec_index), 
                         list(year = tmp_data$year, scenario = tmp_data$scenario),
                         FUN = sum)
temp$type = 'psurv100'
plot_data = rbind(plot_data, temp)

# Index 3: hatch * psurv100
tmp_data = left_join(hatch_data, psurv100_data, by = c('year', 'id', 'scenario'))
tmp_data$rec_index = tmp_data$hatch*tmp_data$psurv 
temp = aggregate(list(p_index = tmp_data$rec_index), 
                   list(year = tmp_data$year, scenario = tmp_data$scenario),
                   FUN = sum)
temp$type = 'hatch_psurv100'
plot_data = rbind(plot_data, temp)

# Index 4: psurv30
tmp_data = psurv30_data
tmp_data$rec_index = tmp_data$psurv 
temp = aggregate(list(p_index = tmp_data$rec_index), 
                   list(year = tmp_data$year, scenario = tmp_data$scenario),
                   FUN = sum)
temp$type = 'psurv30'
plot_data = rbind(plot_data, temp)

# Index 5: hatch * psurv30
tmp_data = left_join(hatch_data, psurv30_data, by = c('year', 'id', 'scenario'))
tmp_data$rec_index = tmp_data$hatch*tmp_data$psurv 
temp = aggregate(list(p_index = tmp_data$rec_index), 
                   list(year = tmp_data$year, scenario = tmp_data$scenario),
                   FUN = sum)
temp$type = 'hatch_psurv30'
plot_data = rbind(plot_data, temp)

# Make plot:
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$type = factor(plot_data$type, levels = c("hatch", 'psurv100', 'hatch_psurv100', 'psurv30', 'hatch_psurv30'))
plot_data$type = factor(plot_data$type, 
                        labels = c("Sigma~HS", 'Sigma~P[s~100]', 'Sigma~P[s~100]~HS', 'Sigma~P[s~30]','Sigma~P[s~30]~HS'))

recin1 = ggplot(plot_data, aes(x = year, y = p_index, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ type, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 3) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")


# Make plot recruitment index ------------------------------------------------------

png(filename = 'figures/fore_rec_index.png', width = 190, height = 110, 
    units = 'mm', res = 500)
print(recin1)
dev.off()


# Plot 9: Compare ind rec with rec estimates ------------------------------
# Keep plot_data from previous plot:
rec_estimates = read.csv('Compare_Recs_Surv.csv')
rec_estimates$p_index[rec_estimates$scenario != 'sage1_comps'] = rec_estimates$p_index[rec_estimates$scenario != 'sage1_comps'] * 1e-6
rec_estimates$type = rec_estimates$scenario
rec_estimates$type = factor(rec_estimates$type, levels = c("sage0_mod", 'sage1_mod', 'sage1_comps'))
rec_estimates$type = factor(rec_estimates$type, 
                        labels = c("Age-0~abundance~(million~ind)", "Age-1~abundance~(million~ind)", 'Age-1~proportion'))
new_plot_data = rbind(plot_data, rec_estimates)
new_plot_data = new_plot_data[new_plot_data$year <= 2019 & new_plot_data$year >= 2010, ]
new_plot_data$scenario = factor(new_plot_data$scenario, levels = c(scenarioLevels, "sage0_mod", 'sage1_mod', 'sage1_comps'))

recin2 = ggplot(new_plot_data, aes(x = year, y = p_index, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2010, to = 2018, by = 2)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ type, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 3) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# Make plot:
png(filename = 'figures/fore_rec_index_compare.png', width = 190, height = 150, 
    units = 'mm', res = 500)
print(recin2)
dev.off()


# Plot 10: Changes in diet ------------------------------------------------

#Prey type:
plot_data = bind_rows(plot_data_10a, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
diet1 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Prey preference') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_y_reverse(limits = c(6.5, 0.5), breaks = 1:6, labels = c('EupO', 'EupS', 'NCaS', 'NCaO', 'Cop', 'MZP')) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_grid(~stage)

# Prey size
plot_data = bind_rows(plot_data_10b, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$stage = as.factor(plot_data$stage)

# Plot:
diet2 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab('Prey size (mm)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(~stage, scales = 'free_y')

# Make plot:
png(filename = 'figures/fore_preyfig.png', width = 190, height = 130, 
    units = 'mm', res = 500)
grid.arrange(diet1, diet2)
dev.off()


# Plot 11: Environmental variables -------------------------------------------------

plot_data = bind_rows(plot_data_8c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$variable = factor(plot_data$variable, levels = c("temperature", 'pCO2'))
plot_data$variable2 = factor(plot_data$variable, labels = c("Temperature~(C)", 'pCO[2]~(mu*atm)'))

# Plot:
ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 1) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/fore_envvar.png', device = 'png', width = 95, height = 150, units = 'mm', dpi = 500)


# Plot 12: prey density data -------------------------------------------------------

plot_data = bind_rows(plot_data_9c, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "euphausiidsShelf", "neocalanusShelf", 
                                                           "neocalanus", "copepods", "microzoo"))
plot_data$variable2 = factor(plot_data$variable, labels = c("EupO~(mg~C/m^3)",
                                                            "EupS~(mg~C/m^3)", "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)", "MZP~(mg~C/m^3)"))

# Plot:
ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=0.1) +
  geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = 'figures/fore_preyvar.png', device = 'png', width = 190, height = 110, units = 'mm', dpi = 500)


# Plot map anomalies ----------------------------------------------------------

# Hatching success
plot_data = bind_rows(plot_data_1b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = mean)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom1_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Hatch success')
anom1_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Hatch success')

# Survival 100 dph
plot_data = bind_rows(plot_data_2b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom2_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Survival probability (100 dph)')
anom2_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Survival probability (100 dph)')

# SL
plot_data = bind_rows(plot_data_4b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom3_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Standard length (mm)')
anom3_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Standard length (mm)')

# Make plot:
png(filename = 'figures/fore_mapanom_85.png', width = 75, height = 190, units = 'mm', res = 500)
grid.arrange(anom1_85, anom2_85, anom3_85, ncol = 1)
dev.off()

png(filename = 'figures/fore_mapanom_45.png', width = 75, height = 190, units = 'mm', res = 500)
grid.arrange(anom1_45, anom2_45, anom3_45, ncol = 1)
dev.off()

# ----

# Temperature
plot_data = bind_rows(plot_data_8b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'temperature', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom4_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Temperature (C)')
anom4_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Temperature (C)')

# pCO2
plot_data = bind_rows(plot_data_8b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'pCO2', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom5_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'pCO[2]~(mu*atm)', parse = TRUE)
anom5_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'pCO[2]~(mu*atm)', parse = TRUE)

# Make plot:
png(filename = 'figures/fore_mapanom_env_85.png', width = 95, height = 160, units = 'mm', res = 500)
grid.arrange(anom4_85, anom5_85, ncol = 1)
dev.off()

png(filename = 'figures/fore_mapanom_env_45.png', width = 95, height = 160, units = 'mm', res = 500)
grid.arrange(anom4_45, anom5_45, ncol = 1)
dev.off()

# ----

# MZL:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'microzoo', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom6_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'MZP')
anom6_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'MZP')

# Cop:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'copepods', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom7_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Cop')
anom7_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Cop')

# NCaO:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'neocalanus', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom8_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'NCaO')
anom8_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'NCaO')

# NCaS:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'neocalanusShelf', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom9_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'NCaS')
anom9_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'NCaS')

# EupS:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'euphausiidsShelf', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom10_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'EupS')
anom10_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'EupS')

# EupO:
plot_data = bind_rows(plot_data_9b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'euphausiids', ]
plot_data = aggregate(list(var = plot_data$value2), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = median)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

anom11_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'EupO')
anom11_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'EupO')

# Make plot:
png(filename = 'figures/fore_mapanom_prey_85.png', width = 190, height = 120, units = 'mm', res = 500)
grid.arrange(anom6_85, anom7_85, anom8_85, anom9_85, anom10_85, anom11_85, ncol = 3)
dev.off()

png(filename = 'figures/fore_mapanom_prey_45.png', width = 190, height = 120, units = 'mm', res = 500)
grid.arrange(anom6_45, anom7_45, anom8_45, anom9_45, anom10_45, anom11_45, ncol = 3)
dev.off()



# Map ind dead  -----------------------------------------------------------
n_years = 91

# FDL:
plot_data = bind_rows(plot_data_0e, .id = "column_label")
plot_data = plot_data %>%
              group_by(scenario) %>%
              count(id)
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id = plot_data$id, scenario = plot_data$scenario), mean)
plot_data$var = 1 - plot_data$n/n_years
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = 1

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

mapdead1_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'Reached PNR')
mapdead1_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'Reached PNR')

# DW < 75%:
plot_data = bind_rows(plot_data_0f, .id = "column_label")
plot_data = plot_data %>%
  group_by(scenario) %>%
  count(id)
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id = plot_data$id, scenario = plot_data$scenario), mean)
plot_data$var = 1 - plot_data$n/n_years
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = 1

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

mapdead2_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'Starved')
mapdead2_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'Starved')

# out EBS:
plot_data = bind_rows(plot_data_0g, .id = "column_label")
plot_data = plot_data %>%
  group_by(scenario) %>%
  count(id)
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id = plot_data$id, scenario = plot_data$scenario), mean)
plot_data$var = 1 - plot_data$n/n_years
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = 1

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

mapdead3_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'Out of EBS')
mapdead3_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'Out of EBS')

# all dead:
plot_data = bind_rows(plot_data_0h, .id = "column_label")
plot_data = plot_data %>%
  group_by(scenario) %>%
  count(id)
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id = plot_data$id, scenario = plot_data$scenario), mean)
plot_data$var = 1 - plot_data$n/n_years
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = 1

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

mapdead4_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'All dead')
mapdead4_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'All dead')

# Make plot:
png(filename = 'figures/fore_mapdead_85.png', width = 190, height = 150, units = 'mm', res = 500)
grid.arrange(mapdead1_85, mapdead2_85, mapdead3_85, mapdead4_85, ncol = 2)
dev.off()

png(filename = 'figures/fore_mapdead_45.png', width = 190, height = 150, units = 'mm', res = 500)
grid.arrange(mapdead1_45, mapdead2_45, mapdead3_45, mapdead4_45, ncol = 2)
dev.off()


# Plot spaiotemporal trends -----------------------------------------------

# Hatch success:
plot_data = bind_rows(plot_data_1a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend1_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Hatching success')
trend1_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Hatching success')

# Survival (100 dph):
plot_data = bind_rows(plot_data_2a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend2_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Survival probability (100 dph)')
trend2_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Survival probability (100 dph)')

# SL
plot_data = bind_rows(plot_data_4a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
freqTab = plot_data %>%
            count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend3_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Standard length (mm)')
trend3_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Standard length (mm)')

# Make plot:
png(filename = 'figures/fore_mapslope_85.png', width = 75, height = 190, units = 'mm', res = 500)
grid.arrange(trend1_85, trend2_85, trend3_85, ncol = 1)
dev.off()

png(filename = 'figures/fore_mapslope_45.png', width = 75, height = 190, units = 'mm', res = 500)
grid.arrange(trend1_45, trend2_45, trend3_45, ncol = 1)
dev.off()

# --------------
# Temperature
plot_data = bind_rows(plot_data_8a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'temperature', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend4_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Temperature (C)')
trend4_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Temperature (C)')


# pCO2
plot_data = bind_rows(plot_data_8a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'pCO2', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend5_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'pCO[2]~(mu*atm)', parse = TRUE) 
trend5_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'pCO[2]~(mu*atm)', parse = TRUE)

# Make plot:
png(filename = 'figures/fore_mapslope_env_85.png', width = 95, height = 160, units = 'mm', res = 500)
grid.arrange(trend4_85, trend5_85, ncol = 1)
dev.off()

png(filename = 'figures/fore_mapslope_env_45.png', width = 95, height = 160, units = 'mm', res = 500)
grid.arrange(trend4_45, trend5_45, ncol = 1)
dev.off()

# -------------------------------
# MZL
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'microzoo', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend6_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'MZP')
trend6_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'MZP')

# Cop
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'copepods', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend7_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'Cop')
trend7_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'Cop')

# NCaO:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'neocalanus', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend8_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'NCaO')
trend8_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'NCaO')

# NCaO:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'neocalanus', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend8_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'NCaO')
trend8_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'NCaO')


# NCaS:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'neocalanusShelf', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend9_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'NCaS')
trend9_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'NCaS')

# EupS:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'euphausiidsShelf', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend10_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'EupS')
trend10_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'EupS')

# EupO:
plot_data = bind_rows(plot_data_9a, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = plot_data[plot_data$variable == 'euphausiids', ]
freqTab = plot_data %>%
  count(id) # select ID with enough obs for SL and DW
sel_id = freqTab$id[which(freqTab$n > 100)]
plot_data = plot_data[plot_data$id %in% sel_id, ]

mods = dlply(plot_data, c("id", "scenario"), function(df) {
  mod1 = lm(value ~ year, data = df)
  slopeMod = coef(mod1)[2]
  return(slopeMod)
})
new_data = attr(mods, 'split_labels')
new_data$var = unlist(mods)
plot_data = new_data
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

trend11_45 = plot_map_var(plot_data = plot_data45, legTitle = '', mainTitle = 'EupO')
trend11_85 = plot_map_var(plot_data = plot_data85, legTitle = '', mainTitle = 'EupO')

# Make plot:
png(filename = 'figures/fore_mapslope_prey_85.png', width = 190, height = 120, units = 'mm', res = 500)
grid.arrange(trend6_85, trend7_85, trend8_85, trend9_85, trend10_85, trend11_85, ncol = 3)
dev.off()

png(filename = 'figures/fore_mapslope_prey_45.png', width = 190, height = 120, units = 'mm', res = 500)
grid.arrange(trend6_45, trend7_45, trend8_45, trend9_45, trend10_45, trend11_45, ncol = 3)
dev.off()


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Information to map
shift_value_1 = 0
shift_value_2 = 360

map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
  dplyr::filter(region != "Antarctica")

country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group),
                                fill = "gainsboro",
                                color = "gainsboro",
                                size = 0.15)

# -------------------------------------------------------------------------
# Plot trajectories:

# List to save plots:
# plotList = list()
# indList = 1
# 
# for(i in seq_along(cores)) {
#   
#   core_name = cores[i]
#   files_core = list.files(path = file.path(main_folder, core_name))
#   fcore = grep(pattern = 'Results_files', x = list.files(path = file.path(main_folder, core_name)))
#   
#   for(j in seq_along(fcore)) {
#    
#     tmpData = read_data_in(eggInclude = FALSE, 
#                            path = file.path(main_folder, core_name, files_core[fcore[j]]))
#     tmpData$horizPos1 = ifelse(test = tmpData$horizPos1 > 0, yes = tmpData$horizPos1 - 360, 
#                                no = tmpData$horizPos1)
#     tmpData$horizPos1 = tmpData$horizPos1 + 360
#     tmpData$relDay = lubridate::day(tmpData$startTime)
#     
#     # Find initial and final points
#     init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
# 
#     plotList[[indList]] = plot_trajectory(tmpData)
#     
#     indList = indList+1
#      
#   } 
#     
# }
# 
# # Plot trajectories: 
# 
# plotList2 = plotList[c(1,5,8,2,6,9,3,7,10,4)] # reorder list by year
# 
# png(filename = 'figures/hind_trajectories.png', width = 190, height = 200, 
#     units = 'mm', res = 500)
# do.call("grid.arrange", c(plotList2, ncol = 3))
# dev.off()



# Plot spatial indicators -------------------------------------------------
# Distance:

dist_data = bind_rows(plot_data_11a, .id = "column_label")
dist_data$q50 = dist_data$q50*1.852 # from nm to km
dist_data$q5 = dist_data$q5*1.852 # from nm to km
dist_data$q95 = dist_data$q95*1.852 # from nm to km
dist_data$q25 = dist_data$q25*1.852 # from nm to km
dist_data$q75 = dist_data$q75*1.852 # from nm to km
plot_data = dist_data
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

sp1 = ggplot(plot_data, aes(x = year)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = scenario), alpha=alphaLevel) +
  #geom_ribbon(aes(ymin = q25, ymax = q75, fill = scenario), alpha=0.2) +
  geom_line(aes(y = q50, colour = scenario)) +
  theme_bw() +
  xlab('') +
  ylab('Distance (km)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  coord_cartesian(ylim = c(0, 1000)) +  
  scale_color_manual(values = mainCols) +
  scale_fill_manual(values = mainCols) +
  theme(legend.position = 'none')

# Inertia:
plot_data = bind_rows(plot_data_13, .id = "column_label")
plot_data$scenario = factor(plot_data$scenario, levels = scenarioLevels)

# Plot:
sp2 = ggplot(plot_data, aes(x = year, y = I_end, color = scenario)) + 
  geom_line() +
  theme_bw() +
  xlab('') +
  ylab('Inertia') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = 2015, to = 2095, by = 20)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none') 

png(filename = 'figures/fore_spatialind.png', width = 95, height = 150, 
    units = 'mm', res = 500)
grid.arrange(sp1, sp2, nrow = 2)
dev.off()


# Plot: Distance traveled map
plot_data = bind_rows(plot_data_11b, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(var = plot_data$dist), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = mean, na.rm = TRUE)
plot_data$var = sqrt(plot_data$var)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

dist_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'Distance (km)', breaks = c(5, 10,15,20), labels = c(25,100, 225, 400))
dist_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'Distance (km)', breaks = c(5, 10,15,20), labels = c(25,100, 225, 400))

# Plot: Direction map
plot_data = bind_rows(plot_data_12, .id = "column_label")
plot_data$id = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(var = plot_data$dist), list(id = plot_data$id, scenario = plot_data$scenario),
                      FUN = mean, na.rm = TRUE)
plot_data$lon = baseLocs$horizPos1[match(plot_data$id, baseLocs$id)]
plot_data$lat = baseLocs$horizPos2[match(plot_data$id, baseLocs$id)]

plot_data = left_join(baseLocs3, plot_data, by = c('id', 'lon', 'lat', 'scenario'))
plot_data$var[which(is.na(plot_data$var))] = NA

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

direc_45 = plot_map_var2(plot_data = plot_data45, legTitle = '', mainTitle = 'Bearing', breaks = c(0,90,180,270, 360), limits = c(0,360), labels = c('N', 'E', 'S', 'W', 'N'))
direc_85 = plot_map_var2(plot_data = plot_data85, legTitle = '', mainTitle = 'Bearing', breaks = c(0,90,180,270, 360), limits = c(0,360), labels = c('N', 'E', 'S', 'W', 'N'))

# Make plot:
png(filename = 'figures/fore_dist_direc_85.png', width = 95, height = 150, 
    units = 'mm', res = 500)
grid.arrange(dist_85, direc_85, nrow = 2)
dev.off()

png(filename = 'figures/fore_dist_direc_45.png', width = 95, height = 150, 
    units = 'mm', res = 500)
grid.arrange(dist_45, direc_45, nrow = 2)
dev.off()



# Plot density of final locations -----------------------------------------
plot_data = bind_rows(plot_data_14, .id = "column_label")
plot_data$period = cut_interval(x = plot_data$year, n = 3, breaks = c(2010, 2040, 2070, 2100), 
                                  labels = c('2010-2040', '2041-2070', '2071-2100'))
plot_data$period = factor(plot_data$period)

plot_data45 = plot_data[plot_data$scenario == scenarioLevels[2], ]
plot_data85 = plot_data[plot_data$scenario == scenarioLevels[1], ]

density_45 = plot_map_2d_density(plot_data = plot_data45)
density_85 = plot_map_2d_density(plot_data = plot_data85)

# Make plot:
png(filename = 'figures/fore_2d_density_85.png', width = 75, height = 190, units = 'mm', res = 500)
print(density_85)
dev.off()

png(filename = 'figures/fore_2d_density_45.png', width = 75, height = 190, units = 'mm', res = 500)
print(density_45)
dev.off()


# -------------------------------------------------------------------------
# Plot initial locations:
plot_initial_locations(initData = base_locs)
ggsave(filename = 'figures/initLocations.png', device = 'png', width = 95, height = 80, units = 'mm', dpi = 500)

